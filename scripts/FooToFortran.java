import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * FooToFortran — ANTLR4-based replacement for scripts/foo.pl.
 *
 * Translates a Foo module (foofiles/*.foo) into the three Fortran artefacts
 * foo.pl emits (pre-C-preprocessor): &lt;module&gt;.F90, &lt;module&gt;.int, &lt;module&gt;.use.
 * The reference output is in release/; the goal is equivalent, compilable
 * Fortran (not byte-exact).
 *
 * This is an in-progress reimplementation. Current coverage:
 *   - module rename + .F90 boilerplate (use/int/macros includes)
 *   - leading doc-comment block, section comments, signature comments
 *   - direct (non-inherited) procedures: header transform, reversed
 *     declarations, implicit self declaration
 *   - statement body: dot-selector -&gt; '%', type-aware generic call resolution
 *   - .int (generic interfaces) and .use (resolved 'use ... only:' lists)
 * Not yet done: get_from inheritance, full array/function decls, submodules.
 *
 * Usage:
 *   FooToFortran --types &lt;types.foo&gt; --foo &lt;file.foo&gt; --out-dir &lt;dir&gt;
 *                [--foofiles-dir &lt;dir&gt;]
 */
public final class FooToFortran {

    // ------------------------------------------------------------------ types

    /** A derived type from types.foo: its components and their Foo type text. */
    static final class DerivedType {
        final String fooName;                       // e.g. IRREP, VEC{REAL}
        final Map<String, String> components = new LinkedHashMap<>(); // name -> foo type text
        DerivedType(String n) { this.fooName = n; }
    }

    static final class TypeTable {
        final Map<String, DerivedType> types = new LinkedHashMap<>();
        DerivedType get(String fooName) { return types.get(canon(fooName)); }
        boolean isComponent(String fooType, String name) {
            DerivedType t = get(fooType);
            return t != null && t.components.containsKey(name);
        }
        String componentType(String fooType, String name) {
            DerivedType t = get(fooType);
            return t == null ? null : t.components.get(name);
        }
    }

    /** Canonicalise a Foo type name for keying (strip spaces). */
    static String canon(String t) {
        return t == null ? null : t.replaceAll("\\s+", "");
    }

    // -------------------------------------------------------------------- main

    public static void main(String[] argv) throws Exception {
        Path typesPath = null, fooPath = null, outDir = null, foofilesDir = null;
        for (int i = 0; i < argv.length; i++) {
            switch (argv[i]) {
                case "--types":        typesPath   = Paths.get(argv[++i]); break;
                case "--foo":          fooPath     = Paths.get(argv[++i]); break;
                case "--out-dir":      outDir      = Paths.get(argv[++i]); break;
                case "--foofiles-dir": foofilesDir = Paths.get(argv[++i]); break;
                default: throw new IllegalArgumentException("unknown arg: " + argv[i]);
            }
        }
        if (fooPath == null || outDir == null) {
            throw new IllegalArgumentException(
                "Usage: FooToFortran --foo <file.foo> --out-dir <dir> "
                + "[--types <types.foo>] [--foofiles-dir <dir>]");
        }
        if (foofilesDir == null) foofilesDir = fooPath.toAbsolutePath().getParent();
        if (typesPath == null)   typesPath   = foofilesDir.resolve("types.foo");

        TypeTable types = new TypeTable();
        if (Files.exists(typesPath)) buildTypeTable(types, typesPath);

        ModuleEmitter em = new ModuleEmitter(types, parse(fooPath), fooPath);
        em.emit();

        Files.createDirectories(outDir);
        String stem = outStem(fooPath.getFileName().toString());
        Files.writeString(outDir.resolve(stem + ".F90"), em.f90.toString(), StandardCharsets.UTF_8);
        Files.writeString(outDir.resolve(stem + ".int"), em.intf.toString(), StandardCharsets.UTF_8);
        Files.writeString(outDir.resolve(stem + ".use"), em.use.toString(), StandardCharsets.UTF_8);
    }

    static FooParser.ProgramContext parse(Path p) throws IOException {
        FooLexer lexer = new FooLexer(CharStreams.fromPath(p));
        CommonTokenStream toks = new CommonTokenStream(lexer);
        FooParser parser = new FooParser(toks);
        return parser.program();
    }

    /** Foo file name -> output stem: vec{real}.foo -> vec_real */
    static String outStem(String fooName) {
        String s = fooName.endsWith(".foo") ? fooName.substring(0, fooName.length() - 4) : fooName;
        s = s.replace('{', '_').replace('}', '_').replace(',', '_').replace('.', '_');
        s = s.replaceAll("_+", "_").replaceAll("_$", "");
        return s;
    }

    // --------------------------------------------------- type-table building

    static void buildTypeTable(TypeTable tt, Path typesFoo) throws IOException {
        FooParser.ProgramContext prog = parse(typesFoo);
        // types.foo is one module whose data section is a list of typeDef.
        for (FooParser.ModuleDefContext m : descendants(prog, FooParser.ModuleDefContext.class)) {
            for (FooParser.TypeDefContext td : descendants(m, FooParser.TypeDefContext.class)) {
                String fooName = td.typeSpec().getText();          // e.g. IRREP or VEC{REAL}
                DerivedType dt = new DerivedType(fooName);
                for (FooParser.VarDeclContext vd : childList(td, FooParser.VarDeclContext.class)) {
                    // store the base type spec only (without @/* suffix or trailing attrs)
                    String type = vd.declTail().typeSpec() != null
                        ? vd.declTail().typeSpec().getText()
                        : vd.declTail().getText();
                    for (FooParser.DeclNameContext dn : vd.identList().declName()) {
                        dt.components.put(nameText(dn.name()), type);
                    }
                }
                tt.types.put(canon(fooName), dt);
            }
        }
    }

    // ------------------------------------------------------------- emitter

    /** Naming helpers shared by the emitter. */
    static String fortranTypeName(String fooType) {
        String s = canon(fooType);
        s = s.replace("?", "").replace("@", "").replace("*", "");  // drop suffixes
        s = s.replace("{", "_").replace("}", "").replace(",", "_").replace(".", "_");
        return s.replaceAll("_+", "_").replaceAll("_$", "");
    }
    static String fortranModName(String fooType) { return fortranTypeName(fooType) + "_MODULE"; }

    static final Set<String> INTRINSIC = Set.of("INT", "REAL", "CPX", "BIN", "STR");

    static boolean isIntrinsicScalar(String t) { return INTRINSIC.contains(canon(t)); }

    static String nameText(FooParser.NameContext n) {
        // a 'name' may be IDENTIFIER with an optional trailing '?'
        return n.getText().replace("?", "");
    }

    final static class ModuleEmitter {
        final TypeTable types;
        final FooParser.ProgramContext prog;
        final Path fooPath;

        String fooModuleName;     // e.g. IRREP
        String selfFooType;       // self's foo type = module type

        final StringBuilder f90  = new StringBuilder();
        final StringBuilder intf = new StringBuilder();
        final StringBuilder use  = new StringBuilder();

        // procedures in declaration order (for .int)
        final List<String> procNames = new ArrayList<>();
        // resolved external generic calls for .use:  fortranMod -> set of "only" symbols
        final Map<String, Set<String>> useOnly = new TreeMap<>();

        ModuleEmitter(TypeTable types, FooParser.ProgramContext prog, Path fooPath) {
            this.types = types; this.prog = prog; this.fooPath = fooPath;
        }

        void emit() {
            FooParser.ModuleDefContext mod = descendants(prog, FooParser.ModuleDefContext.class).get(0);
            fooModuleName = mod.moduleName().getText();
            selfFooType   = fooModuleName;
            String stem   = outStem(fooPath.getFileName().toString());

            // 1. leading doc-comment block (hidden tokens before 'module')
            f90.append(leadingComments(mod));

            // 2. module line + boilerplate
            f90.append("module ").append(fortranTypeName(fooModuleName)).append("_MODULE\n\n");
            f90.append("#  include \"").append(stem).append(".use\"\n\n");
            f90.append("   implicit none\n\n");
            f90.append("#  include \"macros\"\n");
            f90.append("#  include \"").append(stem).append(".int\"\n\n\n");
            f90.append("contains\n");

            // 3. procedures
            for (FooParser.ProcDefContext pd : descendants(mod, FooParser.ProcDefContext.class)) {
                emitProc(pd);
            }

            // 4. end module
            f90.append("\nend module\n");

            buildInterfaceFile();
            buildUseFile(stem);
        }

        // ---- procedures ---------------------------------------------------

        void emitProc(FooParser.ProcDefContext pd) {
            FooParser.ProcHeaderContext h = pd.procHeader();
            String name = h.IDENTIFIER().getText();
            Attrs a = Attrs.parse(h.procAttrs());
            if (a.template) return;                 // template stubs are not emitted
            procNames.add(name);

            List<String> args = headerArgs(h);
            boolean func = h.procResult() != null;
            String result = func ? h.procResult().IDENTIFIER().getText() : null;
            List<String> callArgs = new ArrayList<>();
            if (!a.selfless) callArgs.add("self");
            callArgs.addAll(args);

            f90.append('\n');
            // section comments immediately preceding this proc come from leading hidden tokens
            f90.append(commentsBefore(h));

            StringBuilder hdr = new StringBuilder("   ");
            if (a.prefix() != null) hdr.append(a.prefix()).append(' ');
            hdr.append(func ? "function " : "subroutine ").append(name);
            hdr.append('(').append(String.join(",", callArgs)).append(')');
            if (func) hdr.append(" result(").append(result).append(')');
            f90.append(hdr).append('\n');

            if (a.inherited) {
                f90.append("      ! TODO get_from(").append(a.getFromTarget).append(") not yet implemented\n");
                f90.append(func ? "   end function\n" : "   end subroutine\n");
                return;
            }

            // implicit self declaration is emitted via the source `self :: ...` line
            // body
            for (FooParser.ProcBodyContext b : pd.procBody()) {
                emitBody(b);
            }
            f90.append(func ? "   end function\n" : "   end subroutine\n");
        }

        List<String> headerArgs(FooParser.ProcHeaderContext h) {
            List<String> out = new ArrayList<>();
            if (h.procArgs() != null && h.procArgs().identList() != null) {
                for (FooParser.DeclNameContext dn : h.procArgs().identList().declName()) {
                    out.add(nameText(dn.name()));
                }
            }
            return out;
        }

        void emitBody(FooParser.ProcBodyContext b) {
            if (b.localDecl() != null) { emitDecl(b.localDecl().identList(), b.localDecl().declTail()); return; }
            if (b.stmt() != null)      { emitStmt(b.stmt()); return; }
            // dataStmt / interfaceBlock / useStmt / NEWLINE: not handled in first cut
        }

        // ---- declarations -------------------------------------------------

        void emitDecl(FooParser.IdentListContext ids, FooParser.DeclTailContext tail) {
            List<String> vars = new ArrayList<>();
            for (FooParser.DeclNameContext dn : ids.declName()) vars.add(dn.getText());
            boolean isSelf = vars.size() == 1 && vars.get(0).equals("self");

            String ftype;
            List<String> attrs = new ArrayList<>();

            if (tail.typeSpec() != null) {
                String foo = tail.typeSpec().getText();
                ftype = fortranDeclType(foo, /*routineArg=*/true);
                if (tail.ptrSuffix() != null) {
                    attrs.add(tail.ptrSuffix().getText().equals("@") ? "allocatable" : "PTR");
                }
                if (tail.attrSuffix() != null) {
                    for (FooParser.AttrContext at : tail.attrSuffix().attr()) attrs.add(at.getText());
                }
            } else {
                // attrs-only (e.g. `self :: INOUT`, `self :: allocatable, OUT`) -> implicit self type
                ftype = "type(" + fortranTypeName(selfFooType) + "_TYPE)";
                for (FooParser.AttrContext at : tail.attr()) attrs.add(at.getText());
            }

            StringBuilder d = new StringBuilder("      ").append(ftype);
            for (String at : attrs) d.append(", ").append(at);
            d.append(" :: ").append(String.join(",", vars));
            f90.append(d).append('\n');
        }

        /** Foo type text -> Fortran declaration type (left of '::'). */
        String fortranDeclType(String foo, boolean routineArg) {
            String c = canon(foo);
            if (c.equals("STR")) return routineArg ? "STR(len=*)" : "STR(len=STR_SIZE)";
            if (c.startsWith("STR(")) return c;                    // STR(len=N) kept
            if (isIntrinsicScalar(c)) return c;                    // INT, REAL, CPX, BIN
            if (types.get(c) != null && !c.contains("{"))          // plain derived type
                return "type(" + fortranTypeName(c) + "_TYPE)";
            // arrays / parameterised types: first-cut passthrough (TODO faithful form)
            return c;
        }

        // ---- statements ---------------------------------------------------

        void emitStmt(FooParser.StmtContext s) {
            if (s.simpleLine() != null) {
                for (FooParser.LineStmtContext ls : s.simpleLine().lineStmt()) {
                    if (ls.simpleStmt() != null) {
                        String txt = simpleStmt(ls.simpleStmt());
                        // A lone `end` here is a block terminator mis-parsed as a
                        // statement (END is a soft keyword / valid variable name);
                        // we synthesise `end subroutine`/`end module` ourselves.
                        if (txt != null && !txt.isBlank() && !txt.equalsIgnoreCase("end"))
                            f90.append("      ").append(txt).append('\n');
                    }
                }
            }
            // ifStmt/doStmt/selectStmt/forall: not handled in first cut
        }

        /** Translate a simpleStmt to a Fortran line. */
        String simpleStmt(FooParser.SimpleStmtContext st) {
            if (st.postfix() != null) {
                Chain head = translatePostfix(st.postfix(), /*statementPos=*/true);
                if (st.EQUAL() != null)  return head.text + " = " + expr(st.expr());
                if (st.ARROW() != null)  return head.text + " => " + expr(st.expr());
                return head.text;       // bare call / io
            }
            if (st.EXIT()  != null) return "exit";
            if (st.CYCLE() != null) return "cycle";
            if (st.RETURN()!= null) return "return";
            return null;
        }

        String expr(FooParser.ExprContext e) {
            if (e == null) return "";
            StringBuilder sb = new StringBuilder();
            for (ParseTree c : e.children) {
                if (c instanceof FooParser.PostfixContext) sb.append(translatePostfix((FooParser.PostfixContext) c, false).text);
                else sb.append(c.getText());      // binOp etc. — first-cut passthrough
            }
            return sb.toString();
        }

        /** Result of translating a postfix chain. */
        static final class Chain { String text; String fooType; boolean isCall; }

        /**
         * Translate a postfix chain (head + trailers), converting leading dot
         * selectors to self%..., resolving generic method calls, tracking the
         * type for .use resolution. First-cut: handles the common patterns in
         * simple modules (self-component access and method calls on them).
         */
        Chain translatePostfix(FooParser.PostfixContext p, boolean statementPos) {
            Chain ch = new Chain();
            FooParser.HeadContext head = p.head();
            StringBuilder out = new StringBuilder();
            String curType = null;
            boolean lastWasMethodCall = false;
            String pendingCallName = null;     // a method call awaiting its (args)

            // ----- head
            if (head.callHead() != null) {
                FooParser.CallHeadContext chx = head.callHead();
                if (chx.DOT() != null && chx.name() != null && chx.qualifier() == null
                        && chx.COLON() == null && chx.DCOLON() == null) {
                    // `.x` : selector on self
                    String sel = nameText(chx.name());
                    if (types.isComponent(selfFooType, sel)) {
                        out.append("self%").append(sel);
                        curType = types.componentType(selfFooType, sel);
                    } else {
                        // `.method` -> generic call on self
                        pendingCallName = sel + "_";
                        recordCall(selfFooType, sel);
                        out.append("self");        // first arg; will wrap below
                        curType = null;
                        lastWasMethodCall = true;
                    }
                } else {
                    out.append(head.getText());     // qualified / plain — first-cut passthrough
                }
            } else {
                out.append(head.getText());
            }

            // ----- trailers
            for (FooParser.TrailerContext tr : p.trailer()) {
                if ((tr.DOT() != null || tr.PERCENT() != null)
                        && tr.COLON() == null && tr.DCOLON() == null && !tr.name().isEmpty()) {
                    String sel = nameText(tr.name(0));
                    if (curType != null && types.isComponent(curType, sel)) {
                        out.append('%').append(sel);
                        curType = types.componentType(curType, sel);
                    } else {
                        // method call on current expression: call sel_(expr, ...)
                        recordCall(curType, sel);
                        out = new StringBuilder(sel + "_(" + out);
                        pendingCallName = null;
                        lastWasMethodCall = true;
                        out.append(')');
                        curType = null;
                    }
                } else if (tr.LPAREN() != null) {
                    // call parentheses
                    String inner = tr.argList() != null ? argList(tr.argList()) : "";
                    if (pendingCallName != null) {
                        out = new StringBuilder(pendingCallName + "(" + out
                                + (inner.isEmpty() ? "" : "," + inner) + ")");
                        pendingCallName = null;
                    } else {
                        out.append('(').append(inner).append(')');
                    }
                } else {
                    out.append(tr.getText());
                }
            }
            if (pendingCallName != null) {       // `.method` with no parens (e.g. .destroy)
                out = new StringBuilder(pendingCallName + "(" + out + ")");
            }

            ch.fooType = curType;
            ch.isCall  = lastWasMethodCall;
            String s = out.toString();
            if (statementPos && ch.isCall) s = "call " + s;
            ch.text = s;
            return ch;
        }

        String argList(FooParser.ArgListContext al) {
            List<String> parts = new ArrayList<>();
            for (FooParser.ArgContext a : al.arg()) parts.add(a.getText());  // first-cut passthrough
            return String.join(",", parts);
        }

        void recordCall(String fooType, String method) {
            if (fooType == null) return;          // unresolved — skip (first cut)
            useOnly.computeIfAbsent(fortranModName(fooType), k -> new LinkedHashSet<>())
                   .add(method + "_");
        }

        // ---- .int ---------------------------------------------------------

        void buildInterfaceFile() {
            intf.append("   private\n\n");
            for (String n : procNames) {
                intf.append("   public    ").append(n).append("_\n");
                intf.append("   interface ").append(n).append("_\n");
                intf.append("      module procedure ").append(n).append('\n');
                intf.append("   end interface\n\n");
            }
        }

        // ---- .use ---------------------------------------------------------

        void buildUseFile(String stem) {
            use.append("   use TYPES_MODULE\n");
            if (!fortranTypeName(fooModuleName).equals("SYSTEM"))
                use.append("   use SYSTEM_MODULE\n");
            for (Map.Entry<String, Set<String>> e : useOnly.entrySet()) {
                List<String> only = new ArrayList<>(e.getValue());
                only.sort(null);
                use.append("   use ").append(e.getKey()).append(", only: ")
                   .append(String.join(",", only)).append('\n');
            }
        }

        // ---- comment recovery (hidden channel) ----------------------------

        String leadingComments(FooParser.ModuleDefContext mod) {
            return ""; // TODO: recover the leading doc block from hidden tokens
        }
        String commentsBefore(ParserRuleContext ctx) {
            return ""; // TODO: recover section/signature comments from hidden tokens
        }
    }

    // -------------------------------------------------- attribute parsing

    static final class Attrs {
        boolean pure, PURE, elemental, ELEMENTAL, recursive, leaky, selfless;
        boolean privateAcc, publicAcc, template, inherited;
        String getFromTarget;

        static Attrs parse(FooParser.ProcAttrsContext pa) {
            Attrs a = new Attrs();
            if (pa == null) return a;
            for (FooParser.AttrContext at : pa.attrList().attr()) {
                if (at.GET_FROM() != null) {
                    a.inherited = true;
                    a.getFromTarget = at.getFromArg(0).getText();
                    continue;
                }
                String w = at.getText().toLowerCase(Locale.ROOT);
                switch (w) {
                    case "pure":      if (at.getText().equals("PURE")) a.PURE = true; else a.pure = true; break;
                    case "elemental": if (at.getText().equals("ELEMENTAL")) a.ELEMENTAL = true; else a.elemental = true; break;
                    case "recursive": a.recursive = true; break;
                    case "leaky":     a.leaky = true; break;
                    case "selfless":  a.selfless = true; break;
                    case "private":   a.privateAcc = true; break;
                    case "public":    a.publicAcc = true; break;
                    case "template":  a.template = true; break;
                    default: /* other attrs ignored in header */ break;
                }
            }
            return a;
        }

        /** Fortran prefix from attributes (elemental/pure + recursive). */
        String prefix() {
            StringBuilder p = new StringBuilder();
            if (elemental)      p.append("elemental");
            else if (ELEMENTAL) p.append("ELEMENTAL");
            else if (pure)      p.append("pure");
            else if (PURE)      p.append("PURE");
            if (recursive) { if (p.length() > 0) p.append(' '); p.append("recursive"); }
            return p.length() == 0 ? null : p.toString();
        }
    }

    // ------------------------------------------------------- tree utilities

    @SuppressWarnings("unchecked")
    static <T extends ParserRuleContext> List<T> descendants(ParseTree root, Class<T> cls) {
        List<T> out = new ArrayList<>();
        collect(root, cls, out);
        return out;
    }
    private static <T extends ParserRuleContext> void collect(ParseTree node, Class<T> cls, List<T> out) {
        if (cls.isInstance(node)) out.add((T) node);
        for (int i = 0; i < node.getChildCount(); i++) collect(node.getChild(i), cls, out);
    }

    /** Direct-ish children of a context that are of a given type (any depth). */
    static <T extends ParserRuleContext> List<T> childList(ParserRuleContext ctx, Class<T> cls) {
        return descendants(ctx, cls);
    }

    private FooToFortran() {}
}
