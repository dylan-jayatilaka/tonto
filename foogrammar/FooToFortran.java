import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
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

/**
 * FooToFortran — ANTLR4-based replacement for scripts/foo.pl.
 *
 * Translates a Foo module (foofiles/*.foo) into the three pre-CPP Fortran
 * artefacts foo.pl emits: &lt;stem&gt;.F90, &lt;stem&gt;.int, &lt;stem&gt;.use. Reference
 * output is release/; the goal is equivalent, compilable Fortran (not byte
 * exact).
 *
 * Coverage: module rename + boilerplate; doc/section/signature comments and
 * preprocessor (#...) lines (recovered from the hidden channel); direct
 * procedures (header transform, reversed declarations, implicit self decl,
 * type-aware dot-&gt;% and generic-call resolution); get_from inheritance
 * (parent body spliced + type-substituted, ENSURE messages prefixed); .int
 * and .use generation. Block control flow / arrays / submodules: partial.
 *
 * Usage:
 *   FooToFortran --foo &lt;file.foo&gt; --out-dir &lt;dir&gt;
 *                [--types &lt;types.foo&gt;] [--foofiles-dir &lt;dir&gt;]
 */
public final class FooToFortran {

    // ---------------------------------------------------------------- parsing

    /** A parsed Foo file: tree + token stream (for hidden-channel recovery). */
    static final class Parsed {
        final FooParser.ProgramContext tree;
        final CommonTokenStream toks;
        Parsed(FooParser.ProgramContext t, CommonTokenStream k) { tree = t; toks = k; }
    }

    static Parsed parseFile(Path p) throws IOException {
        FooLexer lexer = new FooLexer(CharStreams.fromPath(p));
        CommonTokenStream toks = new CommonTokenStream(lexer);
        FooParser parser = new FooParser(toks);
        return new Parsed(parser.program(), toks);
    }

    // -------------------------------------------------------------- type table

    static final class DerivedType {
        final String fooName;
        final Map<String, String> components = new LinkedHashMap<>(); // name -> base foo type
        DerivedType(String n) { fooName = n; }
    }

    static final class TypeTable {
        final Map<String, DerivedType> types = new LinkedHashMap<>();
        DerivedType get(String t) { return types.get(canon(t)); }
        boolean isComponent(String t, String n) {
            DerivedType d = get(t); return d != null && d.components.containsKey(n);
        }
        String componentType(String t, String n) {
            DerivedType d = get(t); return d == null ? null : d.components.get(n);
        }
    }

    static String canon(String t) { return t == null ? null : t.replaceAll("\\s+", ""); }

    static void buildTypeTable(TypeTable tt, Path typesFoo) throws IOException {
        Parsed pr = parseFile(typesFoo);
        for (FooParser.TypeDefContext td : descendants(pr.tree, FooParser.TypeDefContext.class)) {
            String fooName = td.typeSpec().getText();
            DerivedType dt = new DerivedType(fooName);
            for (FooParser.VarDeclContext vd : descendants(td, FooParser.VarDeclContext.class)) {
                String type = vd.declTail().typeSpec() != null
                    ? vd.declTail().typeSpec().getText() : vd.declTail().getText();
                for (FooParser.DeclNameContext dn : vd.identList().declName())
                    dt.components.put(nameText(dn.name()), type);
            }
            tt.types.put(canon(fooName), dt);
        }
    }

    // ---------------------------------------------------------------- naming

    static String fortranTypeName(String fooType) {
        String s = canon(fooType).replace("?", "").replace("@", "").replace("*", "");
        s = stripDimensionSpec(s);   // STR(len=len(self)) -> STR ; MAT{REAL}(3,4) -> MAT{REAL}
        s = s.replace("{", "_").replace("}", "").replace(",", "_").replace(".", "_");
        return s.replaceAll("_+", "_").replaceAll("_$", "");
    }

    /** Drop a trailing top-level (...) dimension/kind/len spec from a type — it is
     *  not part of the type's module/type identity (STR(len=..) and MAT{REAL}(3,4)
     *  live in STR_MODULE / MAT_REAL_MODULE). A '(' inside {...} (an element-type
     *  param, e.g. VEC{STR(len=STR_SIZE)}) is kept. */
    static String stripDimensionSpec(String s) {
        int depth = 0;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '{') depth++;
            else if (c == '}') depth--;
            else if (c == '(' && depth == 0) return s.substring(0, i);
        }
        return s;
    }
    static String fortranModName(String fooType) { return fortranTypeName(fooType) + "_MODULE"; }

    /** Scan every *.foo for module-level `public` variable declarations (the
     *  cross-module globals: stdin/stdout/stderr, tonto, std_time, the
     *  gaussian_data tables, etc.). Returns name -> {canon foo type, fortran
     *  module}. A fast text scan (no parse): module-level decls sit at exactly
     *  3-space indent, before `contains`, and carry the `public` attribute. */
    static Map<String, String[]> buildGlobalTable(Path foofilesDir) {
        Map<String, String[]> g = new HashMap<>();
        java.io.File[] files = foofilesDir.toFile().listFiles((d, n) -> n.endsWith(".foo"));
        if (files == null) return g;
        java.util.regex.Pattern modPat =
            java.util.regex.Pattern.compile("^\\s*(?:virtual\\s+)?(?:array\\s+)?module\\s+(\\S+)");
        java.util.regex.Pattern declPat =
            java.util.regex.Pattern.compile("^ {3}([A-Za-z]\\w*)\\s*::([^:].*)$");
        for (java.io.File f : files) {
            String fooMod = null; boolean past = false;
            List<String> lines;
            try { lines = Files.readAllLines(f.toPath(), StandardCharsets.UTF_8); }
            catch (Exception e) {
                try { lines = Files.readAllLines(f.toPath(), StandardCharsets.ISO_8859_1); }
                catch (Exception e2) { continue; }
            }
            for (String line : lines) {
                if (fooMod == null) {
                    java.util.regex.Matcher m = modPat.matcher(line);
                    if (m.find()) { fooMod = m.group(1); continue; }
                }
                if (line.strip().equalsIgnoreCase("contains")) { past = true; continue; }
                if (past || fooMod == null) continue;
                java.util.regex.Matcher m = declPat.matcher(line);
                if (!m.find()) continue;
                String rest = m.group(2);                      // after '::', e.g. "TEXTFILE@, public"
                int depth = 0, cut = rest.length();
                for (int i = 0; i < rest.length(); i++) {
                    char c = rest.charAt(i);
                    if (c == '{' || c == '(') depth++;
                    else if (c == '}' || c == ')') depth--;
                    else if (c == ',' && depth == 0) { cut = i; break; }
                }
                String typeSpec = rest.substring(0, cut).trim();   // type (+ptr suffix)
                String attrs = rest.substring(Math.min(cut + 1, rest.length()));
                if (!attrs.matches(".*\\bpublic\\b.*")) continue;
                String fooType = canon(typeSpec).replace("@", "").replace("*", "").replace("?", "");
                if (fooType.isEmpty()) continue;
                g.putIfAbsent(m.group(1), new String[]{fooType, fortranModName(fooMod)});
            }
        }
        return g;
    }

    static final Set<String> INTRINSIC_SCALAR = Set.of("INT", "REAL", "CPX", "BIN", "STR");
    static boolean isIntrinsicScalar(String t) { return INTRINSIC_SCALAR.contains(canon(t)); }

    /** Identifier attribute words that can be mis-parsed as a (bogus) type. */
    static final Set<String> ATTR_WORDS = Set.of(
        "allocatable", "pointer", "ptr", "target", "save", "readonly",
        "optional", "private", "public", "in", "out", "inout");

    static final Set<String> ASSERT_MACROS = Set.of(
        "ENSURE", "DIE_IF", "WARN_IF", "DIE", "WARN");

    static String nameText(FooParser.NameContext n) {
        // strip only a TRAILING '?' (the placeholder marker); keep any embedded
        // '?' (e.g. make_Hirshfeld?_atom_ED_grid) so applySubst can substitute it.
        String t = n.getText();
        return t.endsWith("?") ? t.substring(0, t.length() - 1) : t;
    }

    static String outStem(String fooName) {
        // vec{real}.foo -> vec_real ; diffraction_data.set.foo -> diffraction_data.set
        // (braces/commas become '_', but the submodule '.' is kept).
        String s = fooName.endsWith(".foo") ? fooName.substring(0, fooName.length() - 4) : fooName;
        s = s.replace('{', '_').replace('}', '_').replace(',', '_');
        return s.replaceAll("_+", "_").replaceAll("_(?=\\.|$)", "");
    }

    // ------------------------------------------------------------------- main

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
        if (fooPath == null || outDir == null)
            throw new IllegalArgumentException("Usage: FooToFortran --foo <f.foo> --out-dir <d> "
                + "[--types <types.foo>] [--foofiles-dir <d>]");
        if (foofilesDir == null) foofilesDir = fooPath.toAbsolutePath().getParent();
        if (typesPath == null)   typesPath   = foofilesDir.resolve("types.foo");

        TypeTable types = new TypeTable();
        if (Files.exists(typesPath)) buildTypeTable(types, typesPath);

        Map<String, String[]> globals = buildGlobalTable(foofilesDir);

        ModuleEmitter em = new ModuleEmitter(types, parseFile(fooPath), fooPath, foofilesDir, globals);
        em.emit();
        Files.createDirectories(outDir);
        String name = fooPath.getFileName().toString();
        if (em.isVirtual) {
            // Virtual (get_from template) modules are inlined into their heirs and
            // never compiled (foo.pl emits no .F90 for them either). But the build
            // declares a per-file .F90 output for every source, so emit an empty
            // stub to satisfy that rule; it is excluded from the compiled sources.
            Files.writeString(outDir.resolve(outStem(name) + ".F90"),
                "! " + name + " is a virtual (get_from) template module - no compiled code\n",
                StandardCharsets.UTF_8);
            return;
        }
        // .F90 file is underscored; .int/.use keep the brace form (match foo.pl).
        Files.writeString(outDir.resolve(outStem(name) + ".F90"), em.f90.toString(), StandardCharsets.UTF_8);
        Files.writeString(outDir.resolve(braceStem(name) + ".int"), em.intf.toString(), StandardCharsets.UTF_8);
        Files.writeString(outDir.resolve(braceStem(name) + ".use"), em.use.toString(), StandardCharsets.UTF_8);
    }

    /** Brace-form stem: the .foo file name without its extension (vec{diis}). */
    static String braceStem(String fooName) {
        return fooName.endsWith(".foo") ? fooName.substring(0, fooName.length() - 4) : fooName;
    }

    // --------------------------------------------------------------- emitter

    static final class ModuleEmitter {
        final TypeTable types;
        final Parsed main;
        final Path fooPath, foofilesDir;
        final Map<String, Parsed> parentCache = new HashMap<>();

        String fooModuleName, selfFooType, currentProc, currentProcBase, selfModuleName;
        boolean isVirtual;
        boolean inComponent;   // true while emitting derived-type components
        List<String> selectKeywords;   // case labels of the enclosing select (for UNKNOWN)
        final Set<String> selflessProcs = new LinkedHashSet<>();   // selfless proc names in this module
        boolean inheritInjectPending; String inheritParent;
        Set<String> currentArgs = new LinkedHashSet<>();
        Map<String, String> localVarTypes = new HashMap<>();   // local/arg name -> base foo type
        final Map<String, String> moduleVars = new HashMap<>();   // this module's own vars -> foo type
        Map<String, String> subst = new LinkedHashMap<>();   // type-param substitutions (get_from)
        final StringBuilder f90 = new StringBuilder(), intf = new StringBuilder(), use = new StringBuilder();
        final Map<String, Integer> overloadCount = new HashMap<>();   // base name -> overload count
        final Map<String, Integer> overloadIdx = new HashMap<>();     // base name -> running index
        final Map<String, List<String>> interfaceProcs = new LinkedHashMap<>(); // base -> specific names
        final Map<String, Set<String>> useOnly = new TreeMap<>();

        final Map<String, String[]> globals;   // global var name -> {canon foo type, fortran module}

        ModuleEmitter(TypeTable types, Parsed main, Path fooPath, Path foofilesDir,
                      Map<String, String[]> globals) {
            this.types = types; this.main = main; this.fooPath = fooPath; this.foofilesDir = foofilesDir;
            this.globals = globals;
        }

        void emit() {
            FooParser.ModuleDefContext mod =
                descendants(main.tree, FooParser.ModuleDefContext.class).get(0);
            fooModuleName = mod.moduleName().getText();
            // For a submodule (MOLECULE.BASE) self's type is the main type (MOLECULE).
            selfFooType   = fooModuleName.contains(".")
                ? fooModuleName.substring(0, fooModuleName.indexOf('.')) : fooModuleName;
            selfModuleName = fortranTypeName(fooModuleName) + "_MODULE";
            // `virtual module X` is a get_from template: parsed, but not compiled.
            if (mod.IDENTIFIER() != null
                    && mod.IDENTIFIER().getText().equalsIgnoreCase("virtual")) {
                isVirtual = true;
                return;
            }
            // The include directives keep the brace form (vec{diis}.use), even
            // though the .F90 file itself is underscored (vec_diis.F90).
            String stem = braceStem(fooPath.getFileName().toString());

            Cursor c = new Cursor(main.toks);
            c.flushHidden(f90, mod.MODULE().getSymbol().getTokenIndex(), 0);  // doc block

            // pre-pass: count overloads per procedure name (templates excluded);
            // also record which procedures are `selfless` (no self argument)
            for (FooParser.ProcDefContext pd : descendants(mod, FooParser.ProcDefContext.class)) {
                Attrs pa = Attrs.parse(pd.procHeader().procAttrs());
                if (pa.template) continue;
                overloadCount.merge(pd.procHeader().IDENTIFIER().getText(), 1, Integer::sum);
                if (pa.selfless) selflessProcs.add(pd.procHeader().IDENTIFIER().getText());
            }
            // record this module's own module-level variables (public AND private)
            // so `connections_for(X).element` resolves to connections_for(X)%element
            for (int ci = 0; ci < mod.getChildCount(); ci++) {
                ParseTree ch = mod.getChild(ci);
                if (!(ch instanceof FooParser.ModuleDataItemContext)) continue;
                FooParser.ModuleDataItemContext mi = (FooParser.ModuleDataItemContext) ch;
                if (mi.varDecl() == null || mi.varDecl().declTail().typeSpec() == null) continue;
                String t = canon(mi.varDecl().declTail().typeSpec().getText()).replace("?", "");
                for (FooParser.DeclNameContext dn : mi.varDecl().identList().declName())
                    moduleVars.put(nameText(dn.name()), t);
            }

            f90.append("module ").append(fortranTypeName(fooModuleName)).append("_MODULE\n\n");
            f90.append("#  include \"").append(stem).append(".use\"\n");
            c.pos = mod.MODULE().getSymbol().getTokenIndex() + 1;

            // Walk the module's children in source order: module-level use/type/
            // var/data items (the "data section"), then contains + procedures.
            boolean implicitDone = false;
            for (int i = 0; i < mod.getChildCount(); i++) {
                ParseTree ch = mod.getChild(i);
                if (ch instanceof FooParser.ModuleDataItemContext) {
                    FooParser.ModuleDataItemContext mi = (FooParser.ModuleDataItemContext) ch;
                    if (mi.NEWLINE() != null) continue;
                    c.flushHidden(f90, mi.getStart().getTokenIndex(), 3);
                    if (mi.implicitStmt() != null) { emitImplicitBlock(stem); implicitDone = true; }
                    else if (mi.useStmt() != null) emitModuleUse(mi.useStmt());
                    else if (mi.typeDef() != null) emitTypeDef(mi.typeDef(), c);
                    else if (mi.varDecl() != null) {
                        int before = f90.length();
                        emitDecl(mi.varDecl().identList(), mi.varDecl().declTail(), 3);
                        spliceTrailingComment(c, mi.getStop().getTokenIndex(), before);
                    }
                    else if (mi.dataStmt() != null) emitDataStmt(mi.dataStmt(), 3);
                    else if (mi.interfaceBlock() != null && mi.interfaceBlock().IDENTIFIER() != null) {
                        // Explicit module-level generic interface
                        //   `interface NAME  m1  m2  … end`
                        // groups distinctly-named specifics under one generic. Register
                        // NAME -> {m1,m2,…} so buildInterfaceFile emits `interface NAME_`.
                        // foo.pl drops these, so `.NAME` calls have no generic to resolve.
                        String gname = mi.interfaceBlock().IDENTIFIER().getText();
                        for (FooParser.GenericItemContext gi : mi.interfaceBlock().genericItem())
                            for (FooParser.ProcHeaderContext ph : gi.procHeader())
                                interfaceProcs.computeIfAbsent(gname, k -> new ArrayList<>())
                                              .add(ph.IDENTIFIER().getText());
                    }
                    c.pos = Math.max(c.pos, mi.getStop().getTokenIndex() + 1);
                    c.lastLine = mi.getStop().getLine();
                } else if (ch instanceof org.antlr.v4.runtime.tree.TerminalNode
                           && ((org.antlr.v4.runtime.tree.TerminalNode) ch).getSymbol().getType() == FooLexer.CONTAINS) {
                    if (!implicitDone) { emitImplicitBlock(stem); implicitDone = true; }
                    f90.append("\ncontains\n");
                    c.pos = Math.max(c.pos, ((org.antlr.v4.runtime.tree.TerminalNode) ch).getSymbol().getTokenIndex() + 1);
                    c.lastLine = -1;
                } else if (ch instanceof FooParser.ModuleProcItemContext) {
                    FooParser.ModuleProcItemContext pi = (FooParser.ModuleProcItemContext) ch;
                    if (pi.procDef() != null) {
                        emitProc(pi.procDef(), c);
                        c.lastLine = pi.procDef().getStop().getLine();
                    }
                }
            }
            if (!implicitDone) emitImplicitBlock(stem);

            f90.append("\nend module\n");

            buildInterfaceFile();
            buildUseFile();
        }

        // ---- procedures ------------------------------------------------

        void emitProc(FooParser.ProcDefContext pd, Cursor c) {
            FooParser.ProcHeaderContext h = pd.procHeader();
            String name = h.IDENTIFIER().getText();
            Attrs a = Attrs.parse(h.procAttrs());
            if (a.template) return;
            int nOver = overloadCount.getOrDefault(name, 1);
            int idx = overloadIdx.getOrDefault(name, 0);
            overloadIdx.put(name, idx + 1);
            String specName = nOver > 1 ? name + "_" + idx : name; // overloads -> name_0, name_1
            currentProc = specName;                               // overload-specific (for ENSURE prefix)
            currentProcBase = name;                               // base name (for get_from parent lookup)
            interfaceProcs.computeIfAbsent(name, k -> new ArrayList<>()).add(specName);

            // section comments / blanks preceding this procedure
            int hdrTok = h.getStart().getTokenIndex();
            f90.append('\n');
            c.flushHidden(f90, hdrTok, 0);

            List<String> args = new ArrayList<>();
            if (h.procArgs() != null && h.procArgs().identList() != null)
                for (FooParser.DeclNameContext dn : h.procArgs().identList().declName())
                    args.add(nameText(dn.name()));
            boolean func = h.procResult() != null;
            String result = func ? h.procResult().IDENTIFIER().getText() : null;

            List<String> callArgs = new ArrayList<>();
            if (!a.selfless) callArgs.add("self");
            callArgs.addAll(args);
            currentArgs = new LinkedHashSet<>(callArgs);

            StringBuilder hdr = new StringBuilder("   ");
            if (a.prefix() != null) hdr.append(a.prefix()).append(' ');
            hdr.append(func ? "function " : "subroutine ").append(specName);
            hdr.append('(').append(String.join(",", callArgs)).append(')');
            if (func) hdr.append(" result (").append(result).append(')');
            f90.append(hdr).append('\n');

            // advance the main cursor past this stub's tokens (so following
            // section comments are attributed to the next procedure)
            int endTok = pd.getStop().getTokenIndex();

            if (a.inherited) {
                a.signatureComment = signatureComment(main, pd);
                // signature comment from THIS file (between header NEWLINE and end)
                Cursor sc = new Cursor(main.toks);
                sc.pos = h.getStop().getTokenIndex() + 1;
                sc.flushHidden(f90, endTok, 0);
                emitInheritedBody(a, func, args);
            } else {
                renderBody(main, pd, /*inherited=*/false, null, a.selfless);
            }
            c.pos = Math.max(c.pos, endTok + 1);
            f90.append(func ? "   end function\n" : "   end subroutine\n");
        }

        void emitImplicitBlock(String stem) {
            f90.append("\n   implicit none\n\n");
            f90.append("#  include \"macros\"\n");
            f90.append("#  include \"").append(stem).append(".int\"\n\n");
        }

        /** A module-level `use` of an external (non-Foo) module, e.g. `USE mpi`. */
        void emitModuleUse(FooParser.UseStmtContext u) {
            StringBuilder s = new StringBuilder("   ").append(u.USE().getText()).append(' ')
                .append(u.moduleRef().getText());
            List<FooParser.NameContext> ns = u.name();
            if (!ns.isEmpty()) {                    // ..., only: a, b
                s.append(", ").append(nameText(ns.get(0))).append(": ");
                List<String> rest = new ArrayList<>();
                for (int i = 1; i < ns.size(); i++) rest.add(nameText(ns.get(i)));
                s.append(String.join(",", rest));
            }
            f90.append(s).append('\n');
        }

        /** A derived-type definition: `type IRREP_TYPE … end type`. */
        void emitTypeDef(FooParser.TypeDefContext td, Cursor c) {
            // `array type VEC{…}` only signals to the legacy translator that such
            // array types occur; foo.pl emits no derived type for it (just any
            // body comment). Emit no type/end type wrapper, but keep the comment.
            boolean isArray = td.IDENTIFIER() != null
                && td.IDENTIFIER().getText().equalsIgnoreCase("array");
            if (!isArray)
                f90.append("   type ").append(fortranTypeName(td.typeSpec().getText())).append("_TYPE\n");
            inComponent = true;
            for (int i = 0; i < td.getChildCount(); i++) {
                ParseTree ch = td.getChild(i);
                if (ch instanceof FooParser.VarDeclContext) {
                    FooParser.VarDeclContext vd = (FooParser.VarDeclContext) ch;
                    c.flushHidden(f90, vd.getStart().getTokenIndex(), 5);
                    emitDecl(vd.identList(), vd.declTail(), 5);
                    c.pos = Math.max(c.pos, vd.getStop().getTokenIndex() + 1);
                    c.lastLine = vd.getStop().getLine();
                }
            }
            // trailing comments in the type body (after the last component, or a
            // comment-only body like the `array type …` declarations)
            inComponent = false;
            if (td.endKw() != null)
                c.flushHidden(f90, td.endKw().getStart().getTokenIndex(), 6);
            if (!isArray) f90.append("   end type\n");
        }

        /** A Fortran data statement: data name(dims)/ v1, v2, … / (one line, compilable). */
        void emitDataStmt(FooParser.DataStmtContext d, int indent) {
            String head = sp(indent) + "data " + nameText(d.name())
                        + (d.dimSpec() != null ? d.dimSpec().getText() : "") + "/";
            List<String> vals = new ArrayList<>();
            for (FooParser.DataValueContext dv : d.dataValue()) vals.add(dv.getText());
            // Wrap so no physical line (incl. trailing " &") exceeds Fortran's 132.
            // Break only after a comma — never mid-value — so strings stay intact.
            final int MAX = 128;
            StringBuilder out = new StringBuilder(), line = new StringBuilder(head);
            int prefixLen = line.length();
            for (int i = 0; i < vals.size(); i++) {
                String piece = vals.get(i) + (i == vals.size() - 1 ? "/" : ",");
                if (line.length() > prefixLen && line.length() + piece.length() > MAX) {
                    out.append(line).append(" &\n");
                    line = new StringBuilder(sp(indent));
                    prefixLen = line.length();
                }
                line.append(piece);
            }
            if (vals.isEmpty()) line.append("/");
            f90.append(out).append(line).append('\n');
        }

        /** Resolve and splice the parent body for get_from(...). */
        void emitInheritedBody(Attrs a, boolean func, List<String> args) {
            ParentRef pr = ParentRef.parse(a.getFromTarget, fooModuleName);
            String routine = pr.routine != null ? pr.routine : currentProcBase;
            try {
                Parsed parent = loadModule(pr.module);
                FooParser.ProcDefContext target = findOverload(parent, routine, a.signatureComment, args);
                if (target != null) {
                    subst = buildSubst(a.getFromAttr, pr.module);
                    renderBody(parent, target, true, pr.module, a.selfless);
                    subst = new LinkedHashMap<>();
                    return;
                }
            } catch (IOException ignored) { }
            f90.append("      ! get_from(").append(a.getFromTarget)
               .append(") — parent body not found\n");
        }

        /** Resolve a `.selector` that is a get_from placeholder to its substitution
         *  value (plain identifier only) before deciding component-vs-method, so
         *  `self(a).TEST?` with TEST?=>basis becomes self(a)%basis (basis is a
         *  component), matching foo.pl's substitute-then-translate order. */
        String substSelector(String sel) {
            if (subst.isEmpty()) return sel;
            String v = subst.get(sel);
            if (v == null) v = subst.get(sel + "?");
            return (v != null && v.matches("\\w+")) ? v : sel;
        }

        /** Pair a parent type-arg with the child's, recursing into nested params. */
        void pairTypeArgs(Map<String, String> m, String parent, String child) {
            parent = canon(parent); child = canon(child);
            if (parent.equals(child)) return;
            m.putIfAbsent(parent, child);
            List<String> p = typeArgsOf(parent), c = typeArgsOf(child);
            for (int i = 0; i < Math.min(p.size(), c.size()); i++) pairTypeArgs(m, p.get(i), c.get(i));
        }

        /** Build the placeholder substitution map for a get_from(...) directive. */
        Map<String, String> buildSubst(FooParser.AttrContext gf, String parentModule) {
            Map<String, String> m = new LinkedHashMap<>();
            // positional type-arg substitution: parent type args -> child (self) type
            // args, recursing into nested params so e.g. MAP{VEC{KEY},VEC{VAL}} vs
            // MAP{VEC{INT},VEC{INT}} yields KEY->INT and VAL->INT (not just VEC{KEY}->..).
            List<String> p = typeArgsOf(parentModule), c = typeArgsOf(selfFooType);
            for (int i = 0; i < Math.min(p.size(), c.size()); i++) pairTypeArgs(m, p.get(i), c.get(i));
            // The bare universal self-type placeholders INTRINSIC / OBJECT map to the
            // inheriting type: get_from(INTRINSIC) in module INT makes INTRINSIC -> INT,
            // so a template declaration `v :: VEC{INTRINSIC}` becomes `VEC{INT}`. (The
            // parameterised case VEC{INTRINSIC} inherited by VEC{REAL} is already
            // handled positionally above.)
            String pBase = canon(parentModule);
            if ((pBase.equals("INTRINSIC") || pBase.equals("OBJECT")) && !pBase.equals(canon(selfFooType)))
                m.putIfAbsent(pBase, selfFooType);
            // named substitutions from `KEY?=>VAL` arguments (skip arg 0 = the module)
            if (gf != null) {
                List<FooParser.GetFromArgContext> args = gf.getFromArg();
                for (int i = 1; i < args.size(); i++) {
                    FooParser.GetFromArgContext ga = args.get(i);
                    if (ga.ARROW() != null && ga.getFromKey() != null) {
                        String key = ga.getFromKey().getText() + (ga.QUESTION() != null ? "?" : "");
                        String val = ga.getFromVal() != null ? ga.getFromVal().getText() : "";
                        // A self-component value (ARG?=>.use_BFGS) -> self%use_BFGS. A
                        // self-method value (SET?=>.set_x) is left as ".set_x" — applySubst
                        // turns the call site KEY(args) into set_x_(self,args).
                        if (val.matches("\\.\\w+(\\.\\w+)*")
                            && types.isComponent(selfFooType, val.substring(1).split("\\.")[0]))
                            val = "self%" + val.substring(1).replace(".", "%");
                        else if (val.matches("self(\\.\\w+)+")) val = val.replace(".", "%");
                        m.put(key, val);
                    }
                }
            }
            return m;
        }

        /** Apply the active substitution map (whole-token) to a text fragment. */
        String applySubst(String s) {
            if (subst.isEmpty() || s == null) return s;
            List<String> keys = new ArrayList<>(subst.keySet());
            keys.sort((x, y) -> Integer.compare(y.length(), x.length()));  // longest first
            for (String k : keys) {
                String base = k.endsWith("?") ? k.substring(0, k.length() - 1) : k;
                String v = subst.get(k);
                String qb = java.util.regex.Pattern.quote(base);
                // A method-reference value — `.set_x` (self-method) or `:make_grid`
                // (same-module generic). Two body forms:
                //   .KEY?(a) already rendered KEY_(self,a) -> just rename the stem;
                //    KEY?(a) (bare placeholder) -> m_(self,a); bare KEY -> m_(self).
                if (v.matches("[.:]\\w+")) {
                    String m = v.substring(1);
                    s = s.replaceAll("\\b" + qb + "(?=_)", java.util.regex.Matcher.quoteReplacement(m));
                    s = s.replaceAll("\\b" + qb + "\\??\\s*\\(",
                                     java.util.regex.Matcher.quoteReplacement(m + "_(self,"));
                    s = s.replaceAll("\\b" + qb + "\\??",
                                     java.util.regex.Matcher.quoteReplacement(m + "_(self)"));
                    continue;
                }
                String repl = java.util.regex.Matcher.quoteReplacement(v);
                if (!k.endsWith("?")) {                       // positional type arg
                    s = s.replaceAll("\\b" + qb + "\\b", repl);
                    continue;
                }
                // Explicit `KEY?` (raw text) is always replaced. No leading \b so an
                // embedded placeholder (make_Hirshfeld?_atom) matches even when
                // preceded by '_'; longest-first ordering avoids suffix over-matches.
                s = s.replaceAll(qb + "\\?", repl);
                // `KEY` with the '?' stripped by nameText: bare (followed by a
                // non-word) or a generic-call stem `KEY_` (a method placeholder
                // `.GRID?(..)` renders as GRID_(self,..); keep the trailing '_').
                // Skip this if the base also names a real arg/local (V? vs the V arg).
                boolean isVar = currentArgs.contains(base) || localVarTypes.containsKey(base);
                if (!isVar)
                    s = s.replaceAll("\\b" + qb + "(?=_|[^A-Za-z0-9_]|$)", repl);
            }
            return s;
        }

        Parsed loadModule(String fooModule) throws IOException {
            // file head is the lower-cased type name, e.g. OBJECT -> object.foo
            String file = fooModule.toLowerCase(Locale.ROOT);
            Parsed p = parentCache.get(file);
            if (p == null) { p = parseFile(foofilesDir.resolve(file + ".foo")); parentCache.put(file, p); }
            return p;
        }

        /** Find a procDef of given name; if several, match by signature comment. */
        FooParser.ProcDefContext findOverload(Parsed src, String name, String sigComment, List<String> args) {
            List<FooParser.ProcDefContext> matches = new ArrayList<>();
            for (FooParser.ProcDefContext pd : descendants(src.tree, FooParser.ProcDefContext.class))
                if (pd.procHeader().IDENTIFIER().getText().equals(name)) matches.add(pd);
            if (matches.isEmpty()) return null;
            if (matches.size() == 1) return matches.get(0);
            // 1) narrow to templates with the exact argument-name list (a get_from
            //    proc shares its template's arg names, e.g. (a,transpose_a,dagger_a))
            List<FooParser.ProcDefContext> pool = new ArrayList<>();
            if (args != null)
                for (FooParser.ProcDefContext pd : matches)
                    if (args.equals(procArgNames(pd))) pool.add(pd);
            if (pool.isEmpty()) pool = matches;           // arg list didn't disambiguate
            if (pool.size() == 1) return pool.get(0);
            // 2) among the survivors, the signature (doc) comment is the tiebreaker
            if (sigComment != null)
                for (FooParser.ProcDefContext pd : pool)
                    if (sigComment.equals(signatureComment(src, pd))) return pd;
            return pool.get(0);
        }

        /** The declared argument names of a procedure header. */
        List<String> procArgNames(FooParser.ProcDefContext pd) {
            List<String> r = new ArrayList<>();
            FooParser.ProcHeaderContext h = pd.procHeader();
            if (h.procArgs() != null && h.procArgs().identList() != null)
                for (FooParser.DeclNameContext dn : h.procArgs().identList().declName())
                    r.add(nameText(dn.name()));
            return r;
        }

        /** The `! ...` signature comment immediately after a proc header. */
        String signatureComment(Parsed src, FooParser.ProcDefContext pd) {
            int from = pd.procHeader().getStop().getTokenIndex();
            int to = pd.getStop().getTokenIndex();
            StringBuilder sb = new StringBuilder();
            boolean started = false;
            for (int i = from + 1; i <= to; i++) {
                Token t = src.toks.get(i);
                int ty = t.getType();
                if (ty == FooLexer.NEWLINE) continue;
                if (t.getChannel() == Token.HIDDEN_CHANNEL) {
                    // collect the whole leading comment block (the first line alone
                    // is often shared between overloads — e.g. change_basis_to)
                    if (ty == FooLexer.COMMENT) { sb.append(t.getText().trim()).append('\n'); started = true; }
                    continue;
                }
                break;   // a real declaration/statement -> end of the comment block
            }
            return started ? sb.toString() : null;
        }

        // ---- body rendering -------------------------------------------

        /** Render a procedure body (decls + statements + hidden tokens). */
        void renderBody(Parsed src, FooParser.ProcDefContext pd, boolean inherited,
                        String parentName, boolean selfless) {
            List<FooParser.ProcBodyContext> body = pd.procBody();
            Cursor c = new Cursor(src.toks);
            c.pos = pd.procHeader().getStop().getTokenIndex() + 1;
            inheritInjectPending = inherited; inheritParent = parentName;
            if (inherited) {
                c.lastLine = -1;                // suppress spurious leading blanks
                // skip the parent's own signature comment (we emit the inheriting
                // file's): jump to the first real (decl/stmt) body element, past
                // any leading blank lines and the parent comment. BUT a preprocessor
                // directive (#ifdef ...) preceding that first statement is code
                // structure, not documentation — stop at it so it survives, else a
                // leading `#ifdef MPI` is dropped while its `#endif` remains.
                int hdrEnd = pd.procHeader().getStop().getTokenIndex() + 1;
                for (FooParser.ProcBodyContext b : body)
                    if (b.localDecl() != null || b.stmt() != null) {
                        int firstStmt = b.getStart().getTokenIndex();
                        c.pos = firstStmt;
                        for (int i = hdrEnd; i < firstStmt; i++)
                            if (src.toks.get(i).getType() == FooLexer.PP_LINE) { c.pos = i; break; }
                        break;
                    }
            } else if (!body.isEmpty()) {
                // flush the leading signature comment before any implicit self decl
                c.flushHidden(f90, body.get(0).getStart().getTokenIndex(), 6);
            }
            // per-procedure symbol table: local/arg variable types (for resolving
            // `localvar.component` -> `localvar%component`)
            localVarTypes = new HashMap<>();
            for (FooParser.ProcBodyContext b : body)
                if (b.localDecl() != null && b.localDecl().declTail().typeSpec() != null) {
                    String t = canon(applySubst(b.localDecl().declTail().typeSpec().getText())).replace("?", "");
                    for (FooParser.DeclNameContext dn : b.localDecl().identList().declName())
                        localVarTypes.put(nameText(dn.name()), t);
                }
            // self is declared implicitly when the body doesn't declare it itself
            // (but not for `routinal`/`functional` procs, where self is a procedure
            // argument given an interface block in the body — a scalar `TYPE :: self`
            // there would clash with the dummy procedure; foo.pl emits it anyway,
            // which is a bug).
            Attrs pAttrs = Attrs.parse(pd.procHeader().procAttrs());
            if (!selfless && !pAttrs.routinal && !pAttrs.functional && !bodyDeclaresSelf(body))
                f90.append("      ").append(selfDeclType()).append(" :: self\n");
            emitBodyList(body, c, 6, true);
            c.flushHidden(f90, pd.getStop().getTokenIndex(), 6);   // trailing comments
        }

        /** Emit a `routinal` function-argument interface block. */
        void emitInterfaceBlock(FooParser.InterfaceBlockContext ib, int indent) {
            f90.append(sp(indent)).append("interface\n");
            if (ib.abstractItem() != null)
                for (FooParser.AbstractItemContext ai : ib.abstractItem())
                    if (ai.procDef() != null) emitInterfaceProc(ai.procDef(), indent + 3);
            f90.append(sp(indent)).append("end interface\n");
        }

        void emitInterfaceProc(FooParser.ProcDefContext pd, int indent) {
            FooParser.ProcHeaderContext h = pd.procHeader();
            Attrs a = Attrs.parse(h.procAttrs());
            List<String> args = procArgNames(pd);
            boolean func = h.procResult() != null;
            StringBuilder hdr = new StringBuilder(sp(indent));
            if (a.prefix() != null) hdr.append(a.prefix()).append(' ');
            hdr.append(func ? "function " : "subroutine ").append(h.IDENTIFIER().getText())
               .append('(').append(String.join(",", args)).append(')');
            if (func) hdr.append(" result (").append(h.procResult().IDENTIFIER().getText()).append(')');
            f90.append(hdr).append('\n');
            Set<String> saved = currentArgs;
            currentArgs = new LinkedHashSet<>(args);
            for (FooParser.ProcBodyContext b : pd.procBody()) {
                if (b.useStmt() != null)
                    f90.append(sp(indent)).append("use ")
                       .append(fortranModName(b.useStmt().moduleRef().getText())).append('\n');
                else if (b.localDecl() != null)
                    emitDecl(b.localDecl().identList(), b.localDecl().declTail(), indent + 3);
            }
            currentArgs = saved;
            f90.append(sp(indent)).append(func ? "end function\n" : "end subroutine\n");
        }

        /** A `TYPE::method(args)` statement mis-parsed as a declaration: the
         *  declared "name" is an uppercase TYPE and the "type" is a lowercase
         *  method with an argument list. */
        boolean misparsedTypeCall(FooParser.LocalDeclContext ld) {
            if (ld.identList().declName().size() != 1) return false;
            FooParser.DeclNameContext dn = ld.identList().declName(0);
            if (dn.dimSpec() != null || !isModuleLikeQualifier(nameText(dn.name()))) return false;
            FooParser.TypeSpecContext ts = ld.declTail().typeSpec();
            // The "type" after :: must be a lowercase method name (not a real type),
            // with no ptr/attr/initialiser — then it's actually a TYPE::method call,
            // with args `(…)` (a dimSpec) or none.
            return ts != null && ts.baseType() != null
                && !isModuleLikeQualifier(ts.baseType().getText())
                && ld.declTail().ptrSuffix() == null && ld.declTail().attrSuffix() == null
                && ld.declTail().initSuffix() == null;
        }

        void emitTypeQualifiedCallStmt(FooParser.LocalDeclContext ld, int indent) {
            String type = nameText(ld.identList().declName(0).name());
            FooParser.TypeSpecContext ts = ld.declTail().typeSpec();
            String method = ts.baseType().getText();           // non-generic (`::`) -> specific name
            String args = ts.dimSpec() != null ? renderDimSpec(ts.dimSpec()) : "";   // no parens if no args
            f90.append(sp(indent)).append("call ").append(method).append(args).append('\n');
            recordUse(fortranModName(type), method);
        }

        boolean bodyDeclaresSelf(List<FooParser.ProcBodyContext> body) {
            for (FooParser.ProcBodyContext b : body)
                if (b.localDecl() != null)
                    for (FooParser.DeclNameContext dn : b.localDecl().identList().declName())
                        if (nameText(dn.name()).equals("self")) return true;
            return false;
        }

        /** Emit a list of body elements (decls/statements) at a given indent.
         *  When hoist is true (the top-level procedure body), ENSURE/DIE/WARN
         *  precondition statements that appear before the first executable
         *  statement are stored and re-emitted (at column 3) after the whole
         *  declaration block — they assert on the arguments, and Fortran forbids
         *  an executable before a declaration. */
        void emitBodyList(List<FooParser.ProcBodyContext> elems, Cursor c, int indent, boolean hoist) {
            int firstActive = -1;
            if (hoist)
                for (int i = 0; i < elems.size(); i++) {
                    FooParser.ProcBodyContext b = elems.get(i);
                    if (b.stmt() != null && !isAssertionStmt(b.stmt())) { firstActive = i; break; }
                }
            List<FooParser.StmtContext> preconds = new ArrayList<>();
            for (int i = 0; i < elems.size(); i++) {
                FooParser.ProcBodyContext b = elems.get(i);
                if (b.interfaceBlock() != null) {           // `routinal` function-arg interface
                    c.flushHidden(f90, b.getStart().getTokenIndex(), indent);
                    emitInterfaceBlock(b.interfaceBlock(), indent);
                    c.pos = Math.max(c.pos, b.getStop().getTokenIndex() + 1);
                    c.lastLine = b.getStop().getLine();
                    continue;
                }
                if (b.localDecl() == null && b.stmt() == null) continue;   // blank / unhandled
                c.flushHidden(f90, b.getStart().getTokenIndex(), indent);
                if (i == firstActive) {                       // emit hoisted preconditions here
                    for (FooParser.StmtContext pc : preconds) emitPrecond(pc);
                    preconds.clear();
                }
                boolean isPre = hoist && firstActive >= 0 && i < firstActive
                                && b.stmt() != null && isAssertionStmt(b.stmt());
                if (isPre) {
                    preconds.add(b.stmt());                   // store; re-emitted at firstActive
                } else if (b.localDecl() != null && misparsedTypeCall(b.localDecl())) {
                    // `TYPE::method(args)` on its own line parses as a declaration
                    // (identList :: declTail) — re-emit it as the call it really is.
                    int before = f90.length();
                    emitTypeQualifiedCallStmt(b.localDecl(), indent);
                    spliceTrailingComment(c, b.getStop().getTokenIndex(), before);
                } else if (b.localDecl() != null) {
                    int before = f90.length();
                    emitDecl(b.localDecl().identList(), b.localDecl().declTail(), indent);
                    spliceTrailingComment(c, b.getStop().getTokenIndex(), before);
                } else {
                    int before = f90.length();
                    emitStmt(b.stmt(), c, indent);
                    spliceTrailingComment(c, b.getStop().getTokenIndex(), before);
                }
                c.pos = Math.max(c.pos, b.getStop().getTokenIndex() + 1);
                c.lastLine = b.getStop().getLine();
            }
            for (FooParser.StmtContext pc : preconds) emitPrecond(pc);   // no executable: emit at end
        }

        /** Append a same-line trailing comment to the just-emitted statement
         *  line (foo.pl keeps `stmt   ! note` together). `before` is f90's length
         *  prior to emitting the element; if nothing was emitted, do nothing. */
        void spliceTrailingComment(Cursor c, int stopTok, int before) {
            if (f90.length() <= before || f90.charAt(f90.length() - 1) != '\n') return;
            String tc = trailingComment(c, stopTok);
            if (tc != null) f90.insert(f90.length() - 1, tc);
        }

        /** Splice a trailing comment onto a block-header line (`do … ! note`).
         *  `nl` is the NEWLINE token closing the header. */
        void appendHeaderComment(Cursor c, org.antlr.v4.runtime.tree.TerminalNode nl) {
            if (nl == null) return;
            String tc = trailingComment(c, nl.getSymbol().getTokenIndex());
            if (tc != null) f90.append(tc);
        }

        /** A COMMENT trailing the statement's NEWLINE (hidden channel, same source
         *  line). Consumes it from the cursor and returns "  <text>", else null. */
        String trailingComment(Cursor c, int stopTok) {
            if (stopTok < 0 || stopTok >= c.toks.size()) return null;
            int line = c.toks.get(stopTok).getLine();
            for (int i = stopTok - 1; i >= 0; i--) {
                if (i < c.pos) break;                                // already consumed/emitted
                Token t = c.toks.get(i);
                if (t.getChannel() != Token.HIDDEN_CHANNEL) break;   // reached code
                if (t.getLine() != line) break;
                if (t.getType() == FooLexer.COMMENT) {
                    c.pos = Math.max(c.pos, i + 1);
                    return "  " + t.getText();
                }
            }
            return null;
        }

        /** True if a statement is a single ENSURE/DIE/WARN assertion call. */
        boolean isAssertionStmt(FooParser.StmtContext s) {
            if (s.simpleLine() == null || s.simpleLine().lineStmt().size() != 1) return false;
            FooParser.SimpleStmtContext st = s.simpleLine().lineStmt(0).simpleStmt();
            if (st == null || st.postfix() == null || st.postfix().head().callHead() == null) return false;
            FooParser.NameContext n = st.postfix().head().callHead().name();
            return n != null && ASSERT_MACROS.contains(nameText(n));
        }

        /** Emit a hoisted precondition (ENSURE/DIE/WARN) at column 3. */
        void emitPrecond(FooParser.StmtContext s) {
            String t = renderSimpleStmt(s.simpleLine().lineStmt(0).simpleStmt());
            if (t != null && !t.isBlank()) f90.append("   ").append(t).append('\n');
        }

        /** Inject the get_from inherited-code comment before the first statement. */
        void beforeStmt(int indent) {
            if (inheritInjectPending) {
                f90.append('\n').append(sp(indent)).append("! The following code is inherited from ")
                   .append(inheritParent).append('\n');         // raw Foo name (e.g. VEC{OBJECT})
                inheritInjectPending = false;
            }
        }

        // ---- declarations ----------------------------------------------

        void emitDecl(FooParser.IdentListContext ids, FooParser.DeclTailContext tail, int indent) {
            List<String> vars = new ArrayList<>();
            for (FooParser.DeclNameContext dn : ids.declName()) vars.add(renderDeclName(dn));

            boolean isArg = !ids.declName().isEmpty()
                && currentArgs.contains(nameText(ids.declName(0).name()));
            String ftype; List<String> attrs = new ArrayList<>(); List<String> inits = new ArrayList<>();
            boolean typeIsAttr = tail.typeSpec() != null
                && ATTR_WORDS.contains(canon(tail.typeSpec().getText()).toLowerCase(Locale.ROOT));
            if (tail.typeSpec() != null && !typeIsAttr) {
                ftype = fortranType(typeSpecText(tail.typeSpec()), isArg);
                if (tail.ptrSuffix() != null)
                    attrs.add(tail.ptrSuffix().getText().equals("@") ? "allocatable" : "PTR");
                if (tail.attrSuffix() != null)
                    for (FooParser.AttrContext at : tail.attrSuffix().attr()) addAttrOrInit(at, attrs, inits);
            } else {
                // attrs-only declaration (implicit self type), incl. an attribute
                // word mis-parsed as a type (e.g. `self :: allocatable, OUT`).
                ftype = selfDeclType();
                if (typeIsAttr) attrs.add(tail.typeSpec().getText());
                if (tail.ptrSuffix() != null)
                    attrs.add(tail.ptrSuffix().getText().equals("@") ? "allocatable" : "PTR");
                if (tail.attrSuffix() != null)
                    for (FooParser.AttrContext at : tail.attrSuffix().attr()) addAttrOrInit(at, attrs, inits);
                if (tail.attr() != null)
                    for (FooParser.AttrContext at : tail.attr()) addAttrOrInit(at, attrs, inits);
            }
            if (tail.initSuffix() != null) inits.add("= " + renderExpr(tail.initSuffix().expr()));
            // a pointer component with no explicit initializer defaults to null
            if (inComponent && tail.ptrSuffix() != null
                && tail.ptrSuffix().getText().equals("*") && inits.isEmpty())
                inits.add("DEFAULT_NULL");
            StringBuilder d = new StringBuilder(sp(indent)).append(ftype);
            for (String at : attrs) d.append(", ").append(at);
            d.append(" :: ").append(String.join(",", vars));
            for (String in : inits) d.append(' ').append(in);     // DEFAULT(...) / = init after var
            f90.append(d).append('\n');
        }

        /** A DEFAULT(...) attribute is a trailing initializer (after the var), not an attr. */
        void addAttrOrInit(FooParser.AttrContext at, List<String> attrs, List<String> inits) {
            String nm = at.name() != null ? at.name().getText() : "";
            if (nm.regionMatches(true, 0, "DEFAULT", 0, Math.min(7, nm.length())) && nm.length() >= 7)
                inits.add(at.getText());            // DEFAULT(...) and DEFAULT_NULL
            else if (nm.equalsIgnoreCase("readonly"))
                ;   // Foo-only access control; not a Fortran attribute (foo.pl drops it)
            else if (inComponent && nm.equalsIgnoreCase("private"))
                ;   // per-component `private` is dropped (foo.pl); `public` is kept
            else attrs.add(attrText(at));
        }

        /** The Fortran declaration type for `self` — the module's own type. */
        String selfDeclType() { return fortranType(selfFooType, /*isArg=*/true); }

        /** Foo type text -> Fortran declaration type (top-level position). */
        String fortranType(String foo, boolean isArg) {
            String c = canon(applySubst(foo)).replace("?", "");
            if (c.equals("STR")) return isArg ? "STR(len=*)" : "STR(len=STR_SIZE)";
            if (c.startsWith("STR(")) return c;
            // a non-STR intrinsic scalar never takes a (len=...) param (it leaks from
            // an INTRINSIC template instantiated as INT/REAL/...): REAL(len=..) -> REAL
            for (String sc : new String[]{"INT", "REAL", "CPX", "BIN"})
                if (c.startsWith(sc + "(")) return sc;
            if (isIntrinsicScalar(c)) return c;
            ArrayType at = parseArray(c);
            if (at != null) {
                // A leading `len=...` parameter is the STR element's length; for a
                // non-STR element it does not apply and is dropped. The rest are the
                // array dimensions. e.g. VEC{STR}(len=1,6) -> VEC(STR(len=1),6);
                // VEC{INT}(len=len(self(1))) -> VEC(INT,:).
                String lenParam = null, dims = at.dimSpec;
                if (dims != null) {
                    List<String> parts = splitTopComma(dims);
                    if (!parts.isEmpty() && parts.get(0).replace(" ", "").startsWith("len=")) {
                        lenParam = parts.get(0).replace(" ", "").substring(4);
                        parts = parts.subList(1, parts.size());
                    }
                    dims = parts.isEmpty() ? null : String.join(",", parts);
                }
                if (dims == null) dims = repeatColon(at.ndim);
                String elem = (lenParam != null && canon(at.elem).replace("?", "").equals("STR"))
                            ? "STR(len=" + lenParam + ")" : fortranElement(at.elem, isArg);
                return at.head + "(" + elem + "," + dims + ")";
            }
            // A known Foo derived type (in types.foo) or a parameterised type
            // becomes type(X_TYPE); an unknown plain identifier is an external/kind
            // type (e.g. MPI_ADDRESS, MPI_STATUS) and is kept verbatim.
            if (isFooType(c)) return "type(" + fortranTypeName(c) + "_TYPE)";
            return c;
        }

        /** Element type inside VEC{...}/MAT{...}: intrinsic kept, else type(X_TYPE). */
        String fortranElement(String elem, boolean isArg) {
            String e = canon(elem).replace("?", "");
            if (e.equals("STR")) return isArg ? "STR(len=*)" : "STR(len=STR_SIZE)";
            if (isIntrinsicScalar(e)) return e;
            if (isFooType(e)) return "type(" + fortranTypeName(e) + "_TYPE)";
            return e;
        }

        /** Is this a Foo derived type — defined in types.foo or parameterised? */
        boolean isFooType(String c) { return types.get(c) != null || c.contains("{"); }

        String attrText(FooParser.AttrContext at) {
            // dimension(.n_roots+1, …): translate the bound expressions
            if (at.name() != null && at.dimSpec() != null)
                return at.name().getText() + renderDimSpec(at.dimSpec());
            return at.getText();
        }

        /** Type text with a translated array dimension, so expressions in the
         *  dimension (e.g. `VEC{BIN}(neighbours.dim)`) are resolved rather than
         *  copied verbatim. */
        String typeSpecText(FooParser.TypeSpecContext ts) {
            String t = ts.baseType().getText();
            if (ts.QUESTION() != null) t += "?";
            if (ts.dimSpec() != null) t += renderDimSpec(ts.dimSpec());
            return t;
        }

        /** A dimension/type-parameter spec with its bound expressions translated
         *  (so `(.n_roots+1, 0:n)` -> `(self%n_roots+1, 0:n)`). */
        String renderDimSpec(FooParser.DimSpecContext ds) {
            List<String> parts = new ArrayList<>();
            for (FooParser.DimArgContext da : ds.dimArg()) parts.add(renderDimArg(da));
            return "(" + String.join(",", parts) + ")";
        }

        String renderDimArg(FooParser.DimArgContext da) {
            if (da.STAR() != null) return "*";
            if (da.IDENTIFIER() != null && da.EQUAL() != null)
                return da.IDENTIFIER().getText() + "=" + renderExpr(da.expr(0));
            if (da.COLON() != null) {                       // expr? : expr?
                int col = da.COLON().getSymbol().getTokenIndex();
                String lo = "", hi = "";
                for (FooParser.ExprContext e : da.expr())
                    if (e.getStop().getTokenIndex() < col) lo = renderExpr(e); else hi = renderExpr(e);
                return lo + ":" + hi;
            }
            return da.expr() != null && !da.expr().isEmpty() ? renderExpr(da.expr(0)) : da.getText();
        }

        /** A declared name with its dimension expressions translated. */
        String renderDeclName(FooParser.DeclNameContext dn) {
            return nameText(dn.name()) + (dn.dimSpec() != null ? renderDimSpec(dn.dimSpec()) : "");
        }

        // ---- statements ------------------------------------------------

        void emitStmt(FooParser.StmtContext s, Cursor c, int indent) {
            beforeStmt(indent);
            if (s.simpleLine() != null) {
                // Keep ';'-separated statements on one line, as foo.pl does.
                List<String> parts = new ArrayList<>();
                for (FooParser.LineStmtContext ls : s.simpleLine().lineStmt()) {
                    // a bare UNKNOWN(x) inside a select-case body expands to the
                    // known-keyword list + unknown_ call (multi-line)
                    String ua = ls.simpleStmt() != null ? unknownArg(ls.simpleStmt()) : null;
                    if (ua != null && selectKeywords != null) {
                        if (!parts.isEmpty()) {
                            f90.append(sp(indent)).append(String.join("; ", parts)).append('\n');
                            parts.clear();
                        }
                        emitUnknownExpansion(ua, indent);
                        continue;
                    }
                    String txt = renderLineStmt(ls);
                    if (txt != null && !txt.isBlank() && !txt.equalsIgnoreCase("end")) parts.add(txt);
                }
                if (!parts.isEmpty()) {
                    String line = String.join("; ", parts);
                    // preserve a trailing ';' (empty final statement) as foo.pl does
                    if (s.simpleLine().SEMI().size() >= s.simpleLine().lineStmt().size()) line += ";";
                    f90.append(sp(indent)).append(line).append('\n');
                }
                return;
            }
            if (s.ifStmt()     != null) { emitIf(s.ifStmt(), c, indent); return; }
            if (s.doStmt()     != null) { emitDo(s.doStmt(), c, indent); return; }
            if (s.selectStmt() != null) { emitSelect(s.selectStmt(), c, indent); return; }
            if (s.whereStmt()  != null) { emitWhere(s.whereStmt(), c, indent); return; }
            if (s.forallStmt() != null) { emitForall(s.forallStmt(), c, indent); return; }
            f90.append(sp(indent)).append("! TODO stmt: ").append(oneLine(s.getText())).append('\n');
        }

        // ---- block control flow ---------------------------------------

        void emitIf(FooParser.IfStmtContext x, Cursor c, int indent) {
            StringBuilder line = new StringBuilder(sp(indent))
                .append("if (").append(renderExpr(x.expr())).append(") then");
            emitInlineThenBody(line, x.inlineBody(), c, indent);
            for (FooParser.ElseIfClauseContext ei : x.elseIfClause()) {
                c.flushHidden(f90, ei.getStart().getTokenIndex(), indent);
                StringBuilder el = new StringBuilder(sp(indent))
                    .append("else if (").append(renderExpr(ei.expr())).append(") then");
                emitInlineThenBody(el, ei.inlineBody(), c, indent);
            }
            if (x.elseClause() != null) {
                FooParser.ElseClauseContext ec = x.elseClause();
                c.flushHidden(f90, ec.getStart().getTokenIndex(), indent);
                StringBuilder el = new StringBuilder(sp(indent)).append("else");
                emitInlineThenBody(el, ec.inlineBody(), c, indent);
            }
            // flush hidden tokens (trailing comments / #endif etc.) sitting between
            // the last body statement and the `end` keyword, else e.g. a closing
            // #endif is dropped and the matching #ifdef is left unterminated.
            c.flushHidden(f90, x.endKw().getStart().getTokenIndex(), indent + 3);
            f90.append(sp(indent)).append("end if\n");
        }

        /** Emit `<header>[; inline...]` then the block body at indent+3. */
        void emitInlineThenBody(StringBuilder header, FooParser.InlineBodyContext ib, Cursor c, int indent) {
            for (FooParser.SimpleStmtContext ss : ib.simpleStmt()) {
                String t = renderSimpleStmt(ss);
                if (t != null && !t.isBlank() && !t.equalsIgnoreCase("end")) header.append("; ").append(t);
            }
            f90.append(header);
            appendHeaderComment(c, ib.NEWLINE());
            f90.append('\n');
            emitBodyList(ib.procBody(), c, indent + 3, false);
        }

        void emitDo(FooParser.DoStmtContext x, Cursor c, int indent) {
            boolean parallel = false;
            for (FooParser.NameContext n : x.name()) if (nameText(n).equals("parallel")) parallel = true;
            String loopLabel = x.COLON() != null ? nameText(x.name(0)) : null;   // `main: do …`
            StringBuilder line = new StringBuilder(sp(indent));
            if (loopLabel != null) line.append(loopLabel).append(": ");
            line.append("do");
            if (parallel && x.loopHeader() != null) {
                // `parallel do v = lo,hi` -> bounded by the PARALLEL_DO_* macros
                FooParser.LoopHeaderContext lh = x.loopHeader();
                line.append(' ').append(lh.IDENTIFIER().getText())
                    .append(" = PARALLEL_DO_START(").append(renderExpr(lh.expr(0))).append(",1),")
                    .append(renderExpr(lh.expr(1))).append(",PARALLEL_DO_STRIDE(1)");
            } else if (x.loopHeader() != null) {
                line.append(' ').append(renderLoopHeader(x.loopHeader()));
            } else if (x.WHILE() != null) {
                line.append(" while (").append(renderExpr(x.expr())).append(')');
            }
            f90.append(line);
            appendHeaderComment(c, x.NEWLINE().isEmpty() ? null : x.NEWLINE(0));
            f90.append('\n');
            String tag = "\"" + fooModuleName + ":" + currentProc + "\"";   // overload-specific name
            if (parallel) f90.append(sp(indent)).append("LOCK_PARALLEL_DO(").append(tag).append(")\n");
            emitBodyList(x.procBody(), c, indent + 3, false);
            int beforeEnd = f90.length();
            f90.append(sp(indent)).append("end do").append(loopLabel != null ? " " + loopLabel : "").append('\n');
            // keep the end's trailing comment on `end do` (consume it) before the
            // UNLOCK, which goes AFTER the end do (outside the loop), matching foo.pl
            spliceTrailingComment(c, x.getStop().getTokenIndex(), beforeEnd);
            if (parallel) f90.append(sp(indent)).append("UNLOCK_PARALLEL_DO(").append(tag).append(")\n");
        }

        String renderForallHeader(FooParser.ForallHeaderContext h) {
            StringBuilder s = new StringBuilder(h.IDENTIFIER().getText()).append('=')
                .append(renderExpr(h.expr(0))).append(':').append(renderExpr(h.expr(1)));
            if (h.expr().size() > 2) s.append(':').append(renderExpr(h.expr(2)));
            return s.toString();
        }

        void emitForall(FooParser.ForallStmtContext x, Cursor c, int indent) {
            String hdr = renderForallHeader(x.forallHeader());
            if (x.simpleStmt() != null) {                 // single-statement forall
                f90.append(sp(indent)).append("forall (").append(hdr).append(") ")
                   .append(renderSimpleStmt(x.simpleStmt())).append('\n');
                return;
            }
            f90.append(sp(indent)).append("forall (").append(hdr).append(')');
            appendHeaderComment(c, x.NEWLINE().isEmpty() ? null : x.NEWLINE(0));
            f90.append('\n');
            emitBodyList(x.procBody(), c, indent + 3, false);
            f90.append(sp(indent)).append("end forall\n");
        }

        void emitWhere(FooParser.WhereStmtContext x, Cursor c, int indent) {
            StringBuilder line = new StringBuilder(sp(indent))
                .append("where (").append(renderExpr(x.expr())).append(')');
            emitInlineThenBody(line, x.inlineBody(), c, indent);
            for (FooParser.ElsewhereClauseContext ew : x.elsewhereClause()) {
                c.flushHidden(f90, ew.getStart().getTokenIndex(), indent);
                StringBuilder el = new StringBuilder(sp(indent)).append("elsewhere");
                if (ew.expr() != null) el.append(" (").append(renderExpr(ew.expr())).append(')');
                emitInlineThenBody(el, ew.inlineBody(), c, indent);
            }
            f90.append(sp(indent)).append("end where\n");
        }

        String renderLoopHeader(FooParser.LoopHeaderContext lh) {
            StringBuilder sb = new StringBuilder(lh.IDENTIFIER().getText()).append(" = ");
            List<FooParser.ExprContext> e = lh.expr();
            sb.append(renderExpr(e.get(0)));
            for (int i = 1; i < e.size(); i++) sb.append(',').append(renderExpr(e.get(i)));
            return sb.toString();
        }

        void emitSelect(FooParser.SelectStmtContext x, Cursor c, int indent) {
            f90.append(sp(indent)).append("select case (").append(renderExpr(x.expr())).append(")");
            appendHeaderComment(c, x.NEWLINE().isEmpty() ? null : x.NEWLINE(0));
            f90.append('\n');
            // collect the case-label keyword strings, for any UNKNOWN(x) expansion
            List<String> savedKw = selectKeywords;
            List<String> kw = new ArrayList<>();
            for (FooParser.CaseClauseContext cc : x.caseClause())
                if (cc.caseLabel().DEFAULT() == null)
                    for (FooParser.ArgContext a : cc.caseLabel().arg()) kw.add(renderArg(a));
            selectKeywords = kw;
            for (FooParser.CaseClauseContext cc : x.caseClause()) {
                // case labels sit at the select-case level (not indented a further 3),
                // matching foo.pl; the case body is one level (3) deeper.
                c.flushHidden(f90, cc.getStart().getTokenIndex(), indent);
                StringBuilder line = new StringBuilder(sp(indent)).append(renderCaseLabel(cc.caseLabel()));
                List<String> unkLines = null;
                for (FooParser.SimpleStmtContext ss : cc.simpleStmt()) {
                    String ua = unknownArg(ss);
                    if (ua != null) { unkLines = unknownLines(ua); continue; }   // expand below
                    String t = renderSimpleStmt(ss);
                    if (t != null && !t.isBlank() && !t.equalsIgnoreCase("end")) line.append("; ").append(t);
                }
                // foo.pl inlines the first expansion line: `case default; allocate(...)`
                if (unkLines != null && !unkLines.isEmpty()) line.append("; ").append(unkLines.get(0));
                f90.append(line);
                appendHeaderComment(c, cc.NEWLINE());
                f90.append('\n');
                if (unkLines != null)
                    for (int i = 1; i < unkLines.size(); i++)
                        f90.append(sp(indent)).append(unkLines.get(i)).append('\n');
                emitBodyList(cc.procBody(), c, indent + 3, false);
                c.pos = Math.max(c.pos, cc.getStop().getTokenIndex() + 1);
                c.lastLine = cc.getStop().getLine();
            }
            selectKeywords = savedKw;
            f90.append(sp(indent)).append("end select\n");
        }

        /** If `ss` is an `UNKNOWN(x)` call, return its (rendered) argument; else null. */
        String unknownArg(FooParser.SimpleStmtContext ss) {
            if (ss == null || ss.postfix() == null || ss.postfix().head().callHead() == null) return null;
            FooParser.NameContext n = ss.postfix().head().callHead().name();
            if (n == null || !nameText(n).equals("UNKNOWN")) return null;
            for (FooParser.TrailerContext tr : ss.postfix().trailer())
                if (tr.LPAREN() != null) return tr.argList() != null ? renderArgList(tr.argList()) : "";
            return "";
        }

        /** foo.pl expands `UNKNOWN(x)` in a select-case into the known-keyword list
         *  (built from the case labels) plus a call to unknown_. */
        List<String> unknownLines(String arg) {
            List<String> kw = selectKeywords != null ? selectKeywords : new ArrayList<>();
            List<String> r = new ArrayList<>();
            r.add("allocate(tonto%known_keywords(" + kw.size() + "))");
            for (int i = 0; i < kw.size(); i++)
                r.add("tonto%known_keywords(" + (i + 1) + ") = " + kw.get(i));
            r.add("call unknown_(tonto," + arg + ",\"" + fooModuleName + ":" + currentProcBase + "\")");
            r.add("deallocate(tonto%known_keywords)");
            return r;
        }

        void emitUnknownExpansion(String arg, int indent) {
            for (String l : unknownLines(arg)) f90.append(sp(indent)).append(l).append('\n');
        }

        String renderCaseLabel(FooParser.CaseLabelContext cl) {
            if (cl.DEFAULT() != null) return "case default";
            List<String> parts = new ArrayList<>();
            for (FooParser.ArgContext a : cl.arg()) parts.add(renderArg(a));
            return "case (" + String.join(",", parts) + ")";
        }

        String renderLineStmt(FooParser.LineStmtContext ls) {
            if (ls.oneLineIf() != null) {
                FooParser.OneLineIfContext x = ls.oneLineIf();
                return "if (" + renderExpr(x.expr()) + ") " + renderSimpleStmt(x.simpleStmt());
            }
            if (ls.oneLineWhere() != null) {
                FooParser.OneLineWhereContext x = ls.oneLineWhere();
                return "where (" + renderExpr(x.expr()) + ") " + renderSimpleStmt(x.simpleStmt());
            }
            if (ls.simpleStmt() != null) return renderSimpleStmt(ls.simpleStmt());
            return null;
        }

        String renderSimpleStmt(FooParser.SimpleStmtContext st) {
            if (st.EXIT()   != null) return st.name() == null ? "exit"  : "exit "  + nameText(st.name());
            if (st.CYCLE()  != null) return st.name() == null ? "cycle" : "cycle " + nameText(st.name());
            if (st.RETURN() != null) return "return";
            if (st.name() != null && st.LPAREN() != null) {   // Fortran I/O: write(ctrl) out-list
                StringBuilder s = new StringBuilder(nameText(st.name())).append('(');
                if (st.argList() != null) s.append(renderArgList(st.argList()));
                s.append(')');
                if (st.ioTail() != null) s.append(' ').append(renderIoTail(st.ioTail()));
                return applySubst(s.toString());
            }
            if (st.postfix() != null) {
                Chain head = translatePostfix(st.postfix(), /*statementPos=*/true);
                String txt;
                if (st.EQUAL() != null)      txt = head.text + " = "  + renderExpr(st.expr());
                else if (st.ARROW() != null) txt = head.text + " => " + renderExpr(st.expr());
                else if (st.ioTail() != null) txt = head.text + " " + renderIoTail(st.ioTail());
                else txt = head.text;
                txt = assertPrefix(applySubst(txt));
                // a statement that is a method-reference placeholder call (SET?(val))
                // becomes a subroutine call and needs the `call` prefix
                if (st.EQUAL() == null && st.ARROW() == null && st.ioTail() == null
                    && methodRefStmtCall(st)) txt = "call " + txt;
                return txt;
            }
            return null;
        }

        /** True if a statement is a bare call of a method-reference placeholder
         *  (KEY whose substitution value is a self-method `.x`). */
        boolean methodRefStmtCall(FooParser.SimpleStmtContext st) {
            if (st.postfix() == null || st.postfix().head().callHead() == null) return false;
            if (st.postfix().head().callHead().DOT() != null) return false;  // dotted -> already a call
            FooParser.NameContext n = st.postfix().head().callHead().name();
            if (n == null) return false;
            String nm = nameText(n);
            String v = subst.get(nm);
            if (v == null) v = subst.get(nm + "?");
            return v != null && v.matches("[.:]\\w+");
        }

        String renderIoTail(FooParser.IoTailContext t) {
            List<String> parts = new ArrayList<>();
            for (FooParser.ArgContext a : t.arg()) parts.add(renderArg(a));
            return String.join(",", parts);
        }

        /** Prefix ENSURE/DIE/WARN message strings with "MODULE:proc ... ". */
        String assertPrefix(String stmt) {
            int lp = stmt.indexOf('(');
            if (lp <= 0) return stmt;
            String head = stmt.substring(0, lp);
            if (!ASSERT_MACROS.contains(head)) return stmt;
            // The message is the LAST argument; insert the prefix into ITS opening
            // string — not the first quote, which may sit inside the condition
            // (e.g. ENSURE(next_item_(stdin)=="{", "...")).
            int depth = 0, lastComma = -1, close = -1;
            boolean inStr = false; char sc = 0;
            for (int i = lp; i < stmt.length(); i++) {
                char ch = stmt.charAt(i);
                if (inStr) { if (ch == sc) inStr = false; continue; }
                if (ch == '"' || ch == '\'') { inStr = true; sc = ch; }
                else if (ch == '(') depth++;
                else if (ch == ')') { if (--depth == 0) { close = i; break; } }
                else if (ch == ',' && depth == 1) lastComma = i;
            }
            int from = lastComma >= 0 ? lastComma + 1 : lp + 1;
            int q = stmt.indexOf('"', from);
            if (q < 0 || (close >= 0 && q > close)) return stmt;   // message isn't a string
            String pre = fooModuleName + ":" + currentProc + " ... ";   // raw Foo name
            return stmt.substring(0, q + 1) + pre + stmt.substring(q + 1);
        }

        // ---- expressions ----------------------------------------------

        static final Set<Integer> WORD_OPS = Set.of(
            FooLexer.AND, FooLexer.OR, FooLexer.EQV, FooLexer.NEQV,
            FooLexer.EQ, FooLexer.NE, FooLexer.LT_OP, FooLexer.LE_OP,
            FooLexer.GT_OP, FooLexer.GE_OP);

        String renderExpr(FooParser.ExprContext e) {
            if (e == null) return "";
            StringBuilder sb = new StringBuilder();
            for (ParseTree ch : e.children) {
                if (ch instanceof FooParser.PostfixContext)
                    sb.append(translatePostfix((FooParser.PostfixContext) ch, false).text);
                else if (ch instanceof FooParser.BinOpContext)
                    sb.append(renderBinOp((FooParser.BinOpContext) ch));
                else sb.append(ch.getText());
            }
            return applySubst(sb.toString());
        }

        String renderBinOp(FooParser.BinOpContext op) {
            Token t = op.getStart();
            String txt = op.getText();
            return WORD_OPS.contains(t.getType()) ? " " + txt + " " : txt;
        }

        static final class Chain { String text; String fooType; boolean isCall; }

        /** Render a postfix chain with self-dot and generic-call resolution. */
        Chain translatePostfix(FooParser.PostfixContext p, boolean statementPos) {
            Chain ch = new Chain();
            FooParser.HeadContext head = p.head();
            StringBuilder out = new StringBuilder();
            String curType = null;
            String pendingCall = null;           // a `.method`/`MOD:method` awaiting its (args)
            boolean pendingNoRecv = false;       // pendingCall has no receiver (module call)
            boolean isCall = false;

            if (head.callHead() != null) {
                FooParser.CallHeadContext chx = head.callHead();
                boolean hasQual = chx.qualifier() != null;
                boolean colon = chx.COLON() != null, dcolon = chx.DCOLON() != null;
                boolean dot = chx.DOT() != null;
                if (hasQual && (colon || dcolon) && !dot) {
                    // MODULE:method (generic) / MODULE::method (non-generic)
                    String modFoo = chx.qualifier().getText();
                    String method = nameText(chx.name());
                    String[] gi = globals.get(method);
                    if (colon && (!isModuleLikeQualifier(modFoo)
                                  || currentArgs.contains(modFoo) || localVarTypes.containsKey(modFoo))) {
                        // A single ':' whose qualifier is lowercase OR a known
                        // arg/local (e.g. an uppercase loop index L) is not a module
                        // call but an array-section range `lo:hi` that callHead's
                        // qualified-call alternative greedily swallowed.
                        out.append(modFoo).append(':').append(method);
                    } else if (gi != null && gi[1].equals(fortranModName(modFoo))) {
                        // qualified access to a cross-module global variable (e.g.
                        // GAUSSIAN_DATA::spherical_harmonics_for) -> emit unqualified.
                        out.append(method); curType = gi[0];
                        recordUse(gi[1], method);
                    } else {
                        // MODULE:method (generic interface, method_) /
                        // MODULE::method (non-generic, the specific name) — never a
                        // TYPE_-prefixed name.
                        pendingCall = colon ? method + "_" : method;
                        recordUse(fortranModName(modFoo), pendingCall);
                        pendingNoRecv = true; isCall = true;
                    }
                } else if (dot && (colon || dcolon)) {
                    // submodule call on self: .SET:proc / .:proc / .MAIN:proc
                    String method = nameText(chx.name());
                    String ip = !hasQual ? intrinsicProp(method, "self") : null;
                    if (ip != null) {
                        // inlined_by_foo on self: `.:destroyed` -> NOT associated(self)
                        out.append(ip);
                    } else {
                        pendingCall = colon ? method + "_" : method;
                        recordUse(submoduleModule(hasQual ? chx.qualifier().getText() : null), pendingCall);
                        out.append("self"); isCall = true;
                    }
                } else if (dot && chx.name() != null && !hasQual && !colon && !dcolon) {
                    String sel = substSelector(nameText(chx.name()));
                    String ip;
                    String elemComp;
                    if (types.isComponent(selfFooType, sel)) {
                        out.append("self%").append(sel);
                        curType = types.componentType(selfFooType, sel);
                    } else if ((elemComp = selfElemComponent(sel)) != null) {
                        // self is an array VEC{T}; sel is a component of element T
                        out.append("self%").append(sel);
                        curType = elemComp;
                    } else if ((ip = intrinsicProp(sel, "self")) != null) {
                        out.append(ip);                       // .dim -> size(self), etc.
                    } else if (INTRINSIC_FNS.contains(sel.toLowerCase())) {
                        pendingCall = sel;                    // .sin -> sin(self), .trim -> trim(self)
                        out.append("self"); isCall = true;
                    } else {
                        pendingCall = sel + "_"; recordCall(selfFooType, sel);
                        out.append("self"); isCall = true;
                    }
                } else if (!hasQual && (colon || dcolon) && !dot && chx.name() != null) {
                    // same-module reference/call with the qualifier omitted:
                    //   :proc  -> generic name proc_   ;   ::proc -> specific name proc
                    // A following `(args)` trailer makes it a call; bare (e.g. a
                    // procedure passed by name as an argument) stays just the name.
                    String method = nameText(chx.name());
                    String ip = intrinsicProp(method, "self");
                    if (ip != null) {
                        // inlined_by_foo on self, e.g. `.:destroyed` -> NOT associated(self)
                        out.append(ip);
                    } else {
                        out.append(colon ? method + "_" : method);
                        isCall = true;
                    }
                } else if (!hasQual && !colon && !dcolon && !dot && chx.name() != null) {
                    // bare name (local var, `self`, or a cross-module global); track type
                    String nm = nameText(chx.name());
                    out.append(nm);
                    if (nm.equals("self")) curType = selfFooType;
                    else if (localVarTypes.containsKey(nm)) curType = localVarTypes.get(nm);
                    else if (moduleVars.containsKey(nm)) curType = moduleVars.get(nm);
                    else { String gt = resolveGlobal(nm); if (gt != null) curType = gt; }
                } else {
                    out.append(head.getText());          // other forms: TODO
                }
            } else if (head.NOT() != null) {
                out.append("NOT ").append(translatePostfix(head.postfix(), false).text);
            } else if (head.MINUS() != null) {
                out.append('-').append(translatePostfix(head.postfix(), false).text);
            } else if (head.PLUS() != null) {
                out.append('+').append(translatePostfix(head.postfix(), false).text);
            } else if (head.LPAREN() != null) {
                out.append('(')
                   .append(head.argList() != null ? renderArgList(head.argList()) : "")
                   .append(')');
            } else if (head.arrayConstructor() != null) {
                out.append(renderArrayConstructor(head.arrayConstructor()));
            } else {
                out.append(head.getText());              // literal
            }

            for (FooParser.TrailerContext tr : p.trailer()) {
                // A pending method call whose args are NOT a following `(...)` is
                // closed now (e.g. `.x.destroy.something` -> destroy_(self%x) then …).
                if (pendingCall != null && tr.LPAREN() == null) {
                    { String inner = pendingNoRecv ? "" : out.toString();
                  out = new StringBuilder(inner.isEmpty() ? pendingCall : pendingCall + "(" + inner + ")"); }
                    pendingCall = null; pendingNoRecv = false;
                }
                boolean colonTr = (tr.DOT() != null || tr.PERCENT() != null)
                                  && (tr.COLON() != null || tr.DCOLON() != null);
                boolean dotSel = (tr.DOT() != null || tr.PERCENT() != null)
                                 && tr.COLON() == null && tr.DCOLON() == null && !tr.name().isEmpty();
                if (colonTr) {
                    // submodule call: recv.SUBMOD:method / recv.:method
                    List<FooParser.NameContext> ns = tr.name();
                    String submod = ns.size() == 2 ? nameText(ns.get(0)) : null;
                    String method = nameText(ns.get(ns.size() - 1));
                    pendingCall = tr.COLON() != null ? method + "_" : method;
                    String recv = out.toString();
                    if (recv.equals(fortranTypeName(selfFooType))) {
                        // Type-qualified call on self (DIFFRACTION_DATA.READ:proc): the
                        // qualifier names the module, not a receiver object. Pass self
                        // (or nothing, for a selfless target).
                        recordUse(submod == null || submod.equalsIgnoreCase("MAIN")
                                  ? fortranTypeName(selfFooType) + "_MODULE"
                                  : fortranTypeName(selfFooType) + "_" + submod + "_MODULE", pendingCall);
                        if (selflessProcs.contains(method)) { out = new StringBuilder(); pendingNoRecv = true; }
                        else out = new StringBuilder("self");
                    } else if (curType != null) {
                        String base = fortranTypeName(curType);
                        recordUse(submod == null || submod.equalsIgnoreCase("MAIN")
                                  ? base + "_MODULE" : base + "_" + submod + "_MODULE", pendingCall);
                    }
                    isCall = true; curType = null;          // recv stays in `out`; args via next LPAREN
                } else if (dotSel) {
                    String sel = substSelector(nameText(tr.name(0)));
                    String ip, elemComp;
                    if (curType != null && types.isComponent(curType, sel)) {
                        out.append('%').append(sel);
                        curType = types.componentType(curType, sel);
                    } else if ((elemComp = elemComponent(curType, sel)) != null) {
                        // recv is an array VEC{T}; sel is a component of element T
                        out.append('%').append(sel);
                        curType = elemComp;
                    } else if ((ip = intrinsicProp(sel, out.toString())) != null) {
                        out = new StringBuilder(ip); curType = null;
                    } else if (INTRINSIC_FNS.contains(sel.toLowerCase())) {
                        // tonto intrinsic: X.scan(s) -> scan(X,s), X.trim -> trim(X)
                        pendingCall = sel; isCall = true; curType = null;
                    } else {
                        // method call on `out`: defer wrapping so a following
                        // `(args)` trailer is folded in -> sel_(out, args)
                        recordCall(curType, sel);
                        pendingCall = sel + "_"; isCall = true; curType = null;
                    }
                } else if (tr.LPAREN() != null) {
                    String inner = tr.argList() != null ? renderArgList(tr.argList()) : "";
                    if (pendingCall != null) {
                        String recv = out.toString();
                        String args = pendingNoRecv ? inner
                            : (recv.isEmpty() ? inner : (inner.isEmpty() ? recv : recv + "," + inner));
                        out = new StringBuilder(pendingCall + "(" + args + ")");
                        pendingCall = null; pendingNoRecv = false;
                    } else {
                        out.append('(').append(inner).append(')');
                        // A single subscript x(i) yields the element type. A section or
                        // substring x(a:b) / x(i,:) keeps the array/string type, so a
                        // following `.method` still resolves to the right module
                        // (self(2:).foo stays VEC{INT} -> VEC_INT_MODULE, not INT_MODULE;
                        // s(a+1:).foo stays STR).
                        if (!inner.contains(":")) {
                            ArrayType at = curType != null ? parseArray(canon(curType)) : null;
                            curType = at != null ? at.elem : null;
                        }
                    }
                } else if (tr.LBRACKET() != null) {
                    // encapsulated-element access: a(i)[j] -> a(i)%element(j)
                    String inner = tr.argList() != null ? renderArgList(tr.argList()) : "";
                    out.append("%element(").append(inner).append(')');
                    // `element` is an array component; indexing it yields its elem type
                    if (curType != null && types.isComponent(curType, "element")) {
                        ArrayType at = parseArray(canon(types.componentType(curType, "element")));
                        curType = at != null ? at.elem : null;
                    } else curType = null;
                } else {
                    out.append(tr.getText());
                }
            }
            if (pendingCall != null)
                { String inner = pendingNoRecv ? "" : out.toString();
                  out = new StringBuilder(inner.isEmpty() ? pendingCall : pendingCall + "(" + inner + ")"); }

            ch.fooType = curType; ch.isCall = isCall;
            String s = out.toString();
            if (statementPos && isCall) s = "call " + s;
            ch.text = s;
            return ch;
        }

        /** `[a, b, (expr, i=1,n)]` with each element translated. Keeps the
         *  bracket form foo.pl emits. */
        String renderArrayConstructor(FooParser.ArrayConstructorContext ac) {
            List<String> parts = new ArrayList<>();
            for (FooParser.AcElemContext e : ac.acElem()) {
                if (e.loopHeader() != null)          // implied-do: (expr, i=1,n)
                    parts.add("(" + renderExpr(e.expr()) + "," + renderLoopHeader(e.loopHeader()) + ")");
                else
                    parts.add(renderExpr(e.expr()));
            }
            return "[" + String.join(",", parts) + "]";
        }

        String renderArgList(FooParser.ArgListContext al) {
            List<String> parts = new ArrayList<>();
            for (FooParser.ArgContext a : al.arg()) parts.add(renderArg(a));
            return String.join(",", parts);
        }

        String renderArg(FooParser.ArgContext a) {
            if (a.name() != null && a.EQUAL() != null)          // keyword arg
                return nameText(a.name()) + "=" + (a.expr(0) != null ? renderExpr(a.expr(0)) : "*");
            if (a.expr() != null && !a.expr().isEmpty()) {
                StringBuilder sb = new StringBuilder(renderExpr(a.expr(0)));
                // array section a:b:c
                for (int i = 1; i < a.expr().size(); i++) sb.append(':').append(renderExpr(a.expr(i)));
                if (a.expr().size() == 1 && a.COLON() != null && !a.COLON().isEmpty())
                    sb.append(':');
                return sb.toString();
            }
            return a.getText();      // '*' / ':' forms — passthrough
        }

        /** True if a qualifier before ':' names a module/type (uppercase base, per
         *  Foo convention) rather than a lower bound in an array-section range. */
        boolean isModuleLikeQualifier(String q) {
            int i = 0;
            while (i < q.length() && q.charAt(i) != '{' && q.charAt(i) != '(') i++;
            String base = q.substring(0, i);
            boolean hasLetter = false;
            for (int k = 0; k < base.length(); k++) {
                char c = base.charAt(k);
                if (Character.isLetter(c)) {
                    hasLetter = true;
                    if (Character.isLowerCase(c)) return false;
                }
            }
            return hasLetter;
        }

        /** When self is an array VEC{T}/MAT{T}…, the component type of `sel` on the
         *  element type T (so `self.charge` on a VEC{ATOM} -> self%charge), else null. */
        String selfElemComponent(String sel) { return elemComponent(selfFooType, sel); }

        String elemComponent(String type, String sel) {
            if (type == null) return null;
            ArrayType at = parseArray(canon(type));
            if (at != null && at.elem != null && types.isComponent(at.elem, sel))
                return types.componentType(at.elem, sel);
            return null;
        }

        /** Tonto intrinsic functions (foo.pl @tonto_intrinsic_functions): a
         *  `.method(args)` on them drops the `_` and passes the receiver as the
         *  first arg — X.sin -> sin(X), X.scan(s) -> scan(X,s), X.trim -> trim(X).
         *  The argumentless inquiry/pointer ones (size/dim, allocated, associated,
         *  created, destroyed, …) are handled by intrinsicProp instead. */
        static final Set<String> INTRINSIC_FNS = Set.of(
            "abs", "acos", "asin", "atan", "cos", "sin", "tan",
            "mod", "modulo", "scan", "trim", "verify", "nullify");

        /** Array/pointer inquiry methods that map to Fortran intrinsics, or null. */
        String intrinsicProp(String name, String recv) {
            switch (name) {
                case "dim": return "size(" + recv + ")";
                case "dim1": return "size(" + recv + ",1)";
                case "dim2": return "size(" + recv + ",2)";
                case "dim3": return "size(" + recv + ",3)";
                case "dim4": return "size(" + recv + ",4)";
                case "dim5": return "size(" + recv + ",5)";
                case "dim6": return "size(" + recv + ",6)";
                case "dim7": return "size(" + recv + ",7)";
                // NB: .trim / .scan are NOT intrinsics here — a `.proc` always
                // resolves to a `proc_` call (an explicit interface in STR, e.g.
                // trim_ -> trim_blanks_from_end), even when the name coincides
                // with a Fortran intrinsic. So they fall through to method calls.
                case "allocated": return "allocated(" + recv + ")";
                case "deallocated": return "NOT allocated(" + recv + ")";
                case "created":                                 // pointer create/destroy
                case "associated": return "associated(" + recv + ")";
                case "destroyed":                               // inlined_by_foo: .destroyed
                case "disassociated": return "NOT associated(" + recv + ")";
                default: return null;
            }
        }

        void recordCall(String fooType, String method) {
            if (fooType == null) return;
            recordUse(fortranModName(fooType), method + "_");
        }

        /** Fortran module for a submodule qualifier on self: .SET->MOLECULE_SET_MODULE,
         *  .MAIN->MOLECULE_MODULE, .: (null) -> the current module (same submodule). */
        String submoduleModule(String qual) {
            String base = fortranTypeName(selfFooType);
            if (qual == null) return selfModuleName;
            if (qual.equalsIgnoreCase("MAIN")) return base + "_MODULE";
            return base + "_" + qual + "_MODULE";
        }

        /** Record a `use <mod>, only: <symbol>` dependency (skip self-use). */
        void recordUse(String fortranMod, String symbol) {
            if (fortranMod.equals(selfModuleName)) return;        // don't use own module
            // TYPES and SYSTEM are pulled in wholesale (`use X_MODULE`), so they
            // never get an `only:` clause (matches foo.pl).
            if (fortranMod.equals("TYPES_MODULE") || fortranMod.equals("SYSTEM_MODULE")) return;
            useOnly.computeIfAbsent(fortranMod, k -> new java.util.TreeSet<>()).add(symbol);
        }

        /** If `nm` is a cross-module global, set curType to its type, record the
         *  `only:` use dependency, and return true. */
        String resolveGlobal(String nm) {
            String[] gi = globals.get(nm);
            if (gi == null) return null;
            recordUse(gi[1], nm);
            return gi[0];
        }

        // ---- .int / .use ----------------------------------------------

        void buildInterfaceFile() {
            // foo.pl: the interface list defaults to `private`, EXCEPT for the TYPES
            // module, which is `public` so that every derived type it declares is
            // exported to all modules that `use` it. A bare `private` here would make
            // all types private and nothing could reference type(SYSTEM_TYPE) etc.
            intf.append(fooModuleName.equalsIgnoreCase("TYPES") ? "   public\n\n" : "   private\n\n");
            // foo.pl sorts the interface blocks by the emitted name (NAME_),
            // strict ASCII order (LC_ALL=C: uppercase before lowercase).
            List<Map.Entry<String, List<String>>> entries =
                new ArrayList<>(interfaceProcs.entrySet());
            entries.sort((a, b) -> (a.getKey() + "_").compareTo(b.getKey() + "_"));
            for (Map.Entry<String, List<String>> e : entries) {
                intf.append("   public    ").append(e.getKey()).append("_\n");
                intf.append("   interface ").append(e.getKey()).append("_\n");
                for (String spec : e.getValue())
                    intf.append("      module procedure ").append(spec).append('\n');
                intf.append("   end interface\n\n");
            }
        }

        void buildUseFile() {
            String selfMod = fortranTypeName(fooModuleName);
            if (!selfMod.equals("TYPES")) use.append("   use TYPES_MODULE\n");
            if (!selfMod.equals("SYSTEM") && !selfMod.equals("TYPES"))
                use.append("   use SYSTEM_MODULE\n");
            // one line per (module, symbol), sorted (matches foo.pl)
            for (Map.Entry<String, Set<String>> e : useOnly.entrySet())
                for (String sym : e.getValue())
                    use.append("   use ").append(e.getKey()).append(", only: ").append(sym).append('\n');
        }

        // ---- attr signature comment helper (for get_from matching) -----
        // set per-proc before resolving inheritance
    }

    // ----------------------------------------------------- hidden-token cursor

    /** Emits hidden tokens (preprocessor lines + comments) and blank lines. */
    static final class Cursor {
        final CommonTokenStream toks;
        int pos = 0;
        int lastLine = -1;
        Cursor(CommonTokenStream toks) { this.toks = toks; }

        void flushHidden(StringBuilder out, int uptoTokenIndex, int defaultIndent) {
            for (; pos < uptoTokenIndex && pos < toks.size(); pos++) {
                Token t = toks.get(pos);
                if (t.getChannel() != Token.HIDDEN_CHANNEL) continue;
                int ty = t.getType();
                if (ty != FooLexer.COMMENT && ty != FooLexer.PP_LINE) continue;
                if (lastLine >= 0 && t.getLine() > lastLine + 1) out.append('\n');  // one blank max
                int col = t.getCharPositionInLine();
                for (int s = 0; s < col; s++) out.append(' ');
                out.append(t.getText()).append('\n');
                lastLine = t.getLine();
            }
        }
    }

    // ---------------------------------------------------------- get_from ref

    static final class ParentRef {
        String module, routine;
        static ParentRef parse(String target, String selfModule) {
            ParentRef r = new ParentRef();
            String s = target.replaceAll("\\s+", "");
            int colon = s.indexOf(':');
            if (colon >= 0) { r.module = s.substring(0, colon); r.routine = s.substring(colon + 1); }
            else if (s.matches("[A-Z].*")) { r.module = s; r.routine = null; }
            else { r.module = selfModule; r.routine = s; }
            return r;
        }
    }

    // ----------------------------------------------------- attribute parsing

    static final class Attrs {
        boolean pure, PURE, elemental, ELEMENTAL, recursive, leaky, selfless, routinal, functional;
        boolean privateAcc, publicAcc, template, inherited;
        String getFromTarget, signatureComment;
        FooParser.AttrContext getFromAttr;

        static Attrs parse(FooParser.ProcAttrsContext pa) {
            Attrs a = new Attrs();
            if (pa == null) return a;
            for (FooParser.AttrContext at : pa.attrList().attr()) {
                if (at.GET_FROM() != null) {
                    a.inherited = true;
                    a.getFromTarget = at.getFromArg(0).getText();
                    a.getFromAttr = at;
                    continue;
                }
                String raw = at.getText();
                switch (raw.toLowerCase(Locale.ROOT)) {
                    case "pure":      if (raw.equals("PURE")) a.PURE = true; else a.pure = true; break;
                    case "elemental": if (raw.equals("ELEMENTAL")) a.ELEMENTAL = true; else a.elemental = true; break;
                    case "recursive": a.recursive = true; break;
                    case "leaky":     a.leaky = true; break;
                    case "selfless":  a.selfless = true; break;
                    case "routinal":  a.routinal = true; break;   // self is a subroutine arg (interface)
                    case "functional": a.functional = true; break; // self is a function arg (interface)
                    case "private":   a.privateAcc = true; break;
                    case "public":    a.publicAcc = true; break;
                    case "template":  a.template = true; break;
                    case "inlined_by_foo": a.template = true; break;  // inlined at call sites, not emitted
                    default: break;
                }
            }
            return a;
        }

        String prefix() {
            StringBuilder p = new StringBuilder();
            if (elemental) p.append("elemental");
            else if (ELEMENTAL) p.append("ELEMENTAL");
            else if (pure) p.append("pure");
            else if (PURE) p.append("PURE");
            if (recursive) { if (p.length() > 0) p.append(' '); p.append("recursive"); }
            return p.length() == 0 ? null : p.toString();
        }
    }

    // ------------------------------------------------------- tree utilities

    @SuppressWarnings("unchecked")
    static <T extends ParserRuleContext> List<T> descendants(ParseTree root, Class<T> cls) {
        List<T> out = new ArrayList<>(); collect(root, cls, out); return out;
    }
    private static <T extends ParserRuleContext> void collect(ParseTree node, Class<T> cls, List<T> out) {
        if (cls.isInstance(node)) out.add((T) node);
        for (int i = 0; i < node.getChildCount(); i++) collect(node.getChild(i), cls, out);
    }

    /** Token index just after the module's CONTAINS keyword (or module start). */
    static int containsTokenIndex(FooParser.ModuleDefContext mod) {
        if (mod.CONTAINS() != null) return mod.CONTAINS().getSymbol().getTokenIndex() + 1;
        return mod.getStart().getTokenIndex();
    }

    static String oneLine(String s) { return s.replaceAll("\\s+", " ").trim(); }

    static String sp(int n) { StringBuilder b = new StringBuilder(); for (int i = 0; i < n; i++) b.append(' '); return b.toString(); }

    /** Split on top-level commas (ignoring those inside (), {}, []). */
    static List<String> splitTopComma(String s) {
        List<String> out = new ArrayList<>();
        int depth = 0, start = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
            if (ch == '(' || ch == '{' || ch == '[') depth++;
            else if (ch == ')' || ch == '}' || ch == ']') depth--;
            else if (ch == ',' && depth == 0) { out.add(s.substring(start, i)); start = i + 1; }
        }
        out.add(s.substring(start));
        return out;
    }

    static String repeatColon(int n) {
        StringBuilder b = new StringBuilder();
        for (int i = 0; i < n; i++) { if (i > 0) b.append(','); b.append(':'); }
        return b.toString();
    }

    /** Outermost type arguments of a parameterised type: VEC{INTRINSIC} -> [INTRINSIC]. */
    static List<String> typeArgsOf(String t) {
        t = canon(t);
        int b = t.indexOf('{');
        if (b < 0) return new ArrayList<>();
        int depth = 0, end = -1;
        for (int j = b; j < t.length(); j++) {
            char ch = t.charAt(j);
            if (ch == '{') depth++;
            else if (ch == '}') { if (--depth == 0) { end = j; break; } }
        }
        if (end < 0) return new ArrayList<>();
        List<String> out = new ArrayList<>();
        String inner = t.substring(b + 1, end);
        int d = 0, start = 0;
        for (int j = 0; j <= inner.length(); j++) {
            if (j == inner.length() || (inner.charAt(j) == ',' && d == 0)) {
                out.add(inner.substring(start, j)); start = j + 1;
            } else if (inner.charAt(j) == '{') d++;
            else if (inner.charAt(j) == '}') d--;
        }
        return out;
    }

    /** A parsed array type: head (VEC/MAT..MAT7), rank, element type, optional dims. */
    static final class ArrayType { String head; int ndim; String elem; String dimSpec; }

    private static final String[] ARRAY_HEADS = {"MAT7","MAT6","MAT5","MAT4","MAT3","MAT","VEC"};
    private static final int[]    ARRAY_NDIM  = {7, 6, 5, 4, 3, 2, 1};

    /** Parse `VEC{REAL}`, `MAT{REAL}(1:3,1:4)`, `VEC{EVEC{INT}}` … or null. */
    static ArrayType parseArray(String c) {
        for (int i = 0; i < ARRAY_HEADS.length; i++) {
            String h = ARRAY_HEADS[i];
            if (!c.startsWith(h) || c.length() <= h.length() || c.charAt(h.length()) != '{') continue;
            int depth = 0, close = -1;
            for (int j = h.length(); j < c.length(); j++) {
                char ch = c.charAt(j);
                if (ch == '{') depth++;
                else if (ch == '}') { if (--depth == 0) { close = j; break; } }
            }
            if (close < 0) return null;
            ArrayType at = new ArrayType();
            at.head = h; at.ndim = ARRAY_NDIM[i];
            at.elem = c.substring(h.length() + 1, close);
            String rest = c.substring(close + 1);
            if (rest.startsWith("(") && rest.endsWith(")")) at.dimSpec = rest.substring(1, rest.length() - 1);
            return at;
        }
        return null;
    }

    private FooToFortran() {}
}
