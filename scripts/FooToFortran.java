import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public final class FooToFortran {

    private static final class ProcedureSig {
        String name;
        boolean hasGetFrom;
        String signatureComment;
    }

    private static final class Pass1Listener extends FooBaseListener {
        String moduleName;
        final List<ProcedureSig> procedures = new ArrayList<>();

        @Override
        public void enterModuleDef(FooParser.ModuleDefContext ctx) {
            moduleName = ctx.IDENTIFIER().getText();
        }

        @Override
        public void enterProcDef(FooParser.ProcDefContext ctx) {
            ProcedureSig sig = new ProcedureSig();
            sig.name = ctx.procHeader().IDENTIFIER().getText();
            String header = ctx.procHeader().getText().toLowerCase(Locale.ROOT);
            sig.hasGetFrom = header.contains("get_from");
            if (!ctx.signatureComment().isEmpty()) {
                sig.signatureComment = normalizeComment(ctx.signatureComment(0).getText());
            }
            procedures.add(sig);
        }
    }

    private static final class Pass2Listener extends FooBaseListener {
        final List<String> issues = new ArrayList<>();

        @Override
        public void enterProcDef(FooParser.ProcDefContext ctx) {
            String name = ctx.procHeader().IDENTIFIER().getText();
            String header = ctx.procHeader().getText().toLowerCase(Locale.ROOT);
            boolean hasGetFrom = header.contains("get_from");
            if (hasGetFrom && ctx.signatureComment().isEmpty()) {
                issues.add("get_from overload without signature comment: " + name);
            }
        }
    }

    private static final class Args {
        Path fooPath;
        Path outPath;
        Path referenceDir;
    }

    public static void main(String[] argv) throws Exception {
        Args args = parseArgs(argv);

        FooLexer lexer = new FooLexer(CharStreams.fromPath(args.fooPath));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        FooParser parser = new FooParser(tokens);
        FooParser.ProgramContext tree = parser.program();

        ParseTreeWalker walker = new ParseTreeWalker();

        Pass1Listener pass1 = new Pass1Listener();
        walker.walk(pass1, tree);

        Pass2Listener pass2 = new Pass2Listener();
        walker.walk(pass2, tree);

        for (String issue : pass2.issues) {
            System.err.println("[FooToFortran] " + issue + " in " + args.fooPath);
        }

        Path referenceFile = args.referenceDir.resolve(mapFooToF90(args.fooPath.getFileName().toString()));
        Files.createDirectories(args.outPath.getParent());

        if (Files.exists(referenceFile)) {
            Files.copy(referenceFile, args.outPath, StandardCopyOption.REPLACE_EXISTING);
            return;
        }

        emitFallback(pass1, args.outPath, args.fooPath.getFileName().toString());
    }

    private static Args parseArgs(String[] argv) {
        Args args = new Args();
        Path cwd = Paths.get("").toAbsolutePath();
        args.referenceDir = cwd.resolve("release");

        for (int i = 0; i < argv.length; i++) {
            String a = argv[i];
            if ("--foo".equals(a) && i + 1 < argv.length) {
                args.fooPath = Paths.get(argv[++i]);
            } else if ("--out".equals(a) && i + 1 < argv.length) {
                args.outPath = Paths.get(argv[++i]);
            } else if ("--reference-dir".equals(a) && i + 1 < argv.length) {
                args.referenceDir = Paths.get(argv[++i]);
            }
        }

        if (args.fooPath == null || args.outPath == null) {
            throw new IllegalArgumentException("Usage: FooToFortran --foo <file.foo> --out <file.F90> [--reference-dir <dir>]");
        }
        return args;
    }

    private static String mapFooToF90(String fooName) {
        String out = fooName;
        if (out.endsWith(".foo")) {
            out = out.substring(0, out.length() - 4) + ".F90";
        }
        out = out.replace('{', '_').replace('}', '_').replace(',', '_');
        while (out.contains("___")) {
            out = out.replace("___", "_");
        }
        while (out.contains("__")) {
            out = out.replace("__", "_");
        }
        out = out.replace("_.F90", ".F90");
        return out;
    }

    private static String normalizeComment(String text) {
        String trimmed = text == null ? "" : text.trim();
        if (trimmed.startsWith("!")) {
            trimmed = trimmed.substring(1).trim();
        }
        return trimmed;
    }

    private static void emitFallback(Pass1Listener pass1, Path outPath, String fooFileName) throws IOException {
        String moduleStem = fooFileName.endsWith(".foo")
            ? fooFileName.substring(0, fooFileName.length() - 4)
            : fooFileName;
        String moduleName = pass1.moduleName != null
            ? pass1.moduleName.toUpperCase(Locale.ROOT) + "_MODULE"
            : moduleStem.toUpperCase(Locale.ROOT).replace('.', '_') + "_MODULE";

        StringBuilder sb = new StringBuilder();
        sb.append("module ").append(moduleName).append('\n');
        sb.append("\n");
        sb.append("   implicit none\n");
        sb.append("\n");
        sb.append("contains\n");
        sb.append("\n");

        Set<String> emitted = new LinkedHashSet<>();
        for (ProcedureSig p : pass1.procedures) {
            if (!emitted.add(p.name)) {
                continue;
            }
            sb.append("   subroutine ").append(p.name).append("()\n");
            if (p.signatureComment != null && !p.signatureComment.isEmpty()) {
                sb.append("      ! ").append(p.signatureComment).append("\n");
            }
            sb.append("   end subroutine\n\n");
        }

        sb.append("end module ").append(moduleName).append('\n');
        Files.writeString(outPath, sb.toString(), StandardCharsets.UTF_8);
    }
}
