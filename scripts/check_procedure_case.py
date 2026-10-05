#!/usr/bin/env python3
"""Check that every procedure name is spelt with one case, in the .foo sources.

Foo and Fortran do not care about case, so `.reset_io_status` calls
`reset_IO_status` and compiles. Case-sensitive tooling does care: the
dead-code analysis once pruned a procedure because its call site was spelt
differently from its definition. So each name has one spelling: the
definition's, with abbreviations in capitals (UC, SCF, DFT, FT, MO, ANO, ...).

The check collects, per procedure name, the spellings at its definition
headers (3-space indent, after `contains`) and at every call site (`.name`,
`:name`, `::name`), outside comments and strings, and reports a name with
more than one. A spelling that is also a type component in types.foo is a
field access, not a call, and is allowed beside the procedure's spelling:
ATOM's field `f_r` and REFLECTION's procedure `F_r` are different things.
Names listed in IGNORED are exempt: the ENSURE/DIE/WARN macros share their
names with lower-case SYSTEM procedures, `TEST?` and `CUTOFF?` are template
placeholders, and `delta`/`Delta` are two unrelated procedures in different
modules.

Usage: check_procedure_case.py <directory> [<directory> ...]
Prints each problem as the name with its spellings and exits 1 if there are any.
"""
import collections, glob, os, re, sys

IGNORED = {"die", "die_if", "ensure", "warn", "warn_if", "test", "delta", "bonded"}

HEADER = re.compile(r"^   ([A-Za-z_][A-Za-z0-9_]*)\s*(\(|::|result\b|$)")
NOT_A_HEADER = re.compile(r"^   (end|if|do|select|case|else|interface|type|module|use|implicit|include)\b", re.I)
CALL = re.compile(r"(?<![A-Za-z0-9_])(?:\.|::|:)([A-Za-z_][A-Za-z0-9_]*)")
FIELD = re.compile(r"^     ([A-Za-z_][A-Za-z0-9_]*)\s*::")


def code_only(line):
    """The line without its comment and with its strings blanked."""
    out, i, quote = [], 0, None
    while i < len(line):
        ch = line[i]
        if quote:
            if ch == quote:
                quote = None
            i += 1
            continue
        if ch in "\"'":
            quote = ch
            i += 1
            continue
        if ch == "!":
            break
        out.append(ch)
        i += 1
    return "".join(out)


def scan(directories):
    spellings = collections.defaultdict(collections.Counter)   # lower name -> spelling -> count
    where = collections.defaultdict(dict)                      # lower name -> spelling -> first site
    defined, fields = set(), set()
    for d in directories:
        for path in sorted(glob.glob(os.path.join(d, "*.foo"))):
            in_body = False
            is_types = os.path.basename(path) == "types.foo"
            for n, raw in enumerate(open(path, errors="replace"), 1):
                code = code_only(raw.rstrip("\n"))
                if is_types:
                    m = FIELD.match(code)
                    if m:
                        fields.add(m.group(1))
                if re.match(r"^\s*contains\s*$", code):
                    in_body = True
                    continue
                names = []
                if in_body:
                    m = HEADER.match(code)
                    if m and not NOT_A_HEADER.match(code):
                        names.append(m.group(1))
                        defined.add(m.group(1).lower())
                names += CALL.findall(code)
                for name in names:
                    low = name.lower()
                    spellings[low][name] += 1
                    where[low].setdefault(name, f"{os.path.basename(path)}:{n}")
    return spellings, where, defined, fields


def main(directories):
    spellings, where, defined, fields = scan(directories)
    problems = 0
    for low in sorted(defined):
        if low in IGNORED:
            continue
        variants_seen = {s for s in spellings[low] if s not in fields}
        if len(variants_seen) < 2:
            continue
        problems += 1
        variants = ", ".join(f"{s} x{k} (first at {where[low][s]})"
                             for s, k in spellings[low].most_common())
        print(f"{low}: {variants}")
    print(f"{problems} procedure name(s) with more than one spelling")
    return 1 if problems else 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(2)
    sys.exit(main(sys.argv[1:]))
