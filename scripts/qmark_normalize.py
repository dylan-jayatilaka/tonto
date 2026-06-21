#!/usr/bin/env python3
"""Normalise get_from substitution placeholders to a trailing '?'.

For every UPPERCASE substitution key K (except EQ) that lacks a '?':
  - invoking side : `K=>`  -> `K?=>`  (on get_from lines)
  - template side : within the inherited procedure's body in the virtual module,
                    every whole-word `K` -> `K?`
Lowercase keys and EQ are left untouched.  Per-procedure scoped on the template.
Run with --apply to write changes; default is dry-run.
"""
import re, glob, os, sys

FOO = os.path.join(os.path.dirname(__file__), '..', 'foofiles')
APPLY = '--apply' in sys.argv
SKIP = {'EQ'}

def is_key(k):
    return k.isupper() and k not in SKIP and re.fullmatch(r'[A-Z][A-Z0-9_]*', k)

key_re   = re.compile(r'\b([A-Za-z_][A-Za-z0-9_]*)=>')      # KEY=> (no '?')
gf_tgt   = re.compile(r'get_from\(\s*([^,()]+(?:\{[^}]*\})?[^,()]*)')  # first arg of get_from

# (template_file, proc_name) -> set(keys)
tmpl_keys = {}

files = sorted(glob.glob(os.path.join(FOO, '*.foo')))

# ---- Pass 1: collect mapping from invoking get_from lines --------------------
for path in files:
    base = os.path.basename(path)
    for line in open(path, encoding='utf-8', errors='replace'):
        if 'get_from(' not in line:
            continue
        # invoking procedure name = first identifier on the line
        m = re.match(r'\s*([A-Za-z_]\w*)', line)
        inv_proc = m.group(1) if m else None
        # get_from target (module[:proc] or same-module proc)
        mt = gf_tgt.search(line)
        if not mt:
            continue
        target = mt.group(1).strip()
        # split MODULE:proc
        if ':' in target:
            mod, _, tproc = target.partition(':')
            mod, tproc = mod.strip(), tproc.strip()
        else:
            mod, tproc = target, None
        # keys on this line (uppercase, no '?', not EQ)
        keys = {k for k in key_re.findall(line) if is_key(k)}
        if not keys:
            continue
        # resolve template file + proc
        if mod and mod[0].isupper():                 # uppercase module name
            tfile = os.path.join(FOO, mod.lower() + '.foo')
            proc = tproc or inv_proc
        else:                                        # lowercase -> same module proc
            tfile = path
            proc = mod or tproc or inv_proc
        if proc:
            tmpl_keys.setdefault((tfile, proc), set()).update(keys)

# ---- Pass 2: invoking-side edit (KEY=> -> KEY?=> on get_from lines) ----------
def fix_invoke(line):
    if 'get_from(' not in line:
        return line
    return key_re.sub(lambda mo: (mo.group(1) + '?=>') if is_key(mo.group(1))
                                  else mo.group(0), line)

inv_changes = 0
inv_files = set()
for path in files:
    lines = open(path, encoding='utf-8', errors='replace').readlines()
    out = []
    changed = False
    for ln in lines:
        nl = fix_invoke(ln)
        if nl != ln:
            changed = True; inv_changes += 1
        out.append(nl)
    if changed:
        inv_files.add(os.path.basename(path))
        if APPLY:
            open(path, 'w', encoding='utf-8').writelines(out)

# ---- Pass 3: template-side edit (per procedure body) ------------------------
def proc_body_range(lines, proc):
    """Return list of (start,end) line index ranges for procedure `proc` bodies
    (header at 3-space indent: '   proc(' / '   proc ' / '   proc:::' ), body to
    the matching 3-space 'end'."""
    ranges = []
    hdr = re.compile(r'^   ' + re.escape(proc) + r'(\b|\()')
    i = 0
    n = len(lines)
    while i < n:
        if hdr.match(lines[i]):
            j = i + 1
            while j < n and not re.match(r'^   end\b', lines[j]):
                j += 1
            ranges.append((i, j))
            i = j + 1
        else:
            i += 1
    return ranges

tmpl_changes = 0
tmpl_detail = []
# group by file
by_file = {}
for (tfile, proc), keys in tmpl_keys.items():
    by_file.setdefault(tfile, []).append((proc, keys))

for tfile, items in by_file.items():
    if not os.path.exists(tfile):
        continue
    lines = open(tfile, encoding='utf-8', errors='replace').readlines()
    filechanged = False
    for proc, keys in items:
        for (s, e) in proc_body_range(lines, proc):
            for bi in range(s, e + 1):
                orig = lines[bi]
                new = orig
                for k in sorted(keys, key=len, reverse=True):
                    new = re.sub(r'\b' + k + r'\b(?!\?)', k + '?', new)
                if new != orig:
                    lines[bi] = new; tmpl_changes += 1; filechanged = True
        tmpl_detail.append((os.path.basename(tfile), proc, sorted(keys)))
    if filechanged and APPLY:
        open(tfile, 'w', encoding='utf-8').writelines(lines)

# ---- Report -----------------------------------------------------------------
print(f"MODE: {'APPLY' if APPLY else 'DRY-RUN'}")
print(f"Invoking-side edits: {inv_changes} occurrences in {len(inv_files)} files")
print("  invoking files:", ', '.join(sorted(inv_files)))
print(f"Template-side edits: {tmpl_changes} line-changes")
print("  (template-file, procedure, keys):")
seen = set()
for d in sorted(set((a, b, tuple(c)) for a, b, c in tmpl_detail)):
    print(f"    {d[0]:24} {d[1]:28} {' '.join(d[2])}")
