#!/usr/bin/env python3
"""Write the files for a ChimeraX panel of the F_exp maps of one molecule.

usage: make_panel.py <job name> [directory]

Reads <name>_f_exp_l1.cube in the directory (default: the current one) for
the atoms, writes <name>_molecule.pdb, and writes <name>_panel.cxc, which
opens the eleven F_exp maps (l = 0; l = 1 norm and m = -1, 0, 1; l = 2 norm
and m = -2 to 2), each with the molecule, as groups #11 to #21, trims each
surface to 2.2 A from the atoms, and tiles them with one camera. Open it
in ChimeraX, rotate, type "tile" to lay the grid out again, set the levels,
then "view name mine" and save the session; render_panel.py renders it.
"""
import os, sys

name = sys.argv[1]
d = sys.argv[2] if len(sys.argv) > 2 else "."
prefix = sys.argv[3] if len(sys.argv) > 3 else "f_exp"
os.chdir(d)
sym = {1: "H", 6: "C", 7: "N", 8: "O", 16: "S"}
bohr = 0.529177210903

lines = open("%s_%s_l1.cube" % (name, prefix)).readlines()
n = int(lines[2].split()[0])
pdb = ["REMARK   1 %s, one molecule, from the cube header\n" % name]
for i, l in enumerate(lines[6:6 + n]):
    z = int(l.split()[0]); x, y, zc = [float(v) * bohr for v in l.split()[2:5]]
    el = sym[z]
    pdb.append("HETATM%5d %-4s MOL A   1    %8.3f%8.3f%8.3f%6.2f%6.2f          %2s\n"
               % (i + 1, el + str(i + 1), x, y, zc, 1.0, 0.0, el))
pdb.append("END\n")
open("%s_molecule.pdb" % name, "w").write("".join(pdb))

maps = [prefix + m for m in ["_l0", "_l1", "_l1_m-1", "_l1_m0", "_l1_m1",
        "_l2", "_l2_m-2", "_l2_m-1", "_l2_m0", "_l2_m1", "_l2_m2"]]
cxc_name = "%s_panel.cxc" % name if prefix == "f_exp" else "%s_%s_panel.cxc" % (name, prefix)
g = 11
out = ["# ChimeraX: the eleven %s %s maps, each with the molecule, tiled in one scene" % (name, prefix),
       "# so that one view serves all. Rotate and zoom the whole grid; then type \"tile\" to lay",
       "# the maps out flat again in the new orientation; repeat until the view is right; then",
       "#    view name mine",
       "#    (the f_exp panel saves itself as panel.cxs)",
       "# Levels: \"volume #11-21 level -0.1 level 0.1\" for the components; \"volume #11,12,16",
       "# level 0.1\" for the norms. Each surface is trimmed to 2.2 A from its molecule's atoms",
       "# (\"volume zone\"); \"volume unzone #11-21\" shows the whole box again.",
       "set bgColor white", "lighting soft", "graphics silhouettes true"]
for i, m in enumerate(maps):
    out += ["open %s_molecule.pdb" % name, "open %s_%s.cube" % (name, m),
            "rename #1,2 id #%d name %s" % (g + i, m)]
out += ["style stick", "size stickRadius 0.12"]
norms = [g + i for i, m in enumerate(maps) if "_m" not in m]
comps = [g + i for i, m in enumerate(maps) if "_m" in m]
out.append("volume #%s style surface step 1 level 0.1 color #e8a33d transparency 0.25" % ",".join(map(str, norms)))
out.append("volume #%s style surface step 1 level -0.1 color #4a90d9 level 0.1 color #e8a33d transparency 0.25" % ",".join(map(str, comps)))
out += ["volume zone #%d.2 nearAtoms #%d.1 range 2.2" % (k, k) for k in range(g, g + len(maps))]
out += ["tile #%d-%d columns 4 spacingFactor 1.3" % (g, g + len(maps) - 1), "view"]
if prefix == "f_exp":
    out.append("open ../autosave.py")
open(cxc_name, "w").write("\n".join(out) + "\n")
print("wrote %s_molecule.pdb (%d atoms) and %s" % (name, n, cxc_name))
