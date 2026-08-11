# `scripts/` — developer and CI scripts

**Nothing here is installed.** These are run from the repository, by hand or by
a workflow: the test harness, the invariant checks, the translator helper, the
lints, the doctors and their self-tests, and `docker/`, which proves the RGBI
install list in `docs/INSTALLING_RGBI.md` is sufficient by building it from a
bare `ubuntu:24.04`.

Programs that ship to users live in [`../rgbi-scripts/`](../rgbi-scripts) —
that is the distinction between the two directories, not the subject matter.

One exception, and it is an inconsistency rather than a rule: `rgbi_doctor.sh`
*is* installed, into `bin`, extension and all (`CMakeLists.txt`). A user-facing
command should be named like one — see `DEFERRED.md`.
