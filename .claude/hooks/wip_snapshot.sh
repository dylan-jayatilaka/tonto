#!/usr/bin/env bash
#
# Stop hook: snapshot this machine's working tree to a per-machine WIP branch
# on GitHub, so nothing is ever local-only.
#
# Tonto is developed on several machines (sauce, achari2, a Mac) that cannot
# reach each other -- but all of them can reach GitHub over ordinary outbound
# HTTPS. On 2026-08-09 four days of work, including docs/WORKSHOP.md, was
# stranded on machines behind the MPI Goettingen firewall with no way to get it
# out. This makes that impossible: after every turn the tree is on GitHub.
#
# It is deliberately NON-INVASIVE. It never touches your working tree, your
# index, your HEAD, or any branch you work on. It builds a commit object out of
# a scratch index and force-pushes it to refs/heads/wip/<hostname>, which is
# yours alone. Recover from any other machine with:
#
#     git fetch origin && git checkout wip/<hostname>
#
# Emits {"systemMessage": ...} so you can see it happen. Never blocks the turn.

set -uo pipefail

cd "${CLAUDE_PROJECT_DIR:-$PWD}" 2>/dev/null || exit 0
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0
git rev-parse --verify HEAD >/dev/null 2>&1 || exit 0

host=$(hostname -s 2>/dev/null || hostname)
branch="wip/${host}"

# Nothing to snapshot? Stay silent -- the sibling hook reports unpushed commits.
[ -z "$(git status --porcelain)" ] && exit 0

# Build the snapshot in a scratch index so the real one is untouched.
tmpindex=$(mktemp) || exit 0
trap 'rm -f "$tmpindex"' EXIT
export GIT_INDEX_FILE="$tmpindex"

git read-tree HEAD                     2>/dev/null || exit 0
git add -A                             2>/dev/null || exit 0
tree=$(git write-tree                  2>/dev/null) || exit 0

msg="wip snapshot from ${host} on $(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
commit=$(git commit-tree "$tree" -p HEAD -m "$msg" 2>/dev/null) || exit 0

unset GIT_INDEX_FILE

# Force-push: successive snapshots are not fast-forwards of each other.
if git push --force --quiet origin "${commit}:refs/heads/${branch}" 2>/dev/null; then
   n=$(git status --porcelain | wc -l | tr -d ' ')
   jq -Rn --arg m "Snapshotted $n changed file(s) to origin/${branch} -- safe on GitHub." \
      '{systemMessage: $m}'
else
   jq -Rn --arg m "WARNING: could not push the WIP snapshot to origin/${branch}. Your work is on ${host} ONLY. Check network/credentials before you stop for the day." \
      '{systemMessage: $m}'
fi
