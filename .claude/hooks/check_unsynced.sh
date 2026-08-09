#!/usr/bin/env bash
#
# Stop hook: warn when work in this repo exists only on this machine.
#
# Tonto is developed across more than one box (sauce, achari2, ...). Anything
# that is uncommitted, or committed but unpushed, is invisible to every other
# machine -- which is how a docs/WORKSHOP.md came to be stranded on 2026-08-09.
# git push is the save button; this says so at the end of every turn.
#
# Emits {"systemMessage": ...} on stdout when there is something to report, and
# nothing at all when the tree is clean. Never blocks: the turn always ends.

set -uo pipefail

cd "${CLAUDE_PROJECT_DIR:-$PWD}" 2>/dev/null || exit 0
git rev-parse --is-inside-work-tree >/dev/null 2>&1 || exit 0

parts=()

# Tracked files with staged or unstaged changes.
changed=$(git status --porcelain --untracked-files=no | wc -l | tr -d ' ')
if [ "$changed" -gt 0 ]; then
   parts+=("$changed tracked file(s) modified but not committed")
fi

# Untracked markdown -- the WORKSHOP.md failure mode. Build trees hold no .md
# worth keeping, so they are excluded rather than reported every turn.
mapfile -t newdocs < <(git ls-files --others --exclude-standard -- '*.md' \
                       | grep -v -E '^(build|release|debug|mpi|external)/' | head -5)
if [ "${#newdocs[@]}" -gt 0 ]; then
   parts+=("untracked: ${newdocs[*]}")
fi

# Committed but unpushed -- equally invisible to the other machine.
branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)
if upstream=$(git rev-parse --abbrev-ref --symbolic-full-name '@{u}' 2>/dev/null); then
   ahead=$(git rev-list --count "$upstream..HEAD" 2>/dev/null || echo 0)
   if [ "$ahead" -gt 0 ]; then
      parts+=("$ahead commit(s) on $branch not pushed to $upstream")
   fi
elif [ -n "$branch" ] && [ "$branch" != "HEAD" ]; then
   parts+=("branch '$branch' has no upstream -- nothing on it is pushed")
fi

[ "${#parts[@]}" -eq 0 ] && exit 0

msg="Unsynced work in tonto (this machine only): "
msg+=$(printf '%s; ' "${parts[@]}")
msg="${msg%; }. Push it if another machine needs it."

jq -Rn --arg m "$msg" '{systemMessage: $m}'
