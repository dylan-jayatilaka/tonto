#!/bin/sh
#
# Fetch the 167 MB CRYSTAL23 XML that tests/crystal23/ammonium_borane_pHAR_C23 needs.
#
# WHY THIS EXISTS, and why the file is not simply committed:
#
#   The asset is 167 MB. Committing it -- or committing its Git LFS pointer with
#   a .gitattributes that tracks it -- makes EVERY clone pull 167 MB, because LFS
#   smudges the checked-out ref automatically for anyone who has git-lfs
#   installed. GitHub's free LFS allowance is 1 GB of storage AND 1 GB/month of
#   bandwidth, so a handful of clones exhausts it. For one test, in a public
#   repository, that is a bad trade.
#
#   So `develop` and `master` carry NO LFS objects at all (check with
#   `git lfs ls-files develop`). The pointer lives only on the archive tag
#   `archive/release-pHAR-broken`, which nobody checks out, and this script is
#   how you opt in. The test SKIPS when the asset is absent.
#
# The asset is verified to be retrievable: see DEFERRED.md, "Reinstate the
# ammonia-borane pHAR test". Its sha256 is the LFS oid below.
#
# Usage:   scripts/fetch_phar_asset.sh [destination-directory]
#
set -eu

OID=1c5c24f0903c1b8667e3f8aa41ba1b2a550a49370b22db422a37d7a1f093a8ee
SIZE=174978609
NAME=GenerateXML.XML
TAG=archive/release-pHAR-broken
# NOTE the path INSIDE THE TAG is still tests/long/... -- the tag is frozen and
# does not follow the working tree, where the job now lives in tests/crystal23.
PATH_IN_TAG=tests/long/ammonium_borane_pHAR_C23/$NAME

REPO=$(git rev-parse --show-toplevel)
DEST=${1:-$REPO/tests/crystal23/ammonium_borane_pHAR_C23}

if [ ! -d "$DEST" ]; then
   echo "fetch_phar_asset: no such directory: $DEST" >&2
   exit 1
fi

if [ -f "$DEST/$NAME" ] && [ "$(wc -c < "$DEST/$NAME")" -eq "$SIZE" ]; then
   echo "fetch_phar_asset: already present and the right size, nothing to do"
   exit 0
fi

echo "fetch_phar_asset: fetching $NAME ($SIZE bytes) ..."

# Preferred route: git-lfs, from the archive tag. Note .gitattributes is ABSENT
# at that tag -- that loss is what broke the test originally -- but the pointer
# is in the tree, so git-lfs still resolves the oid.
if command -v git-lfs >/dev/null 2>&1; then
   TMP=$(mktemp -d)
   # shellcheck disable=SC2064
   trap "rm -rf '$TMP'" EXIT
   if git -C "$REPO" show "$TAG:$PATH_IN_TAG" > "$TMP/pointer" 2>/dev/null &&
      git -C "$REPO" lfs smudge < "$TMP/pointer" > "$DEST/$NAME" 2>/dev/null &&
      [ "$(wc -c < "$DEST/$NAME")" -eq "$SIZE" ]; then
      echo "fetch_phar_asset: fetched via git-lfs"
   else
      rm -f "$DEST/$NAME"
   fi
fi

# Fallback: the LFS protocol is plain HTTP, so the batch API works with nothing
# installed but curl. This is also how the object was first proved retrievable,
# on a machine with no git-lfs.
if [ ! -f "$DEST/$NAME" ]; then
   REMOTE=$(git -C "$REPO" remote get-url origin)
   case $REMOTE in
      *.git) BATCH="$REMOTE/info/lfs/objects/batch" ;;
      *)     BATCH="$REMOTE.git/info/lfs/objects/batch" ;;
   esac
   HREF=$(curl -s -X POST \
      -H "Accept: application/vnd.git-lfs+json" \
      -H "Content-Type: application/vnd.git-lfs+json" \
      -d "{\"operation\":\"download\",\"transfers\":[\"basic\"],\"objects\":[{\"oid\":\"$OID\",\"size\":$SIZE}]}" \
      "$BATCH" |
      python3 -c 'import sys,json
d=json.load(sys.stdin)
o=d.get("objects",[{}])[0]
a=o.get("actions",{}).get("download",{})
print(a.get("href",""))')
   if [ -z "$HREF" ]; then
      echo "fetch_phar_asset: the LFS object is not retrievable." >&2
      echo "  It may have been pruned, or the bandwidth quota exhausted." >&2
      echo "  Regenerate it from Crystal23_InputFiles.zip instead -- see DEFERRED.md." >&2
      exit 1
   fi
   curl -s -o "$DEST/$NAME" "$HREF"
   echo "fetch_phar_asset: fetched via the LFS batch API"
fi

# Verify. A truncated or wrong file is worse than an absent one, since the test
# would then run and produce numbers nobody could trust.
GOT=$(sha256sum "$DEST/$NAME" | cut -d' ' -f1)
if [ "$GOT" != "$OID" ]; then
   echo "fetch_phar_asset: SHA256 MISMATCH -- removing the file" >&2
   echo "  expected $OID" >&2
   echo "  got      $GOT" >&2
   rm -f "$DEST/$NAME"
   exit 1
fi

echo "fetch_phar_asset: OK -- $DEST/$NAME verified against its LFS oid"
