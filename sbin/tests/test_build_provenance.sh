#!/bin/sh

set -eu

top=$(CDPATH= cd -- "$(dirname "$0")/../.." && pwd)
generator=$top/sbin/generate_build_provenance.sh
tmp_root=${TMPDIR:-/home/nicola/tmp/codex}
mkdir -p "$tmp_root"
work=$(mktemp -d "$tmp_root/yambo-provenance-test.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM

fail() { echo "FAIL: $*" >&2; exit 1; }
assert_field() { grep -Fqx "$1=$2" "$3" || fail "expected $1=$2 in $3"; }
generate() { sh "$generator" "$1" "$2/out.h" "$2/out.txt" >/dev/null; }

repo=$work/repo
mkdir -p "$repo/include/version" "$work/out"
cp "$top/include/version/version.m4" "$repo/include/version/version.m4"
git -C "$repo" init -q
git -C "$repo" config user.name provenance-test
git -C "$repo" config user.email provenance-test@example.invalid
printf '%s\n' baseline > "$repo/tracked"
git -C "$repo" add include/version/version.m4 tracked
git -C "$repo" commit -q -m baseline

generate "$repo" "$work/out"
head=$(git -C "$repo" rev-parse HEAD)
branch=$(git -C "$repo" branch --show-current)
assert_field commit "$head" "$work/out/out.txt"
assert_field ref "$branch" "$work/out/out.txt"
assert_field state clean "$work/out/out.txt"

mtime=$(stat -c %Y "$work/out/out.h")
sleep 1
generate "$repo" "$work/out"
[ "$(stat -c %Y "$work/out/out.h")" = "$mtime" ] || fail "unchanged header was replaced"

printf '%s\n' untracked > "$repo/untracked"
generate "$repo" "$work/out"
assert_field state clean "$work/out/out.txt"
printf '%s\n' dirty >> "$repo/tracked"
generate "$repo" "$work/out"
assert_field state dirty "$work/out/out.txt"
git -C "$repo" add tracked
generate "$repo" "$work/out"
assert_field state dirty "$work/out/out.txt"
git -C "$repo" restore --staged --worktree tracked

git -C "$repo" tag 'quoted"tag'
generate "$repo" "$work/out"
assert_field tag 'quoted"tag' "$work/out/out.txt"
grep -Fq '#define YAMBO_BUILD_TAG "quoted\"tag"' "$work/out/out.h" || fail "C escaping"

git -C "$repo" checkout -q --detach
generate "$repo" "$work/out"
assert_field ref detached "$work/out/out.txt"

nogit=$work/nogit
mkdir -p "$nogit/include/version" "$work/nogit-out"
cp "$top/include/version/version.m4" "$nogit/include/version/version.m4"
cat > "$nogit/include/version/release_provenance.txt" <<EOF
version=5.4.0
commit=1111111111111111111111111111111111111111
commit_short=111111111111
ref=release
tag=v5.4.0
state=clean
source=release-manifest
EOF
generate "$nogit" "$work/nogit-out"
assert_field source release-manifest "$work/nogit-out/out.txt"

rm "$nogit/include/version/release_provenance.txt"
generate "$nogit" "$work/nogit-out"
assert_field commit unknown "$work/nogit-out/out.txt"

YAMBO_BUILD_COMMIT=2222222222222222222222222222222222222222 \
YAMBO_BUILD_REF=ci YAMBO_BUILD_TAG=v-ci YAMBO_BUILD_DIRTY=0 \
  sh "$generator" "$nogit" "$work/nogit-out/out.h" "$work/nogit-out/out.txt" >/dev/null
assert_field source ci-override "$work/nogit-out/out.txt"
assert_field state clean "$work/nogit-out/out.txt"

echo "build provenance tests: PASS"
