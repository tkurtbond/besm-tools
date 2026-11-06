#!/usr/bin/env bash
# compare-besm2_fmt.sh -- compare the besm2-rst variants' output with
# besm2_fmt's golden files (the Ada port, ~/Repos/RPG/Tools/besm2_fmt,
# its test/golden.cases and test/golden/).  Run from this directory,
# after the programs are built (make compare-besm2_fmt does both).
#
#   ./compare-besm2_fmt.sh BESM2_FMT GOLDEN=YAML...
#
# BESM2_FMT is besm2_fmt's directory.  Each GOLDEN=YAML compares the
# file YAML here with besm2_fmt's golden files GOLDEN.*.out, in every
# case of golden.cases whose fixtures' glob matches GOLDEN and whose
# mode is "-" (standard output and error, as here) or "stdout"
# (standard output only: the bad-input cases, whose messages differ).
# The cases that read several files, standard input or -o are left out.
#
# -n is the other way round in besm2_fmt, which uses U+2212 (and the
# times sign) by default: a case's flags lose their -n here, or gain
# one.  Each case is run by besm2-rst, besm2-rst -f, besm2-rst-f,
# besm2-rst-e, besm2-rst-e -f and besm2-rst-f-e.  Prints MATCH or
# DIFFER (with a diff) per case and program, and exits 1 if any
# differed.

set -u
fmt=${1:?usage: compare-besm2_fmt.sh BESM2_FMT GOLDEN=YAML...}
shift
cases=$fmt/test/golden.cases
golden=$fmt/test/golden
[ -r "$cases" ] || { echo "compare-besm2_fmt.sh: no $cases" >&2; exit 2; }
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
programs=("build/besm2-rst" "build/besm2-rst -f" "build/besm2-rst-f"
          "build/besm2-rst-e" "build/besm2-rst-e -f" "build/besm2-rst-f-e")
status=0 count=0

shopt -s extglob
for pair in "$@"; do
  name=${pair%%=*} yaml=${pair#*=}
  [ -r "$yaml" ] || { echo "compare-besm2_fmt.sh: no $yaml" >&2; exit 2; }
  while read -r modes id pattern flags; do
    case $modes in ''|'#'*) continue ;; -|stdout) ;; *) continue ;; esac
    # $pattern is a glob on purpose.
    # shellcheck disable=SC2053
    [[ $name == $pattern ]] || continue
    out=$golden/$name.$id.out
    [ -e "$out" ] || continue
    # Map -n.
    local_flags=() had_n=no
    for f in $flags; do
      if [ "$f" = -n ]; then had_n=yes; else local_flags+=("$f"); fi
    done
    [ $had_n = no ] && local_flags+=(-n)
    for prog in "${programs[@]}"; do
      # $prog is split into words on purpose.
      # shellcheck disable=SC2086
      if [ "$modes" = stdout ]; then
        $prog "${local_flags[@]}" "$yaml" >"$tmp/out" 2>/dev/null
      else
        $prog "${local_flags[@]}" "$yaml" >"$tmp/out" 2>&1
      fi
      st=$?
      [ $st -eq 1 ] && echo "==== exit 1 ====" >>"$tmp/out"
      count=$((count + 1))
      label="$prog ${local_flags[*]} $yaml vs $out"
      if [ $st -gt 1 ]; then
        echo "DIFFER:  $label: exit $st"; status=1
      elif cmp -s "$out" "$tmp/out"; then
        echo "MATCH:   $label"
      else
        echo "DIFFER:  $label"; diff -u "$out" "$tmp/out" | head -40
        status=1
      fi
    done
  done <"$cases"
done
[ $count -gt 0 ] || { echo "compare-besm2_fmt.sh: no cases" >&2; exit 2; }
exit $status
