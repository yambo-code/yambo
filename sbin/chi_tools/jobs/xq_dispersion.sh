#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=12:00:00
#SBATCH --job-name=xq_disp
#SBATCH --output=log/xq_disp.o%A
#SBATCH --error=log/xq_disp.e%A
#SBATCH --mail-type=ALL,TIME_LIMIT_80
#SBATCH --mail-user=ad531114@ucf.edu

# Exciton energies and binding energies at every q from the exported kernel fxc_GG'(q, w):
# Casida with the q-dependent kernel, solved self-consistently (lambda(w) = w), next to the
# BSE and to Casida with the static kernel fxc(q, 0). Run from the directory with SAVE,
# cohsex/ndb.em1s* and g0w0/ndb.QP (same settings as fq_export.sh).
#
# STAGE=export : coupled SEX BSE + BSEChiOut "exc" for q = QFIRST..QLAST on UNDAMPED real
#                frequencies WLO..WHI (step DW), below the lowest gap of every q. Writes
#                kx_xq/ndb.Chi and the BSE excitons of every q (r-xq_bse_*).
# STAGE=scan   : for each q of QS (default QFIRST..QLAST), two Casida runs with that kernel:
#                  xs_q<iq>: matrix at z = 0      -> ktt_casida.py -> static_q<iq>.log
#                  xw_q<iq>: matrices on the frequencies within DWIN of the BSE excitons
#                            -> sc_scan.py (files deleted once read) -> sc_q<iq>.dat/.log
#                Each q takes ~10-30 min (2025 transitions). Split QS over several jobs if needed:
#                  STAGE=scan QS="1 2 3 4 5 6 7 8 9" sbatch xq_dispersion.sh   (and so on)
# STAGE=summary: xq_summary.py -> xq_dispersion.dat/.png, xq_dispersion_omega.dat, and
#                fxc_GGq_scw.dat/.npz: fxc_GG'(q, w*) at the self-consistent exciton energy w*
#                of each q (a static kernel that has the exciton as an eigenvalue).
# STAGE=all    : the three in a row (default).

STAGE=${STAGE:-all}
BANDS=${BANDS:-"11 | 16"}
NG=${NG:-21}
GINP=${GINP:-1}
WBLK=${WBLK:-"5 Ry"}
WDIR=${WDIR:-cohsex}
QP=${QP:-'KfnQPdb= "E < g0w0/ndb.QP"'}
QFIRST=${QFIRST:-1}; QLAST=${QLAST:-27}
QS=${QS:-$(seq -s ' ' $QFIRST $QLAST)}
WLO=${WLO:-2.15}; WHI=${WHI:-2.54}; DW=${DW:-0.01}   # BSE excitons 2.23-2.49 eV, lowest gap 2.543 eV
DWIN=${DWIN:-0.04}                                   # scan window around the BSE excitons
YFLAGS=${YFLAGS--nompi}

if type module >/dev/null 2>&1 && [ -n "$SLURM_JOB_ID" ]; then
  module purge
  module use "$HOME/modulefiles"
  module load cuda_hpc_sdk/24.5
  module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
  module load yambo/5.3-cpu
  source "$NVHPC_ROOT/comm_libs/12.4/hpcx/latest/hpcx-init.sh" && hpcx_load
fi
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-${OMP_NUM_THREADS:-1}}
cd "${SLURM_SUBMIT_DIR:-.}"; mkdir -p log
YSRC=${YSRC:-$HOME/yambo-cpu}
T=$YSRC/sbin/chi_tools
test -e $T/xq_summary.py || { echo "!!! $YSRC lacks xq_summary.py: git pull"; exit 1; }
grep -q "explicit anti-resonant" $YSRC/src/bse/K_driver_init.F || { echo "!!! $YSRC lacks the finite-q fix: git pull + rebuild"; exit 1; }
grep -q "_antiresonant" $YSRC/src/bse/K_Chi_Kdump.F || { echo "!!! $YSRC lacks the finite-q dump: git pull + make yambo"; exit 1; }
[ "$(which yambo)" -nt $YSRC/src/bse/K_Chi_Kdump.F ] || { echo "!!! yambo binary older than the source: make yambo"; exit 1; }
NW=$(python3 -c "print(int(round(($WHI - $WLO) / $DW)) + 1)")
EXTRA=(); XFL=()
if [ "$GINP" = 1 ]; then EXTRA=(BSEGinplane); XFL=(--inplane); fi

bse_in () {   # file kernel q1 q2 [extra lines...]
  local f=$1 kern=$2 q1=$3 q2=$4; shift 4
  cat > $f <<EOF
optics
bss
bse
dipoles
BSKmod= "$kern"
BSEmod= "coupling"
BSSmod= "d"
BSENGexx= $NG RL
BSENGfxc= $NG RL
K_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
% BSEQptR
 $q1 | $q2 |
%
% BSEBands
 $BANDS |
%
% BEnRange
 $WLO | $WHI | eV
%
% BDmRange
 0.0 | 0.0 | eV
%
BEnSteps= $NW
% BLongDir
 1.0 | 0.0 | 0.0 |
%
BSEprop= "abs"
$QP
EOF
  for l in "$@"; do printf '%s\n' "$l" >> $f; done
}
run () {   # job job-string
  rm -rf $1; rm -f [rlo]-$1[._]*
  yambo $YFLAGS -F $1.in -J "$2" > log/$1.out 2>&1 && ls r-$1_* >/dev/null 2>&1 && return 0
  echo "!!! $1 failed: see l-$1* and log/$1.out"; tail -5 log/$1.out; return 1
}
bse_line () {   # iq pattern -> last field of that line in the block of momentum iq
  awk -v q=$1 -v p="$2" '/BSE momentum index/{i=$NF} i==q && index($0,p){print $NF}' r-xq_bse_* | tail -1
}
bse_low () { awk -v q=$1 '/BSE momentum index/{i=$NF} i==q && /lowest excitation energies/{print $6}' r-xq_bse_* | tail -1; }

# ---------- export ----------
if [ "$STAGE" = export ] || [ "$STAGE" = all ]; then
  test -e $WDIR/ndb.em1s || { echo "!!! $WDIR/ndb.em1s missing"; exit 1; }
  rm -rf w_em1s; mkdir -p w_em1s; for f in $WDIR/ndb.em1s*; do ln -s "$PWD/$f" w_em1s/; done
  echo "=== export: q = $QFIRST..$QLAST, $NW undamped frequencies $WLO..$WHI eV, NG = $NG, GINP = $GINP ==="
  bse_in xq_bse.in SEX $QFIRST $QLAST 'BSEChiOut= "exc"' "BSENGBlk= $WBLK" "${EXTRA[@]}"
  run xq_bse "xq_bse,w_em1s" || exit 1
  rm -rf kx_xq; mkdir -p kx_xq; cp xq_bse/ndb.Chi* kx_xq/
  grep -h "head of the exported response\|minimum rcond" r-xq_bse_* | sort | uniq -c | head
  python3 $T/fxc_export.py kx_xq --out fxc_xq_static --static --sym "${XFL[@]}" > log/fxc_xq_static.out || exit 1
  for q in $(seq $QFIRST $QLAST); do        # the scan window must lie in WLO..WHI, below the gap
    python3 - $q "$(bse_low $q)" "$(bse_line $q 'lowest bright excitation (strength')" \
              "$(bse_line $q 'lowest transition energy of the window')" $WLO $WHI $DWIN <<'PY'
import sys
q, lo, br, gap, wlo, whi, d = sys.argv[1], *map(float, sys.argv[2:])
if min(lo, br) - d < wlo or max(lo, br) + d > whi:
    print(f'  !!! q{q}: excitons {lo:.4f} {br:.4f} eV +- {d} not inside {wlo}..{whi} eV: move WLO/WHI')
if whi >= gap:
    print(f'  !!! q{q}: WHI = {whi} eV reaches the gap {gap:.4f} eV (poles of the bubble on the grid)')
PY
  done
fi

# ---------- scan ----------
if [ "$STAGE" = scan ] || [ "$STAGE" = all ]; then
  test -e kx_xq/ndb.Chi || { echo "!!! kx_xq/ndb.Chi missing: run STAGE=export first"; exit 1; }
  for q in $QS; do
    lo=$(bse_low $q); br=$(bse_line $q 'lowest bright excitation (strength')
    [ -n "$lo" ] && [ -n "$br" ] || { echo "!!! q$q: no BSE excitons in r-xq_bse_*"; continue; }
    read W1 W2 <<< $(python3 -c "print(max(min($lo,$br)-$DWIN,$WLO), min(max($lo,$br)+$DWIN,$WHI))")
    echo "=== q$q: BSE lowest $lo, bright $br eV; static matrix and scan $W1..$W2 eV ==="
    bse_in xs_q$q.in CHI $q $q BSEChiDyn "${EXTRA[@]}" '% BSEChiKoutW' ' 0.0 | 0.0 | eV' '%'
    if run xs_q$q "xs_q$q,kx_xq"; then
      python3 $T/ktt_casida.py o-xs_q$q.Ktt_q${q}_transitions o-xs_q$q.Ktt_q${q}_w1 > static_q$q.log
      grep "lowest bright" static_q$q.log
      rm -f o-xs_q$q.Ktt_q${q}_w*
    fi
    bse_in xw_q$q.in CHI $q $q BSEChiDyn "${EXTRA[@]}" '% BSEChiKoutW' " $W1 | $W2 | eV" '%'
    if run xw_q$q "xw_q$q,kx_xq"; then
      python3 $T/sc_scan.py o-xw_q$q.Ktt_q${q}_transitions o-xw_q$q.Ktt_q${q}_w* \
              --out sc_q$q.dat --exciton $br --delete > sc_q$q.log 2>&1
      grep "= w at\|BSE = \|no crossing\|!!!" sc_q$q.log
    fi
    rm -rf xs_q$q xw_q$q
  done
fi

# ---------- summary ----------
if [ "$STAGE" = summary ] || [ "$STAGE" = all ]; then
  python3 $T/xq_summary.py --bse 'r-xq_bse_*' --npz fxc_xq_static.npz --out xq_dispersion || exit 1
  test -s xq_dispersion_omega.dat && \
    python3 $T/fxc_export.py kx_xq --at xq_dispersion_omega.dat --sym "${XFL[@]}" --out fxc_GGq_scw
  ls -lh xq_dispersion* fxc_GGq_scw* 2>/dev/null
fi
