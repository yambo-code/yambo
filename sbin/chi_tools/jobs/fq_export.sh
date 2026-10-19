#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=12:00:00
#SBATCH --job-name=fq_export
#SBATCH --output=log/fq_export.o%A
#SBATCH --error=log/fq_export.e%A

# Steps A + B: the exact kernel fxc_GG'(q, w) at every q of the grid, from the BSE (BSEChiOut).
# Run from 06_TEST_VAL with the same settings as binding.sh. Needs the fork with explicit
# anti-resonant transitions at finite q. Rebuild with a fresh dependency list (the nvfortran
# build otherwise keeps objects that read shifted module variables):
#   cd ~/yambo-cpu && git pull && make clean what=dep && make clean && make yambo
#
#   GINP=1 (default): BSEGinplane. Exchange and fxc act on the in-plane G (G_z = 0) only; the
#          z-only G of the basis are dropped. NG must reach the first in-plane star: about
#          21 RL for this cell (G=0, 14 z-only G, then 6 in-plane). The report line
#          "[Chi/BSK] BSEGinplane: kernel on the in-plane G of the basis  ns  ng" must show ns >= 7.
#   GINP=0 : all G of the basis (the old behaviour; NG=1 gives the head only).
#
#  1. fqx_bse : SEX coupling BSE with BSEChiOut "exc" for q = QFIRST..QLAST (one run, one
#               ndb.Chi with a fragment per q). Each q reports
#               "[Chi/BSK] head of the exported response vs the BSE response" (must be ~0, 1, any).
#  2. fqx_cas : Casida with that kernel (BSKmod="CHI", BSEChiDyn) at q = QV: must give the
#               BSE spectrum of step 1 at that q, to ~1e-4 or better (single-precision
#               BSE matrix and ndb.Chi; hBN test: 2e-5 to 1e-4, as at q -> 0).
#  3. fxc_export.py: text + npz of fxc_GG'(q, w) for every q (static file and all-w file).

BANDS=${BANDS:-"11 | 16"}
GINP=${GINP:-1}                  # 1 = BSEGinplane (step B)
if [ "$GINP" = 1 ]; then NG=${NG:-21}; else NG=${NG:-1}; fi   # G basis of v and fxc
WBLK=${WBLK:-"5 Ry"}
WDIR=${WDIR:-cohsex}
QP=${QP:-'KfnQPdb= "E < g0w0/ndb.QP"'}
QFIRST=${QFIRST:-1}; QLAST=${QLAST:-27}   # q = 1 first keeps the optical q0 in the header
QV=${QV:-2}                      # q used for the Casida validation
WMAX=${WMAX:-4.0}; NW=${NW:-201}; ETA=${ETA:-0.05}
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
grep -q "BSE momentum index" $YSRC/src/bse/K_Chi_export.F || { echo "!!! $YSRC lacks the finite-q export: git pull + make yambo"; exit 1; }
grep -q "BSEGinplane" $YSRC/src/bse/K_driver_init.F || { echo "!!! $YSRC lacks BSEGinplane: git pull + make yambo"; exit 1; }
grep -q "explicit anti-resonant" $YSRC/src/bse/K_driver_init.F || { echo "!!! $YSRC lacks the finite-q anti-resonant fix: git pull + rebuild"; exit 1; }
[ "$(which yambo)" -nt $YSRC/src/bse/K_Chi_export.F ] || { echo "!!! binary older than source: make yambo"; exit 1; }
test -e $WDIR/ndb.em1s || { echo "!!! $WDIR/ndb.em1s missing"; exit 1; }
rm -rf w_em1s; mkdir -p w_em1s; for f in $WDIR/ndb.em1s*; do ln -s "$PWD/$f" w_em1s/; done

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
 0.0 | $WMAX | eV
%
% BDmRange
 $ETA | $ETA | eV
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
  yambo $YFLAGS -F $1.in -J "$2" > log/$1.out 2>&1 && ls o-$1.eps* >/dev/null 2>&1 && return 0
  echo "!!! $1 failed: see l-$1* and log/$1.out"; tail -5 log/$1.out; return 1
}

EXTRA=(); XFL=()
if [ "$GINP" = 1 ]; then EXTRA=(BSEGinplane); XFL=(--inplane); fi
echo "=== NG = $NG RL, GINP = $GINP, q = $QFIRST..$QLAST, Casida check at q = $QV ==="

# ---------- 1. BSE + export at every q ----------
bse_in fqx_bse.in SEX $QFIRST $QLAST 'BSEChiOut= "exc"' "BSENGBlk= $WBLK" "${EXTRA[@]}"
run fqx_bse "fqx_bse,w_em1s" || exit 1
rm -rf kx_fq; mkdir -p kx_fq; cp fqx_bse/ndb.Chi* kx_fq/
echo "=== per-q export checks (residual, |scale/c|, arg) and rcond ==="
grep -h "BSE momentum index\|in-plane G of the basis\|anti-resonant rows\|Lkind\|head of the exported response\|minimum rcond\|lowest bright excitation" r-fqx_bse_* | sed 's/^ */  /'
grep -h "WARNING.*Chi/BSK\|ERROR" r-fqx_bse_* l-fqx_bse_* 2>/dev/null | sort -u | head

# ---------- 2. Casida with the exported kernel at q = QV ----------
bse_in fqx_cas.in CHI $QV $QV BSEChiDyn "${EXTRA[@]}"
run fqx_cas "fqx_cas,kx_fq" && python3 - "$QV" <<'PY'
import sys, glob, numpy as np
q = sys.argv[1]
b = np.loadtxt(glob.glob(f'o-fqx_bse.eps_q{q}_*')[0])
c = np.loadtxt(glob.glob(f'o-fqx_cas.eps_q{q}_*')[0])
d = np.abs(c[:, 1:3] - b[:, 1:3]).max() / np.abs(b[:, 1:3]).max()
print(f'=== Casida with the exported kernel vs BSE at q{q}: max relative difference {d:.2e} (text precision ~1e-6)')
PY

# ---------- 3. fxc_GG'(q, w) files ----------
python3 $YSRC/sbin/chi_tools/fxc_export.py kx_fq --out fxc_GGq_static --static --sym "${XFL[@]}"
python3 $YSRC/sbin/chi_tools/fxc_export.py kx_fq --out fxc_GGq_allw --sym "${XFL[@]}"
ls -lh fxc_GGq_*
