#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --cpus-per-task=2
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=48:00:00
#SBATCH --job-name=xkernel
#SBATCH --output=log/xkernel.o%A
#SBATCH --error=log/xkernel.e%A

# Production run of the exact kernel fxc_GG'(q, w) of a monolayer (WS2, WSe2, ...) from a fresh
# SAVE, with the parameters converged for WS2 (gw_converge.sh, bse_converge.sh):
#
#   STAGE=setup  : yambo setup, system (bands, k grid, VB, spinors), plan of the run; checks only
#   STAGE=gw     : G0W0 plasmon pole, RIM-W, slab cutoff, QP energies of the BSE bands at every k
#                  -> prod/gw/ndb.QP                                                      (MPI)
#   STAGE=w      : static screening of the BSE -> prod/w/ndb.em1s                            (MPI)
#   STAGE=ref    : reference BSE at q = 0: Tamm-Dancoff, exchange on all G (BSENGexx = EXX_REF), as
#                  in the convergence; its A exciton is the number to quote                  (MPI)
#   STAGE=export : coupled BSE at every q with BSEChiOut "exc" and BSEGinplane, on undamped real
#                  frequencies below the lowest gap: writes the kernel prod/kx/ndb.Chi and checks
#                  every q (exported response = BSE response, in-plane G, conditioning)    (1 task)
#   STAGE=scan   : Casida with the kernel at each q of QS: static (fxc(q,0)) and self-consistent
#                  (lambda(w) = w); must give the BSE excitons of the export              (1 task)
#   STAGE=summary: prod/summary.txt, xq_dispersion.dat/.png and the kernel files:
#                    fxc_GGq_static.*  fxc(q, w=0)
#                    fxc_GGq_allw.*    fxc(q, w) on the whole grid
#                    fxc_GGq_scw.*     fxc(q, w*) at the self-consistent exciton energy w* of each q
#   STAGE=all    : all of them in a row (default). Every stage skips what is already done, so a job
#                  that stops (time limit, error) is resubmitted with the same command.
#
# Run from the directory with SAVE (QE nscf on the production k grid + p2y). Everything goes to
# prod/. Typical use, one directory per material and grid:
#   cd WSe2/k18 && sbatch ~/yambo-cpu/sbin/chi_tools/jobs/exact_kernel.sh
#
# Spinor (SOC) SAVE: detected from r_setup ("Spinor components: 2"). Each band then holds one
# electron, so the band counts double (GW_BANDS, WB) and the BSE window is 2:4 instead of 1:2.
#
# The kernel stages (export, scan) run as one task with all the job's CPUs as OpenMP threads (the
# export supports one MPI rank only); the others use srun with the job's MPI tasks.
#
# Choices that differ from the convergence runs, and why (see doc/greens_function_fxc.md):
#  - The export BSE has coupling (the exact kernel needs the full response) and BSEGinplane:
#    exchange and kernel on the in-plane G (G_z = 0) of NG_X (default 2 Ry: G = 0 and the first
#    in-plane star). The pure-G_z plane waves of the vacuum make the kernel ill-conditioned. Its
#    BSE is therefore a slightly different model from the reference BSE; summary.txt prints both
#    binding energies, and the kernel is exact for the export BSE.
#  - Frequencies are undamped, WLO..WHI with WHI just below the indirect QP gap (the lowest gap of
#    any q) and WLO = WMARGIN below the lowest exciton of the reference BSE, step DW. fxc(w) has poles
#    between the excitons; an exciton within a step or two of one can be missed by the scan (summary
#    says so): re-export with a smaller DW (test: 51 meV off at DW = 0.01, 0.2 meV at DW = 0.002).
#
# What the kernel cannot do: P is the density response, so an exciton with no density vertex (the
# spin-forbidden dark excitons of a spinor SAVE, relative strength ~1e-8) is not a pole of P, and
# no density kernel puts it back: Casida leaves those transitions at their bare energies. The
# summary lists them. With SOC the lowest exciton of WSe2/WS2 is such a dark state.

STAGE=${STAGE:-all}
# GW (converged for WS2: 40 Ry, 800 bands or 400 with the BG terminator, 12 Ry, RIM-W)
GW_EXX=${GW_EXX:-40}; GW_BLOCK=${GW_BLOCK:-12}; GTERM=${GTERM-BG}
RIMW_G=${RIMW_G:-15}
PPA=${PPA:-'PPAPntXp= 27.21138 eV'}
# BSE (converged for WS2 at 12x12: 100 bands, 3 Ry, 6 Ry, 1:2)
XBLK=${XBLK:-6}; WBLK=${WBLK:-3}; EXX_REF=${EXX_REF:-6}
# kernel export
NG_X=${NG_X:-"2 Ry"}                 # BSENGexx = BSENGfxc of the export (in-plane G of it are kept)
DW=${DW:-}; WMARGIN=${WMARGIN:-0.5}; DWIN=${DWIN:-0.04}  # eV: grid step (default 0.002, 0.005 for large systems), margin below the exciton, scan half-window
QS=${QS:-}                           # q of the scan (default: all if cheap, else 1; see below)
ETA=${ETA:-0.05}; DE=${DE:-0.005}    # broadening and step of the reference spectrum
CUT=${CUT:-'CUTGeo= "slab z"'}
RIM=${RIM:-$'RandQpts= 1000000\nRandGvec= 100 RL'}
FFTG=${FFTG:-""}
EXTRA_GW=${EXTRA_GW:-""}; EXTRA_BSE=${EXTRA_BSE:-""}
YSRC=${YSRC:-$HOME/yambo-cpu}        # the fork's source (tools in sbin/chi_tools)
FORKCHECK=${FORKCHECK:-1}
YFLAGS=${YFLAGS-auto}
RUNNER=${RUNNER:-""}
CHECK=${CHECK:-1}

if type module >/dev/null 2>&1 && [ -n "$SLURM_JOB_ID" ]; then
  module purge
  module use "$HOME/modulefiles"
  module load cuda_hpc_sdk/24.5
  module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
  module load yambo/5.3-cpu
  source "$NVHPC_ROOT/comm_libs/12.4/hpcx/latest/hpcx-init.sh" && hpcx_load
fi
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-${OMP_NUM_THREADS:-1}}
NOMPI=""; yambo -h 2>&1 | grep -q nompi && NOMPI=-nompi
[ "$YFLAGS" = auto ] && YFLAGS=$NOMPI
NP=${NP:-${SLURM_NTASKS:-1}}
NT1=${NT1:-$((NP * OMP_NUM_THREADS))}        # threads of the one-task stages
if [ "$NP" -gt 1 ]; then
  if [ "$NP" = "${SLURM_NTASKS:-}" ]; then
    [ -n "$RUNNER" ] || RUNNER="srun --cpus-per-task=$OMP_NUM_THREADS"
  else
    [ -n "$RUNNER" ] || RUNNER="srun -n $NP --cpus-per-task=$OMP_NUM_THREADS"
  fi
  YFLAGS=""
fi
TOP="${SLURM_SUBMIT_DIR:-$PWD}"
cd "$TOP"; mkdir -p log prod
P=$TOP/prod
[ -e "$P/SAVE" ] || ln -s ../SAVE "$P/SAVE"
FFTGL=""; [ -n "$FFTG" ] && FFTGL="FFTGvecs= $FFTG"
T=$YSRC/sbin/chi_tools

# --- the fork: kernel export and tools ---
if [ "$FORKCHECK" = 1 ]; then
  for f in sbin/chi_tools/xq_summary.py sbin/chi_tools/sc_scan.py sbin/chi_tools/ktt_casida.py sbin/chi_tools/fxc_export.py \
           src/bse/K_Chi_export.F src/bse/K_Chi_Kdump.F; do
    [ -e "$YSRC/$f" ] || { echo "!!! $YSRC/$f missing: set YSRC to the fork's source (git pull there)"; exit 1; }
  done
  grep -q "explicit anti-resonant" $YSRC/src/bse/K_driver_init.F &&
  grep -q "BSEGinplane" $YSRC/src/bse/K_driver_init.F &&
  grep -q "lowest bright excitation" $YSRC/src/bse/K_diago_driver.F || {
    echo "!!! $YSRC lacks the finite-q export, BSEGinplane or the binding report: git pull + make yambo"; exit 1; }
  [ "$(which yambo)" -nt $YSRC/src/bse/K_Chi_export.F ] || {
    echo "!!! $(which yambo) is older than $YSRC/src/bse/K_Chi_export.F: make yambo (FORKCHECK=0 skips this test)"; exit 1; }
fi

# --- system ---
setup_info () {
  [ -f "$TOP/SAVE/ns.db1" ] || { echo "!!! $TOP/SAVE/ns.db1 not found: run p2y in the nscf .save folder and put its SAVE here"; exit 1; }
  [ -f "$TOP/r_setup" ] || (cd "$TOP" && $RUNNER yambo $YFLAGS > log/setup.out 2>&1)
  local r=$TOP/r_setup
  NB_SAVE=$(awk -F: '/^ *Bands  /{print $2+0; exit}' $r 2>/dev/null)
  NK=$(awk -F: '/IBZ K-points/{print $2+0; exit}' $r 2>/dev/null)
  NKBZ=$(awk -F: '/BZ +K-points/ && !/IBZ/{print $2+0; exit}' $r 2>/dev/null)
  NQ=$(awk -F: '/IBZ Q-points/{print $2+0; exit}' $r 2>/dev/null)
  VB=$(awk -F: '/Filled Bands/{print $2+0; exit}' $r 2>/dev/null)
  NSP=$(awk -F: '/Spinor components/{print $2+0; exit}' $r 2>/dev/null)
  NGRID=$(awk -F: '/Grid dimensions/{print $2+0; exit}' $r 2>/dev/null)
  [ -n "$NB_SAVE" ] && [ -n "$NK" ] && [ -n "$NKBZ" ] && [ -n "$NQ" ] && [ -n "$VB" ] && [ -n "$NSP" ] || {
    echo "!!! cannot read bands/k-points/q-points/spinors from $r (yambo setup failed?)"
    tail -5 log/setup.out 2>/dev/null; [ -f $r ] && tail -5 $r; rm -f $r; exit 1; }
  CB=$((VB + 1))
}
setup_info
SF=$NSP                                   # 2: spinors, one electron per band
GBASE=800; [ -n "$GTERM" ] && GBASE=400            # converged for WS2: 800 bands, or 400 with the BG terminator
GW_BANDS=${GW_BANDS:-$((GBASE * SF))}
WB=${WB:-$((100 * SF))}
BANDS=${BANDS:-$([ "$SF" = 2 ] && echo 2:4 || echo 1:2)}
NV=${BANDS%%:*}; NC=${BANDS##*:}
BLO=$((VB - NV + 1)); BHI=$((VB + NC))
NTR=$((NKBZ * NV * NC))
[ -n "$DW" ] || { [ $NTR -le 1000 ] && DW=0.002 || DW=0.005; }
if [ -z "$QS" ]; then   # each scanned frequency diagonalizes a 2N x 2N Casida matrix read from text (~1 GB at 2N = 5000)
  if [ $NTR -le 1000 ]; then QS=$(seq -s ' ' 1 $NQ); else QS=1; QS_NOTE=1; fi
fi

plan () {
  echo "=== $(basename "$TOP"): SAVE $NB_SAVE bands, ${NGRID:-?}x${NGRID:-?} grid, $NK IBZ k, $NQ q, VB $VB, $([ $SF = 2 ] && echo spinors \(SOC\) || echo no SOC); $NP MPI tasks x $OMP_NUM_THREADS threads, one-task stages $NT1 threads ==="
  echo "    GW   : PPA, EXX $GW_EXX Ry, $GW_BANDS bands${GTERM:+ + $GTERM terminator}, block $GW_BLOCK Ry, RIM-W, QP bands $BLO-$BHI at all $NK k"
  echo "    W    : static screening, $WB bands, block $XBLK Ry"
  echo "    ref  : BSE bands $BLO-$BHI ($BANDS), TDA, exchange $EXX_REF Ry (all G), W block $WBLK Ry, RIM-W"
  echo "    kernel: coupled BSE + export at q = 1..$NQ, exchange/kernel $NG_X (in-plane G), $NTR resonant transitions per q (Casida matrix $((2 * NTR)))"
  echo "    scan : Casida at q = $QS"
  [ -n "${QS_NOTE:-}" ] && echo "           (large matrices: scan the other q in parallel jobs, e.g. STAGE=scan QS=\"2 3 4 5\" sbatch ...)"
  return 0
}
plan
msg=""
[ "$GW_BANDS" -le "$NB_SAVE" ] || msg="$msg\n!!! GW_BANDS=$GW_BANDS > the $NB_SAVE bands of the SAVE: redo the nscf with nbnd >= $GW_BANDS"
[ "$WB" -le "$NB_SAVE" ] || msg="$msg\n!!! WB=$WB > the $NB_SAVE bands of the SAVE"
[ "$BHI" -le "$WB" ] || msg="$msg\n!!! BSE bands up to $BHI need WB >= $BHI"
[ "$NV" -le "$VB" ] || msg="$msg\n!!! $NV valence bands requested, there are $VB"
awk -v a=$WBLK -v b=$XBLK 'BEGIN{exit !(a > b)}' && msg="$msg\n!!! WBLK $WBLK Ry > XBLK $XBLK Ry"
[ -z "$msg" ] || { printf "$msg\n"; echo "!!! nothing was run"; exit 1; }

# --- MPI layouts (Yambo aborts when a role has more tasks than elements; see gw_converge.sh) ---
split () {   # ntasks cap1 cap2 ... -> tasks per role, greedy largest divisor; nothing if impossible
  local r=$1 out="" c d; shift
  for c in "$@"; do
    for ((d = c < r ? c : r; d > 1; d--)); do [ $((r % d)) -eq 0 ] && break; done
    [ "$d" -lt 1 ] && d=1
    out="$out $d"; r=$((r / d))
  done
  [ "$r" -eq 1 ] && echo $out
}
fit () {     # ntasks cap1 cap2 cap3 -> a b c, a*b*c = ntasks, each <= its cap
  local n=$1 c1=$2 c2=$3 c3=$4 a b c
  for ((a = c1 < n ? c1 : n; a >= 1; a--)); do (( n % a )) && continue
    for ((b = c2 < n / a ? c2 : n / a; b >= 1; b--)); do (( (n / a) % b )) && continue
      c=$((n / a / b)); (( c <= c3 )) && { echo $a $b $c; return 0; }
    done
  done
  return 1
}
NQP=$((NK * (BHI - BLO + 1)))
par_gw () {   # nbands -> DIP, X, SE lines of the GW
  [ "$NP" -gt 1 ] || return 0
  local nc=$(( $1 - VB )) d x s
  d=($(split $NP $nc $NK)); [ ${#d[@]} -eq 2 ] || { echo "!!! $NP tasks do not fit the GW dipoles: c <= $nc, k <= $NK" >&2; return 1; }
  x=($(split $NP $nc $NKBZ $NQ)); [ ${#x[@]} -eq 3 ] || { echo "!!! $NP tasks do not fit the GW screening: c <= $nc, k <= $NKBZ, q <= $NQ" >&2; return 1; }
  s=($(split $NP $BHI $NQ $NQP)); [ ${#s[@]} -eq 3 ] || { echo "!!! $NP tasks do not fit the self-energy: b <= $BHI, q <= $NQ, qp <= $NQP" >&2; return 1; }
  printf 'DIP_CPU= "%d %d 1"\nDIP_ROLEs= "k c v"\n' ${d[1]} ${d[0]}
  printf 'X_and_IO_CPU= "%d 1 %d %d 1"\nX_and_IO_ROLEs= "q g k c v"\n' ${x[2]} ${x[1]} ${x[0]}
  printf 'SE_CPU= "%d %d %d"\nSE_ROLEs= "q qp b"\n' ${s[1]} ${s[2]} ${s[0]}
}
par_w () {    # DIP and X lines of the static screening (DipBands 1..WB in every BSE run)
  [ "$NP" -gt 1 ] || return 0
  local nc=$((WB - VB)) d x
  d=($(fit $NP $nc $NK $VB)) || { echo "!!! $NP tasks do not fit the dipoles: c <= $nc, k <= $NK, v <= $VB" >&2; return 1; }
  x=($(fit $NP $nc $NKBZ $NQ)) || { echo "!!! $NP tasks do not fit the screening: c <= $nc, k <= $NKBZ, q <= $NQ" >&2; return 1; }
  printf 'DIP_CPU= "%d %d %d"\nDIP_ROLEs= "k c v"\n' ${d[1]} ${d[0]} ${d[2]}
  printf 'X_and_IO_CPU= "%d 1 %d %d 1"\nX_and_IO_ROLEs= "q g k c v"\n' ${x[2]} ${x[1]} ${x[0]}
}
par_bs () {   # BS lines of the reference BSE
  [ "$NP" -gt 1 ] || return 0
  local t b; t=$(( NC * VB - (VB - NV + 1) + 1 ))
  b=($(fit $NP $NK $((NV * NC)) $t)) || b=($(fit $NP $NK $NP $t))
  printf 'BS_CPU= "%d %d %d"\nBS_ROLEs= "k eh t"\n' ${b[0]} ${b[1]} ${b[2]}
}
for l in "$(par_gw $GW_BANDS 2>&1 >/dev/null)" "$(par_w 2>&1 >/dev/null)"; do
  [ -z "$l" ] || { echo "$l"; echo "!!! nothing was run (change the number of tasks)"; exit 1; }
done

# --- runs (in prod/) ---
launch () {   # job jobdirs done-test [one] : yambo on prod/<job>.in; "one" = one task, NT1 threads
  local job=$1 js=$2 done=$3 run="$RUNNER" yf="$YFLAGS" nt=$OMP_NUM_THREADS e try
  [ "$4" = one ] && { run=""; yf=$NOMPI; nt=$NT1; }
  for try in 1 2 3; do
    ( cd $P && OMP_NUM_THREADS=$nt $run yambo $yf -F $job.in -J "$js" > $job.out 2>&1 )
    ( cd $P && eval "$done" ) && return 0
    # Yambo race (fixed in the fork): the master writes ndb.cutoff while a slower task reads it
    grep -qs "Reading File .*ndb.cutoff" $P/LOG/l-${job}_* $P/l-${job}_* || break
    [ $try -lt 3 ] || break
    echo "  $job: ndb.cutoff read while being written (MPI race): retrying"
    rm -rf $P/$job/ndb.cutoff* $P/[rl]-${job}_* $P/LOG/l-${job}_*
  done
  echo "!!! $job failed: see prod/$job.out and prod/LOG/l-${job}_* (or prod/l-${job}_*)"
  e=$(grep -h "ERROR" $P/LOG/l-${job}_* $P/l-${job}_* 2>/dev/null | sed 's/.*\[ERROR\]/  [ERROR]/' | sort -u | head -5)
  if [ -n "$e" ]; then echo "$e"; else tail -5 $P/$job.out; fi
  return 1
}
fresh () { rm -rf $P/$1 $P/o-$1.* $P/[rl]-$1_* $P/LOG/l-$1_*; }
say () { echo "  $* ($(date +%H:%M))"; }

gw_in () {   # file nbands exx [rim lines]
  local par rw; par=$(par_gw $2) || return 1
  rw=$'rim_w\nRandGvecW= '$RIMW_G' RL'; [ -n "$GTERM" ] && rw="$rw"$'\nGTermKind= "'$GTERM'"'
  cat > $1 <<EOF
$par
$rw
HF_and_locXC
gw0
ppa
el_el_corr
dyson
rim_cut
em1d
X_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
SE_Threads= $OMP_NUM_THREADS
$CUT
${4:-$RIM}
$FFTGL
EXXRLvcs= $3 Ry
VXCRLvcs= $3 Ry
Chimod= "HARTREE"
% BndsRnXp
 1 | $2 |
%
NGsBlkXp= $GW_BLOCK Ry
% LongDrXp
 1.0 | 1.0 | 0.0 |
%
$PPA
% GbndRnge
 1 | $2 |
%
DysSolver= "n"
%QPkrange
 1|$NK|$BLO|$BHI|
%
$EXTRA_GW
EOF
}

w_in () {
  local par; par=$(par_w) || return 1
  cat > $1 <<EOF
em1s
rim_cut
X_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
$par
$CUT
$RIM
$FFTGL
Chimod= "hartree"
% DipBands
 1 | $WB |
%
% BndsRnXs
 1 | $WB |
%
NGsBlkXs= $XBLK Ry
% LongDrXs
 1.0 | 0.0 | 0.0 |
%
EOF
}

bse_in () {   # file kind(ref|kx|cas) q1 q2 [extra lines...]
  local f=$1 kind=$2 q1=$3 q2=$4 lines par="" lo hi ns dm; shift 4
  case $kind in
    ref) par=$(par_w | grep DIP; par_bs)
         lines=$'rim_w\nRandGvecW= '$RIMW_G$' RL\nBSKmod= "SEX"\nBSEmod= "resonant"\nBSENGexx= '$EXX_REF$' Ry\nBSENGBlk= '$WBLK' Ry'
         lo=$(python3 -c "print(f'{max(0.05, $GDIR - 1.5):.3f}')"); hi=$(python3 -c "print(f'{$GDIR + 0.5:.3f}')")
         ns=$(python3 -c "print(int(round(($hi - $lo) / $DE)) + 1)"); dm=$ETA ;;
    kx)  lines=$'rim_w\nRandGvecW= '$RIMW_G$' RL\nBSKmod= "SEX"\nBSEmod= "coupling"\nBSENGexx= '$NG_X$'\nBSENGfxc= '$NG_X$'\nBSENGBlk= '$WBLK$' Ry\nBSEChiOut= "exc"\nBSEGinplane'
         lo=$WLO; hi=$WHI; ns=$NW; dm=0.0 ;;
    cas) lines=$'BSKmod= "CHI"\nBSEmod= "coupling"\nBSENGexx= '$NG_X$'\nBSENGfxc= '$NG_X$'\nBSEChiDyn\nBSEGinplane'
         lo=$WLO; hi=$WHI; ns=$NW; dm=0.0 ;;
  esac
  cat > $f <<EOF
optics
bse
bss
rim_cut
$lines
BSSmod= "d"
K_Threads= $([ $kind = ref ] && echo $OMP_NUM_THREADS || echo $NT1)
DIP_Threads= $([ $kind = ref ] && echo $OMP_NUM_THREADS || echo $NT1)
$par
$CUT
$RIM
$FFTGL
% BSEQptR
 $q1 | $q2 |
%
% DipBands
 1 | $WB |
%
% BSEBands
 $BLO | $BHI |
%
% BEnRange
 $lo | $hi | eV
%
% BDmRange
 $dm | $dm | eV
%
BEnSteps= $ns
% BLongDir
 1.0 | 0.0 | 0.0 |
%
BSEprop= "abs"
KfnQPdb= "E < gw/ndb.QP"
$EXTRA_BSE
EOF
  for l in "$@"; do printf '%s\n' "$l" >> $f; done
}

# report lines of a BSE run: rep <job> <iq> <pattern> -> last field (per momentum block)
rep () { awk -v q=$2 -v p="$3" '/BSE momentum index/{i=$NF} (i==q || i=="") && index($0,p){print $NF}' $P/r-$1_* 2>/dev/null | tail -1; }
rep_low () { awk -v q=$2 '/BSE momentum index/{i=$NF} (i==q || i=="") && /lowest excitation energies/{print $6}' $P/r-$1_* 2>/dev/null | tail -1; }
qp_gaps () {  # -> direct and indirect QP gap (eV) between VB and CB, from prod/o-gw.qp
  python3 - $P/o-gw.qp $VB $CB <<'PY'
import sys
f, vb, cb = sys.argv[1], int(sys.argv[2]), int(sys.argv[3])
E = {}
for l in open(f):
    if l.lstrip().startswith('#') or not l.strip():
        continue
    x = l.split()
    E[(int(float(x[0])), int(float(x[1])))] = float(x[2]) + float(x[3])
ks = sorted({k for k, b in E})
v = {k: E[(k, vb)] for k in ks if (k, vb) in E}; c = {k: E[(k, cb)] for k in ks if (k, cb) in E}
print(f'{min(c[k] - v[k] for k in v if k in c):.4f} {min(c.values()) - max(v.values()):.4f}')
PY
}
grid () {     # WLO WHI NW of the export: below the indirect QP gap, down to WMARGIN under the exciton
  python3 -c "
import math
lo, hi = $1 - $WMARGIN, $2 - 2 * $DW
lo = math.floor(lo / $DW) * $DW
print(f'{lo:.3f} {hi:.3f} {int(round((hi - lo) / $DW)) + 1}')"
}

# --------------------------------------------------------------------------------------------
case $STAGE in setup) echo "=== setup done: nothing else run"; exit 0 ;; esac

# ---------- GW ----------
case $STAGE in gw|all)
  if [ -s $P/gw/ndb.QP ] && [ -s $P/o-gw.qp ]; then echo "=== GW done"
  else
    echo "=== GW ($(date +%H:%M)) ==="
    if [ "$CHECK" = 1 ]; then   # cheap GW with the same MPI layout, block and RIM-W: catches an abort in
      ref=$(par_gw $GW_BANDS); nbs=$GW_BANDS     # the self-energy step before hours of screening
      for ((t = BHI + 1; t < GW_BANDS; t++)); do [ "$(par_gw $t 2>/dev/null)" = "$ref" ] && { nbs=$t; break; }; done
      fresh gw_check
      gw_in $P/gw_check.in $nbs 5 $'RandQpts= 1000\nRandGvec= 1 RL' || exit 1
      say "check: cheap GW, $nbs bands, same MPI layout, block and RIM-W"
      launch gw_check gw_check "ls o-gw_check.qp >/dev/null 2>&1" || {
        grep -qs "for RIM-W is higher" $P/LOG/l-gw_check_* $P/l-gw_check_* &&
          echo "!!! RIM-W needs more G-vectors than the $GW_BLOCK Ry block has: raise GW_BLOCK or lower RIMW_G"
        echo "!!! the check failed: nothing else was run"; exit 1; }
      fresh gw_check; rm -f $P/gw_check.*
      say "check OK"
    fi
    fresh gw; gw_in $P/gw.in $GW_BANDS $GW_EXX || exit 1
    say "gw: $GW_BANDS bands, QP bands $BLO-$BHI at $NK k"
    launch gw gw "ls o-gw.qp >/dev/null 2>&1 && ls gw/ndb.QP >/dev/null 2>&1" || exit 1
  fi
  read GDIR GIND <<< $(qp_gaps)
  echo "  QP gap: direct $GDIR eV, indirect $GIND eV" ;; esac
[ -s $P/o-gw.qp ] || { echo "!!! prod/o-gw.qp missing: run STAGE=gw first"; exit 1; }
read GDIR GIND <<< $(qp_gaps)

# ---------- static screening ----------
case $STAGE in w|ref|export|all)
  if ls $P/w/ndb.em1s >/dev/null 2>&1; then echo "=== screening done"
  else
    echo "=== static screening ($(date +%H:%M)) ==="
    fresh w; w_in $P/w.in || exit 1
    say "w: $WB bands, block $XBLK Ry"
    launch w w "ls w/ndb.em1s >/dev/null 2>&1" || exit 1
  fi ;; esac

# ---------- reference BSE ----------
case $STAGE in ref|export|all)
  if [ -n "$(rep bse_ref 1 'binding energy, gap - lowest bright')" ]; then echo "=== reference BSE done"
  else
    echo "=== reference BSE ($(date +%H:%M)) ==="
    fresh bse_ref; bse_in $P/bse_ref.in ref 1 1 || exit 1
    say "bse_ref: TDA, bands $BLO-$BHI, exchange $EXX_REF Ry, W block $WBLK Ry, RIM-W"
    launch bse_ref "bse_ref,w" "ls r-bse_ref_* >/dev/null 2>&1 && grep -q 'binding energy, gap' r-bse_ref_*" || exit 1
    grep -qs "break level degeneracy" $P/LOG/l-bse_ref_* $P/l-bse_ref_* &&
      echo "  note: BSE bands $BLO-$BHI split a degenerate pair at some k (Yambo warning): consider another BANDS window"
  fi
  echo "  reference BSE: lowest $(rep_low bse_ref 1) eV, A $(rep bse_ref 1 'lowest bright excitation (strength') eV, Eb $(rep bse_ref 1 'binding energy, gap - lowest bright') eV" ;; esac

# frequency grid of the kernel (fixed once the export has run: read back from its input)
if [ -s $P/kx_bse.in ] && [ -s $P/kx/ndb.Chi ]; then
  read WLO WHI <<< $(awk '/BEnRange/{getline; print $1, $3}' $P/kx_bse.in); NW=$(awk -F= '/BEnSteps/{print $2+0}' $P/kx_bse.in)
else
  EL=$(rep_low bse_ref 1)
  case $STAGE in export|scan|summary|all) [ -n "$EL" ] || { echo "!!! no reference BSE: run STAGE=ref first"; exit 1; } ;; esac
  [ -n "$EL" ] && read WLO WHI NW <<< $(grid $EL $GIND)
fi

# ---------- kernel export ----------
case $STAGE in export|all)
  if [ -s $P/kx/ndb.Chi ] && [ -f $P/kx/ok ]; then echo "=== kernel export done ($NW frequencies $WLO..$WHI eV)"
  else
    echo "=== kernel export: q = 1..$NQ, $NW undamped frequencies $WLO..$WHI eV (indirect QP gap $GIND eV), $NT1 threads ($(date +%H:%M)) ==="
    fresh kx_bse; rm -rf $P/kx
    bse_in $P/kx_bse.in kx 1 $NQ || exit 1
    kx_done () { ls kx_bse/ndb.Chi >/dev/null 2>&1 && [ "$(awk '/BSE momentum index/{n=$NF} END{print n}' r-kx_bse_* 2>/dev/null)" = $NQ ] &&
                 [ "$(grep -c 'lowest excitation energies' r-kx_bse_* 2>/dev/null | awk -F: '{s+=$NF} END{print s}')" -ge $NQ ]; }
    launch kx_bse "kx_bse,w" kx_done one || exit 1
    mkdir -p $P/kx; cp $P/kx_bse/ndb.Chi* $P/kx/
    # every q: exported response = BSE response, in-plane G reached, excitons inside the grid
    python3 - $P $NQ $WLO $WHI $DWIN <<'PY' || exit 1
import glob, sys
p, nq, wlo, whi, dwin = sys.argv[1], int(sys.argv[2]), *map(float, sys.argv[3:])
q, d, bad, ns = None, {}, [], 0
for f in glob.glob(f'{p}/r-kx_bse_*'):
    for l in open(f, errors='ignore'):
        if 'in-plane G of the basis' in l: ns = int(l.split()[-2])     # printed just before the q line
        if 'BSE momentum index' in l: q = int(l.split()[-1]); d.setdefault(q, {})['ns'] = ns
        if q is None: continue
        if 'head of the exported response' in l: d[q]['head'] = [float(x) for x in l.split(':')[-1].split()]
        if 'minimum rcond' in l: d[q]['rcond'] = [float(x) for x in l.split(':')[-1].split()]
        if 'lowest excitation energies' in l: d[q]['low'] = float(l.split(':')[-1].split()[0])
        if 'lowest bright excitation (strength' in l: d[q]['br'] = float(l.split()[-1])
        if 'lowest transition energy of the window' in l: d[q]['gap'] = float(l.split()[-1])
print('    q   head residual  |scale/c|   in-plane G   rcond          lowest   bright    gap')
for iq in range(1, nq + 1):
    r = d.get(iq, {})
    h, ns, rc = r.get('head', [9, 0, 0]), r.get('ns', 0), r.get('rcond', [0, 0])
    lo, br, gap = r.get('low', 0), r.get('br', 0), r.get('gap', 0)
    print(f'  {iq:3d}   {h[0]:11.2e}  {h[1]:9.6f}   {ns:5d}      {min(rc):9.2e}   {lo:8.4f} {br:8.4f} {gap:8.4f}')
    if not r: bad.append(f'q{iq}: no report')
    if h[0] > 1e-4 or abs(h[1] - 1) > 1e-3: bad.append(f'q{iq}: exported response differs from the BSE response')
    if ns < 7: bad.append(f'q{iq}: only {ns} in-plane G (need G=0 + the first star, 7): raise NG_X')
    if min(lo, br) - dwin < wlo: bad.append(f'q{iq}: exciton {min(lo, br):.4f} eV below the grid ({wlo} eV): raise WMARGIN')
    if whi >= gap: bad.append(f'q{iq}: grid reaches the gap {gap:.4f} eV')
for b in bad: print('  !!!', b)
sys.exit(1 if bad else 0)
PY
    touch $P/kx/ok
    rm -f $P/kx_bse/ndb.BS_*        # the BSE kernel (large, not needed by the Casida runs)
  fi ;; esac

# ---------- Casida with the kernel at every q ----------
case $STAGE in scan|all)
  [ -f $P/kx/ok ] || { echo "!!! no checked kernel in prod/kx: run STAGE=export"; exit 1; }
  for q in $QS; do
    [ -s $P/sc_q$q.dat ] && [ -s $P/static_q$q.log ] && continue
    lo=$(rep_low kx_bse $q); br=$(rep kx_bse $q 'lowest bright excitation (strength')
    [ -n "$lo" ] && [ -n "$br" ] || { echo "!!! q$q: no BSE excitons in prod/r-kx_bse_*"; exit 1; }
    read W1 W2 <<< $(python3 -c "print(f'{max(min($lo,$br)-$DWIN,$WLO):.4f} {min(max($lo,$br)+$DWIN,$WHI):.4f}')")
    say "q$q: BSE lowest $lo, bright $br eV: static Casida, and self-consistent on $W1..$W2 eV"
    fresh xs_q$q; bse_in $P/xs_q$q.in cas $q $q '% BSEChiKoutW' ' 0.0 | 0.0 | eV' '%'
    launch xs_q$q "xs_q$q,kx,w" "ls o-xs_q$q.Ktt_q${q}_w1 >/dev/null 2>&1" one || exit 1
    ( cd $P && python3 $T/ktt_casida.py o-xs_q$q.Ktt_q${q}_transitions o-xs_q$q.Ktt_q${q}_w1 > static_q$q.log ) || exit 1
    rm -f $P/o-xs_q$q.Ktt_q${q}_w*
    fresh xw_q$q; bse_in $P/xw_q$q.in cas $q $q '% BSEChiKoutW' " $W1 | $W2 | eV" '%'
    launch xw_q$q "xw_q$q,kx,w" "ls o-xw_q$q.Ktt_q${q}_w[0-9]* >/dev/null 2>&1" one || exit 1
    ( cd $P && python3 $T/sc_scan.py o-xw_q$q.Ktt_q${q}_transitions o-xw_q$q.Ktt_q${q}_w* \
                --out sc_q$q.dat --exciton $br --delete > sc_q$q.log 2>&1 ) || { tail -5 $P/sc_q$q.log; exit 1; }
    grep "= w at\|BSE = \|no crossing\|!!!" $P/sc_q$q.log | sed 's/^/    /'
    rm -rf $P/xs_q$q $P/xw_q$q
  done ;; esac

# ---------- summary ----------
case $STAGE in summary|all)
  [ -f $P/kx/ok ] || { echo "!!! no checked kernel: run STAGE=export"; exit 1; }
  cd $P
  python3 $T/fxc_export.py kx --out fxc_GGq_static --static --sym --inplane > fxc_static.out || { tail fxc_static.out; exit 1; }
  python3 $T/fxc_export.py kx --out fxc_GGq_allw --sym --inplane > fxc_allw.out || { tail fxc_allw.out; exit 1; }
  if ls sc_q*.dat >/dev/null 2>&1; then
    python3 $T/xq_summary.py --bse r-kx_bse_* --npz fxc_GGq_static.npz --out xq_dispersion > xq_summary.out || { tail xq_summary.out; exit 1; }
    [ -s xq_dispersion_omega.dat ] && python3 $T/fxc_export.py kx --at xq_dispersion_omega.dat --sym --inplane --out fxc_GGq_scw > fxc_scw.out
  fi
  python3 - "$P" "$(basename "$TOP")" "$GDIR" "$GIND" "$SF" "$NGRID" "$BLO-$BHI" \
    "$(awk -F= '/BSENGexx/{print $2; exit}' bse_ref.in 2>/dev/null)" "$(awk -F= '/BSENGexx/{print $2; exit}' kx_bse.in 2>/dev/null)" > summary.txt <<'PY'
import glob, sys
import numpy as np
p, name, gdir, gind, sf, ngrid, bands, exx, ngx = sys.argv[1:]
def rep(job, key, q=1):
    i, v = None, None
    for f in glob.glob(f'{p}/r-{job}_*'):
        for l in open(f, errors='ignore'):
            if 'BSE momentum index' in l: i = int(l.split()[-1])
            if (i in (None, q)) and key in l: v = l
    return v
num = lambda l, k=-1: float(l.split()[k]) if l else float('nan')
print(f'{name}: {ngrid}x{ngrid} k grid, {"spinors (SOC)" if sf == "2" else "no SOC"}, BSE bands {bands}')
print(f'QP gap (G0W0, RIM-W): direct {gdir} eV, indirect {gind} eV')
for job, what in (('bse_ref', f'reference BSE (TDA, exchange{exx} on all G)'),
                  ('kx_bse', f'export BSE (coupling, exchange{ngx} on in-plane G), q1')):
    a = num(rep(job, 'lowest bright excitation (strength')); g = num(rep(job, 'lowest transition energy of the window'))
    lo = rep(job, 'lowest excitation energies'); lo = float(lo.split(':')[-1].split()[0]) if lo else float('nan')
    print(f'{what}:\n    lowest {lo:.4f} eV, A (lowest bright) {a:.4f} eV, gap {g:.4f} eV, Eb(A) {g - a:.4f} eV')
# Casida with fxc(q, w) against the BSE, per q: every BSE state of the export report (its six lowest)
# inside the scanned window and visible in the density response (strength >= 1e-4 of the brightest)
# must be a self-consistent solution lambda(w) = w. States with strength < 1e-6 (spin-forbidden dark
# excitons with SOC) have no pole in P, so no density kernel reproduces them: listed, not checked.
bse, q = {}, None
for f in glob.glob(f'{p}/r-kx_bse_*'):
    for l in open(f, errors='ignore'):
        if 'BSE momentum index' in l: q = int(l.split()[-1])
        if q and 'lowest excitation energies' in l: bse.setdefault(q, {})['E'] = [float(v) for v in l.split(':')[-1].split()]
        if q and 'their relative optical strength' in l: bse.setdefault(q, {})['S'] = [float(v) for v in l.split(':')[-1].split()]
rows, worst, dark = [], 0.0, []
for f in sorted(glob.glob(f'{p}/sc_q*.log'), key=lambda x: int(x.split('sc_q')[-1][:-4])):
    iq = int(f.split('sc_q')[-1][:-4])
    xs = []
    for l in open(f):
        if l.startswith('lambda_') and '= w at' in l:
            xs += [float(v) for v in l.split('= w at')[1].split('eV')[0].split(',')]
    try:
        w = np.loadtxt(f'{p}/sc_q{iq}.dat', ndmin=2)[:, 0]
    except Exception:
        continue
    # the scan follows the lowest eigenvalue and the lowest bright one (> 1%): check the BSE lowest
    # visible state and the BSE lowest bright state
    ES = list(zip(bse.get(iq, {}).get('E', []), bse.get(iq, {}).get('S', [])))
    dark += [(iq, E) for E, S in ES if S < 1e-6 and w.min() <= E <= w.max()]
    want = [next(((E, S) for E, S in ES if S >= 1e-4), None), next(((E, S) for E, S in ES if S > 1e-2), None)]
    for E, S in {x for x in want if x and w.min() <= x[0] <= w.max()}:
        d = min((abs(x - E) for x in xs), default=np.inf) * 1000
        worst = max(worst, d); rows.append((iq, E, S, d))
if rows:
    print(f'Casida with fxc(q, w) vs BSE (lowest visible and lowest bright state of each q): {len(rows)} states at {len({r[0] for r in rows})} q,'
          f' max |self-consistent - BSE| = {worst:.2f} meV (should be ~1 meV or less)')
    for iq, E, S, d in rows:
        if d > 1:
            print(f'    q{iq}: BSE state {E:.4f} eV (strength {S:.1e}) found only to {d:.1f} meV: near a pole of fxc(w)?'
                  ' a finer DW (re-export) resolves it')
    if dark:
        print(f'  density-dark BSE states (strength < 1e-6, e.g. spin-forbidden with SOC), not in P, so not in any'
              f' density-kernel TDDFT: ' + ', '.join(f'q{i} {E:.4f}' for i, E in dark[:8]) + (' ...' if len(dark) > 8 else ''))
    try:
        x = np.loadtxt(f'{p}/xq_dispersion.dat', ndmin=2)
        print(f'q1: Eb(A) self-consistent {x[0, 9]:.4f} eV (BSE {x[0, 8]:.4f}), static fxc(q1, 0) {x[0, 10]:.4f} eV')
    except Exception:
        pass
else:
    print('no self-consistent Casida results yet (STAGE=scan)')
print('kernel files (prod/): fxc_GGq_static.*, fxc_GGq_allw.*' + (', fxc_GGq_scw.*' if glob.glob(f'{p}/fxc_GGq_scw*') else ''))
PY
  cat summary.txt
  ls -lh $P/fxc_GGq_* $P/xq_dispersion* 2>/dev/null | awk '{n = split($NF, a, "/"); print "  " $5 "  prod/" a[n]}' ;; esac
