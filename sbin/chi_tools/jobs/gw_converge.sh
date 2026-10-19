#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --cpus-per-task=2
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=48:00:00
#SBATCH --job-name=gw_conv
#SBATCH --output=log/gw_conv.o%A
#SBATCH --error=log/gw_conv.e%A

# G0W0 (plasmon pole) convergence for monolayer WS2 in Yambo. Converges the quasiparticle
# band gap (direct gap at the k-point of the KS minimum gap, normally K) one parameter at a
# time, each at fixed values of the others:
#
#   STAGE=exx    : exchange cutoff EXXRLvcs (Ry)            list EXX_LIST (exchange only: minutes)
#   STAGE=bands  : bands in screening and self-energy N      list BANDS_LIST (BndsRnXp = GbndRnge = 1..N)
#   STAGE=block  : screening matrix cutoff NGsBlkXp (Ry)     list BLOCK_LIST
#   STAGE=grid   : bands x block together (the two are coupled)   BANDS_LIST x BLOCK_LIST
#   STAGE=dirs   : the same GW in other directories (k grids, vacuum sizes) DIRS="dir1 dir2 ..."
#                  each with its own SAVE (nscf + p2y + setup done by you); "." is this directory
#
# W=mpa replaces the plasmon pole (one pole fitted at 0 and PPAPntXp) by the multipole approximation
# (Leon et al., PRB 104, 115157 (2021)): MPA_NF/2 poles fitted on a complex-frequency grid, close to
# full frequency at a fraction of its cost (about MPA_NF/2 times a plasmon-pole run). Job names get
# _mpa (_mpa<MPA_NF> for MPA_NF other than 16). Yambo 5.3 does not run RIM-W with MPA (rim_w is
# switched off), so the plasmon-pole error is measured without RIM-W: final (plasmon pole) against
# final_mpa, same grid and parameters, and added to the RIM-W result as a correction.
# QPBANDS="25-28" computes QP energies for more bands than VB, CB (the gap still uses VB, CB); the
# summary then also prints the splittings VB-(VB-1) and (CB+1)-CB at the gap k (the SOC splittings
# at K with a spin-orbit SAVE).
#
# RIMW=1 adds RIM-W (runlevel rim_w): the head and wings of W near q=0 are integrated over each
# small BZ instead of taken at the q-point, which is what makes coarse k grids of 2D systems
# converge (Guandalini et al., npj Comput. Mater. 9, 44 (2023)). Yambo uses it in the plasmon-
# pole self-energy with the slab cutoff, as here. RandGvecW=RIMW_G (default 15 RL) G-vectors of W
# are integrated; they must fit in the screening block. Runs with RIM-W are named <job>_rimw (with GTERM=BG <job>_bg, <job>_rimw_bg), so
# they sit next to the runs without it (e.g. RIMW=1 STAGE=dirs DIRS=". k18").
#   STAGE=summary: tables, convergence flags, 1/N extrapolation in bands, plot
#   STAGE=all    : exx, bands, block, summary
#
# Run from the directory with SAVE (after p2y and the yambo setup). The SAVE must contain at
# least max(BANDS_LIST) bands: for 1000 bands, redo the QE nscf with nbnd >= 1000.
# Every run writes conv/<scan>_<value>/ and its o-*.qp; nothing is overwritten between scans,
# so stages can be split over several jobs (e.g. STAGE=bands BANDS_LIST="1200 1400" ...).
#
# Speed:
#   - MPI: with more than one SLURM task the runs use srun (or RUNNER). The script picks the
#     DIP, X and SE layouts itself so that no role gets more tasks than it has elements (see
#     "MPI layouts" below), refuses to start if some run of the stage cannot fit, and tries each
#     new layout on a cheap GW first (a few minutes; CHECK=0 skips it). A stage stops at its
#     first failed run and prints Yambo's error. Use 2 nodes with
#     sbatch --nodes=2 gw_converge.sh (24 tasks x 2 threads per node by default).
#   - Only the k-point of the minimum KS direct gap is computed (QPK=all for every k).
#   - The exchange scan is exchange-only (HF): the correlation does not depend on EXXRLvcs.
#   - FFTG="40 Ry" (FFTGvecs) shrinks the FFT grid of the wavefunctions; check it on one run.
#
# Defaults (override on the command line, e.g. EXX0=60 BLOCK0=8 sbatch gw_converge.sh):
#   2D Coulomb cutoff CUTGeo="slab z" with RIM for q->0; plasmon pole at 27.2 eV; no band terminator.
#   GTERM=BG adds the Bruneval-Gonze terminator (GTermKind="BG"), which converges faster in the
#   number of bands; runs with and without it are not comparable, so keep one choice per scan.
#   (Up to 2026-10-18 this variable was called TERM, which is the shell's terminal type: the BG
#   terminator was never set and the inputs got a stray line such as "xterm-256color".)

STAGE=${STAGE:-all}
EXX_LIST=${EXX_LIST:-"20 40 60 80 100"}          # Ry
BANDS_LIST=${BANDS_LIST:-"200 400 600 800 1000"}
BLOCK_LIST=${BLOCK_LIST:-"4 6 8 10 12"}           # Ry
EXX0=${EXX0:-40}; BANDS0=${BANDS0:-800}; BLOCK0=${BLOCK0:-12}  # fixed values while scanning the others
RIMW=${RIMW:-0}                                   # 1: RIM-W in the GW runs (job names get _rimw)
RIMW_G=${RIMW_G:-15}                              # RandGvecW, RL
TOL=${TOL:-0.01}                                  # eV, convergence threshold on the gap
CUT=${CUT:-'CUTGeo= "slab z"'}
RIM=${RIM:-$'RandQpts= 1000000\nRandGvec= 100 RL'}
GTERM=${GTERM:-}                                  # BG: Bruneval-Gonze terminator; empty: none
W=${W:-ppa}                                       # ppa: plasmon pole; mpa: multipole approximation (job names get _mpa)
MPA_NF=${MPA_NF:-16}                              # MPA: frequencies sampled (ETStpsXm), 2 per pole
QPBANDS=${QPBANDS:-}                              # QP bands "lo-hi" (e.g. 25-28 for the SOC splittings); default VB-CB
PPA=${PPA:-'PPAPntXp= 27.21138 eV'}
EXTRA_GW=${EXTRA_GW:-""}                         # any extra input lines
# run names: <job>[_rimw][_bg][_mpa], so runs with different settings never overwrite each other
case $W in ppa|mpa) ;; *) echo "!!! W=$W: ppa or mpa"; exit 1 ;; esac
JS=""; [ "$RIMW" = 1 ] && JS="_rimw"
[ -n "$GTERM" ] && JS="${JS}_$(echo "$GTERM" | tr A-Z a-z)"
[ "$W" = mpa ] && { [ "$MPA_NF" = 16 ] && JS="${JS}_mpa" || JS="${JS}_mpa$MPA_NF"; }
[ "$W" = mpa ] && [ "$RIMW" = 1 ] && { echo "!!! W=mpa with RIMW=1: Yambo 5.3 switches rim_w off for MPA (src/interface/INIT_barriers.F), and forcing it on gave inconsistent results; run MPA without RIM-W and compare it with the plasmon pole without RIM-W"; exit 1; }
DIRS=${DIRS:-""}
YFLAGS=${YFLAGS-auto}                            # auto: -nompi for one task, if this yambo has it (MPI builds)
RUNNER=${RUNNER:-""}                              # MPI launcher; default srun when SLURM gives more than one task
QPK=${QPK:-gap}                                   # gap: only the k of the minimum KS direct gap; all: every k
FFTG=${FFTG:-""}                                  # e.g. "40 Ry": FFTGvecs (plane waves of the wavefunctions)

if type module >/dev/null 2>&1 && [ -n "$SLURM_JOB_ID" ]; then
  module purge
  module use "$HOME/modulefiles"
  module load cuda_hpc_sdk/24.5
  module load hdf5/hdf5-1.14.3-cuda_hpc_sdk_24.5.0
  module load yambo/5.3-cpu
  source "$NVHPC_ROOT/comm_libs/12.4/hpcx/latest/hpcx-init.sh" && hpcx_load
fi
export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-${OMP_NUM_THREADS:-1}}
[ "$YFLAGS" = auto ] && { YFLAGS=""; yambo -h 2>&1 | grep -q nompi && YFLAGS=-nompi; }
NP=${NP:-${SLURM_NTASKS:-1}}
CHECK=${CHECK:-1}                                 # 1: test each new MPI layout with a cheap GW before the scan
if [ "$NP" -gt 1 ]; then
  # all allocated tasks: let srun take the layout (tasks per node) from the allocation, no -n
  if [ "$NP" = "${SLURM_NTASKS:-}" ]; then
    [ -n "$RUNNER" ] || RUNNER="srun --cpus-per-task=$OMP_NUM_THREADS"
  else
    [ -n "$RUNNER" ] || RUNNER="srun -n $NP --cpus-per-task=$OMP_NUM_THREADS"
  fi
  YFLAGS=""
fi
TOP="${SLURM_SUBMIT_DIR:-$PWD}"
cd "$TOP"; mkdir -p log conv

# --- system information from the yambo setup (r_setup) of a directory ---
setup_info () {   # dir -> sets NB_SAVE NK NKBZ NQ VB CB K1 K2 NQP
  local d=$1
  if [ ! -f "$d/r_setup" ]; then (cd "$d" && $RUNNER yambo $YFLAGS > log_setup.out 2>&1); fi
  NB_SAVE=$(awk -F: '/^ *Bands  /{print $2+0; exit}' "$d/r_setup")
  NK=$(awk -F: '/IBZ K-points/{print $2+0; exit}' "$d/r_setup")
  NKBZ=$(awk -F: '/BZ +K-points/ && !/IBZ/{print $2+0; exit}' "$d/r_setup")
  NQ=$(awk -F: '/IBZ Q-points/{print $2+0; exit}' "$d/r_setup")
  VB=$(awk -F: '/Filled Bands/{print $2+0; exit}' "$d/r_setup")
  [ -n "$NB_SAVE" ] && [ -n "$NK" ] && [ -n "$NKBZ" ] && [ -n "$NQ" ] && [ -n "$VB" ] || {
    echo "!!! cannot read bands/k-points/q-points from $d/r_setup"; exit 1; }
  CB=$((VB+1))
  KG=$(awk -F: '/Direct Gap localized at k/{print $2+0; exit}' "$d/r_setup")
  if [ "$QPK" = all ] || [ -z "$KG" ]; then K1=1; K2=$NK; else K1=$KG; K2=$KG; fi
  QLO=$VB; QHI=$CB
  if [ -n "$QPBANDS" ]; then QLO=${QPBANDS%-*}; QHI=${QPBANDS#*-}; fi
  [ "$QLO" -le "$VB" ] && [ "$QHI" -ge "$CB" ] || { echo "!!! QPBANDS=$QPBANDS must include $VB-$CB"; exit 1; }
  NQP=$(( (K2-K1+1) * (QHI-QLO+1) ))              # QP states of QPkrange
}

# --- MPI layouts ---
# Yambo aborts with "USER parallel structure does not fit the current run parameters" when a
# role gets more MPI tasks than it has elements (src/parallel/PARALLEL_structure.F). The element
# counts (src/parallel/PARALLEL_global_dimensions.F) for a GW run with bands 1..N:
#   DIPOLES   "k c v"     k: IBZ k-points   c: N-VB conduction bands
#   X (em1d)  "q g k c v" q: IBZ q-points   k: BZ k-points   c: N-VB
#   SE        "q qp b"    q: IBZ q-points   qp: QP states    b: max(VB, top QP band) = QHI in the
#             Fock (Sigma_x) step, N in Sigma_c. A GW run does both with the same SE_CPU, so b <= QHI.
split () {   # ntasks cap1 cap2 ... -> tasks per role, in the order given (greedy, largest divisor
  local r=$1 out="" c d; shift   #  of what is left <= cap); prints nothing if they cannot fit
  for c in "$@"; do
    for ((d = c < r ? c : r; d > 1; d--)); do [ $((r % d)) -eq 0 ] && break; done
    [ "$d" -lt 1 ] && d=1
    out="$out $d"; r=$((r / d))
  done
  [ "$r" -eq 1 ] && echo $out
}

par_se () {   # -> SE lines, valid for the Fock and the Sigma_c steps; 1 if impossible
  [ "$NP" -gt 1 ] || return 0
  local s; s=($(split $NP $QHI $NQ $NQP))         # b first, then q, then qp
  [ ${#s[@]} -eq 3 ] || { echo "!!! $NP MPI tasks do not fit the self-energy: need b x q x qp = $NP with b <= $QHI, q <= $NQ, qp <= $NQP (e.g. 24, 48, 72 or 96 tasks)" >&2; return 1; }
  printf 'SE_CPU= "%d %d %d"\nSE_ROLEs= "q qp b"\n' ${s[1]} ${s[2]} ${s[0]}
}

par_gw () {   # nbands -> DIP, X and SE lines for a GW run; 1 if impossible
  [ "$NP" -gt 1 ] || return 0
  local nc=$(( $1 - VB )) d x
  d=($(split $NP $nc $NK))                        # dipoles: c first, then k
  [ ${#d[@]} -eq 2 ] || { echo "!!! $NP MPI tasks do not fit the dipoles with $1 bands: c <= $nc, k <= $NK" >&2; return 1; }
  x=($(split $NP $nc $NKBZ $NQ))                  # screening: c first, then k, then q
  [ ${#x[@]} -eq 3 ] || { echo "!!! $NP MPI tasks do not fit the screening with $1 bands: c <= $nc, k <= $NKBZ, q <= $NQ" >&2; return 1; }
  printf 'DIP_CPU= "%d %d 1"\nDIP_ROLEs= "k c v"\n' ${d[1]} ${d[0]}
  printf 'X_and_IO_CPU= "%d 1 %d %d 1"\nX_and_IO_ROLEs= "q g k c v"\n' ${x[2]} ${x[1]} ${x[0]}
  par_se
}

gw_in () {   # file exx_Ry nbands block_Ry -> 1 if no MPI layout fits
  local f=$1 exx=$2 nb=$3 blk=$4 par rw=""
  par=$(par_gw $nb) || return 1
  [ "$RIMW" = 1 ] && rw=$'rim_w\nRandGvecW= '$RIMW_G' RL'
  [ -n "$GTERM" ] && rw="$rw"$'\nGTermKind= "'$GTERM'"'
  local XS=Xp wl="$PPA"; [ "$W" = mpa ] && { XS=Xm; wl="ETStpsXm= $MPA_NF"; }
  cat > $f <<EOF
$par
$rw
HF_and_locXC
gw0
$W
el_el_corr
dyson
rim_cut
em1d
X_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
SE_Threads= $OMP_NUM_THREADS
$CUT
$RIM
$FFTGL
EXXRLvcs= $exx Ry
VXCRLvcs= $exx Ry
Chimod= "HARTREE"
% BndsRn$XS
 1 | $nb |
%
NGsBlk$XS= $blk Ry
% LongDr$XS
 1.0 | 1.0 | 0.0 |
%
$wl
% GbndRnge
 1 | $nb |
%
DysSolver= "n"
%QPkrange
 $K1|$K2|$QLO|$QHI|
%
$EXTRA_GW
EOF
}

hf_in () {   # file exx_Ry : exchange only (Sigma_x - Vxc), for the EXXRLvcs scan; 1 if no MPI layout fits
  local par; par=$(par_se) || return 1
  cat > $1 <<EOF
HF_and_locXC
rim_cut
SE_Threads= $OMP_NUM_THREADS
$par
$CUT
$RIM
$FFTGL
EXXRLvcs= $2 Ry
VXCRLvcs= $2 Ry
%QPkrange
 $K1|$K2|$QLO|$QHI|
%
EOF
}

rmjob () {   # dir job : Yambo files of <job> (r-/l-<job>_<runlevels>), not those of <job>_rimw, <job>_bg, ...
  local f
  for f in "$1"/conv/[rl]-$2_* "$1"/conv/LOG/l-$2_*; do
    case "${f##*/}" in [rl]-$2_rimw*|[rl]-$2_bg*) ;; *) rm -f "$f" ;; esac
  done
}

fresh () {   # dir job : SAVE link in conv/, and no leftovers of an earlier failed attempt
  mkdir -p "$1/conv"; [ -e "$1/conv/SAVE" ] || ln -s ../SAVE "$1/conv/SAVE"
  rm -rf "$1/conv/$2" "$1"/conv/o-$2.*; rmjob "$1" $2
}

launch () {   # dir job ext : run yambo on conv/<job>.in; 1, with Yambo's error lines, if no o-<job>.<ext>
  local d=$1 job=$2 ext=$3 e try
  for try in 1 2 3; do
    ( cd "$d/conv" && $RUNNER yambo $YFLAGS -F $job.in -J $job > $job.out 2>&1 )
    ls "$d"/conv/o-$job.$ext >/dev/null 2>&1 && return 0
    # Yambo race: the master writes ndb.cutoff while a slower task re-reads it (coulomb/cutoff_driver.F)
    grep -qs "Reading File .*ndb.cutoff" "$d"/conv/LOG/l-${job}_* "$d"/conv/l-${job}_* || break
    [ $try -lt 3 ] || break
    echo "  $job: ndb.cutoff read while being written (Yambo race between MPI tasks): retrying"
    rm -rf "$d/conv/$job"/ndb.cutoff*; rmjob "$d" $job
  done
  echo "!!! $job failed: see $d/conv/$job.out and $d/conv/LOG/l-${job}_*"
  e=$(grep -h "ERROR" "$d"/conv/LOG/l-${job}_* "$d"/conv/l-${job}_* 2>/dev/null | sed 's/.*\[ERROR\]/  [ERROR]/' | sort -u | head -5)
  if [ -n "$e" ]; then echo "$e"; else tail -5 "$d/conv/$job.out"; fi
  return 1
}

run_hf () {   # dir job exx
  local d=$1 job=$2 exx=$3
  if ls "$d"/conv/o-$job.hf >/dev/null 2>&1; then echo "  $job done"; return 0; fi
  fresh "$d" $job
  hf_in "$d/conv/$job.in" $exx || return 1
  echo "  $job: exchange only, EXX $exx Ry ($(date +%H:%M))"
  launch "$d" $job hf
}

run_gw () {   # dir job exx nb blk ; databases in conv/<job>, outputs conv/[orl]-<job>*
  local d=$1 job=$2 exx=$3 nb=$4 blk=$5
  if ls "$d"/conv/o-$job.qp >/dev/null 2>&1; then echo "  $job done"; return 0; fi
  fresh "$d" $job
  gw_in "$d/conv/$job.in" $exx $nb $blk || return 1
  echo "  $job: EXX $exx Ry, bands $nb, block $blk Ry ($(date +%H:%M))"
  launch "$d" $job qp
}

preflight () {   # dir block_Ry nbands... : before any run of a stage, every run must fit the SAVE and
  # have an MPI layout; each layout not used before is then tried on a cheap GW (minutes): the same
  # MPI layout, block (the smallest of the stage) and RIM-W, with the fewest bands that give that
  # layout. A bad layout, or a block too small for RIM-W, stops the job here instead of after hours
  local d=$1 blk=$2 nb nbs ref msg="" sig job; shift 2
  for nb in "$@"; do
    [ "$nb" -le "$NB_SAVE" ] || msg="$msg"$'\n'"!!! $nb bands requested, the SAVE in $d has $NB_SAVE: redo the nscf with more bands"
    [ "$nb" -gt "$QHI" ] || msg="$msg"$'\n'"!!! $nb bands: needs more than $QHI (the top band of QPkrange)"
    msg="$msg"$'\n'"$(par_gw $nb 2>&1 >/dev/null)"
  done
  msg=$(echo "$msg" | grep . | sort -u)
  [ -z "$msg" ] || { echo "$msg"; echo "!!! nothing was run"; exit 1; }
  [ "$CHECK" = 1 ] && { [ "$NP" -gt 1 ] || [ "$RIMW" = 1 ]; } || return 0
  for nb in $(printf '%s\n' "$@" | sort -n -u); do
    sig=$( { par_gw $nb; echo "$NK $NQ $VB $K1 $K2 $EXTRA_GW $RIMW $RIMW_G $GTERM $W $MPA_NF $QLO $QHI"; [ "$RIMW" = 1 ] && echo "$blk"; } | md5sum | cut -c1-8)
    grep -qx "$sig" "$d/conv/layouts_ok" 2>/dev/null && continue
    ref=$(par_gw $nb); nbs=$nb
    for ((t = QHI + 1; t < nb; t++)); do [ "$(par_gw $t 2>/dev/null)" = "$ref" ] && { nbs=$t; break; }; done
    job=check_$sig
    fresh "$d" $job
    ( RIM=$'RandQpts= 1000\nRandGvec= 1 RL'; gw_in "$d/conv/$job.in" 5 $nbs $blk )
    echo "  check: cheap GW with the MPI layout of $nb bands ($nbs bands, block $blk Ry${JS:+, $JS}, $NP tasks) ($(date +%H:%M))"
    sed -n '/_CPU=/p' "$d/conv/$job.in" | sed 's/^/    /'
    launch "$d" $job qp || { [ "$RIMW" = 1 ] && grep -qs "for RIM-W is higher" "$d"/conv/LOG/l-${job}_* "$d"/conv/l-${job}_* &&
      echo "!!! RIM-W needs more screening G-vectors than a $blk Ry block has: raise the block or lower RIMW_G ($RIMW_G RL)"
      echo "!!! the check failed (above): nothing else was run"; exit 1; }
    echo "$sig" >> "$d/conv/layouts_ok"
    rm -rf "$d/conv/$job" "$d"/conv/[orl]-${job}* "$d"/conv/LOG/l-${job}_* "$d/conv/$job".*
    echo "  check OK ($(date +%H:%M))"
  done
}

minof () { printf '%s\n' "$@" | sort -g | head -1; }

FFTGL=""; [ -n "$FFTG" ] && FFTGL="FFTGvecs= $FFTG"
setup_info "$TOP"
echo "=== SAVE: $NB_SAVE bands, $NK IBZ k-points, VB $VB, CB $CB, QP k $K1-$K2 bands $QLO-$QHI, W $W; MPI tasks $NP x OMP threads $OMP_NUM_THREADS ==="

# every stage stops at the first failed run (later runs would fail the same way)
case $STAGE in exx|all)
  echo "=== exchange cutoff scan (exchange only) ==="
  par_se > /dev/null || exit 1
  for e in $EXX_LIST; do run_hf "$TOP" exx_$e $e || exit 1; done ;; esac
case $STAGE in bands|all)
  echo "=== bands scan (EXX $EXX0 Ry, block $BLOCK0 Ry${JS:+, $JS}) ==="
  preflight "$TOP" $BLOCK0 $BANDS_LIST
  for n in $BANDS_LIST; do run_gw "$TOP" bands_$n$JS $EXX0 $n $BLOCK0 || exit 1; done ;; esac
case $STAGE in block|all)
  echo "=== screening block scan (EXX $EXX0 Ry, bands $BANDS0${JS:+, $JS}) ==="
  preflight "$TOP" $(minof $BLOCK_LIST) $BANDS0
  for b in $BLOCK_LIST; do run_gw "$TOP" block_$b$JS $EXX0 $BANDS0 $b || exit 1; done ;; esac
case $STAGE in grid)
  echo "=== bands x block grid (EXX $EXX0 Ry${JS:+, $JS}) ==="
  preflight "$TOP" $(minof $BLOCK_LIST) $BANDS_LIST
  for n in $BANDS_LIST; do for b in $BLOCK_LIST; do run_gw "$TOP" grid_${n}_$b$JS $EXX0 $n $b || exit 1; done; done ;; esac
case $STAGE in dirs)
  [ -n "$DIRS" ] || { echo "!!! STAGE=dirs needs DIRS=\"dir1 dir2 ...\""; exit 1; }
  for d in $DIRS; do
    setup_info "$TOP/$d"
    echo "=== $d: $NB_SAVE bands, $NK k-points (EXX $EXX0, bands $BANDS0, block $BLOCK0${JS:+, $JS}) ==="
    preflight "$TOP/$d" $BLOCK0 $BANDS0
    run_gw "$TOP/$d" final$JS $EXX0 $BANDS0 $BLOCK0 || exit 1
  done
  setup_info "$TOP" ;; esac

# --- summary ---
case $STAGE in summary|all|exx|bands|block|grid|dirs)
DIRS="$DIRS" python3 - "$TOP" "$TOL" <<'PY'
import glob, os, re, sys
import numpy as np
top, tol = sys.argv[1], float(sys.argv[2])

def read_qp(f):
    """o-*.qp -> {k: {band: (Eo, Eqp)}}"""
    cols, data = None, {}
    for l in open(f):
        if l.startswith('#'):
            h = l.lower()
            if 'band' in h and 'eo' in h:
                cols = re.split(r'\s{2,}', l.strip('# \n'))
            continue
        x = l.split()
        if len(x) < 4:
            continue
        k, b, eo, de = int(float(x[0])), int(float(x[1])), float(x[2]), float(x[3])
        data.setdefault(k, {})[b] = (eo, eo + de)
    return data

def read_hf(f):
    """o-*.hf: K, band, Eo, Ehf, Vxc, Sx -> {k: {band: (Eo, Ehf)}}"""
    data = {}
    for l in open(f):
        if l.startswith('#'):
            continue
        x = l.split()
        if len(x) >= 4:
            data.setdefault(int(float(x[0])), {})[int(float(x[1]))] = (float(x[2]), float(x[3]))
    return data

VBS = {}
def vb_of(f):
    """filled bands from the r_setup of the directory of conv/<file>"""
    d = os.path.dirname(os.path.dirname(os.path.abspath(f)))
    if d not in VBS:
        VBS[d] = None
        try:
            VBS[d] = int(next(l.split(':')[1] for l in open(f'{d}/r_setup') if 'Filled Bands' in l))
        except Exception:
            pass
    return VBS[d]

def gap(f):
    d = read_hf(f) if f.endswith('.hf') else read_qp(f)
    vb = vb_of(f)
    best = None
    for k, bb in d.items():
        if len(bb) < 2:
            continue
        v, c = (vb, vb + 1) if vb in bb and vb + 1 in bb else (min(bb), max(bb))
        ks, qp = bb[c][0] - bb[v][0], bb[c][1] - bb[v][1]
        if best is None or ks < best[1]:
            best = (k, ks, qp, bb[v][1] - bb[v][0], bb[c][1] - bb[c][0])
    return best          # k, KS gap, QP gap, dE(VB), dE(CB)

def scan(prefix, title, unit, ext='qp', sfx=''):
    rows = []
    for f in glob.glob(f'{top}/conv/o-{prefix}_*{sfx}.{ext}'):
        m = re.search(rf'o-{prefix}_([0-9.]+){sfx}\.{ext}$', f)
        if m and gap(f):
            rows.append((float(m.group(1)), *gap(f)))
    rows.sort()
    if not rows:
        other = sorted(os.path.basename(x) for x in glob.glob(f'{top}/conv/o-{prefix}_*')
                       if re.search(rf'o-{prefix}_[0-9.]+{sfx}\.', x))
        if other:
            print(f'\n{title}: no readable o-{prefix}_<value>{sfx}.{ext} (found: {" ".join(other)})')
        return []
    print(f'\n{title}')
    print(f'  {"value":>8s} {"k":>3s} {"KS gap":>8s} {"QP gap":>8s} {"dE(VB)":>8s} {"dE(CB)":>8s} {"change":>8s}')
    ch = [float('nan')] + [rows[i][3] - rows[i - 1][3] for i in range(1, len(rows))]
    # converged at the first value after which every further change stays below tol
    i0 = next((i for i in range(1, len(rows)) if all(abs(c) < tol for c in ch[i:])), None)
    for i, r in enumerate(rows):
        flag = ''
        if i == i0:
            flag = f'  <- converged at {rows[i - 1][0]:g} {unit} (all later changes < {tol*1000:.0f} meV)'
            if i == len(rows) - 1:
                flag += ': only the last step, add a point to confirm'
        print(f'  {r[0]:8g} {r[1]:3d} {r[2]:8.4f} {r[3]:8.4f} {r[4]:8.4f} {r[5]:8.4f} {ch[i]:8.4f}{flag}')
    if i0 is None:
        print(f'  not converged within {tol*1000:.0f} meV: extend the list')
    return rows

def grid_table(sfx, title):
    vals = {}
    for f in glob.glob(f'{top}/conv/o-grid_*_*{sfx}.qp'):
        m = re.search(rf'o-grid_(\d+)_([0-9.]+){sfx}\.qp$', f)
        if m and gap(f):
            vals[(int(m.group(1)), float(m.group(2)))] = gap(f)[2]
    if not vals:
        return
    Ns = sorted({k[0] for k in vals}); Bs = sorted({k[1] for k in vals})
    print(f'\n{title}')
    print('  ' + ' '.join(f'{x:>8g}' for x in [0] + Bs).replace('       0', '   N\\Ry', 1))
    for n in Ns:
        print(f'  {n:8d} ' + ' '.join(f'{vals.get((n, x), float("nan")):8.4f}' for x in Bs))

out = {}
out['exx'] = scan('exx', 'Exchange cutoff EXXRLvcs (Ry): exchange-only gap (Sigma_x - Vxc)', 'Ry', 'hf')
for sfx, tag in [(r + b + m, ' [' + ', '.join(x for x in (r and 'RIM-W', b and 'BG terminator', m and 'MPA') if x) + ']' if r + b + m else '')
                 for m in ('', '_mpa') for r in ('', '_rimw') for b in ('', '_bg')]:
    b = scan('bands', 'Bands in screening and self-energy' + tag, 'bands', sfx=sfx)
    if len(b) >= 3:
        # gap(N) = g_inf + A/N on the last three points (an upper bound on the remaining change when,
        # e.g. with the BG terminator, the changes shrink faster than 1/N)
        N = np.array([r[0] for r in b[-3:]]); g = np.array([r[3] for r in b[-3:]])
        A = np.vstack([np.ones_like(N), 1 / N]).T
        (ginf, a), *_ = np.linalg.lstsq(A, g, rcond=None)
        print(f'  1/N extrapolation (last 3 points): gap(N -> inf) = {ginf:.4f} eV '
              f'({(ginf - g[-1])*1000:+.0f} meV from N = {N[-1]:g})')
    bl = scan('block', 'Screening block NGsBlkXp (Ry)' + tag, 'Ry', sfx=sfx)
    if not sfx:
        out['bands'], out['block'] = b, bl
    grid_table(sfx, 'QP gap (eV), bands (rows) x block in Ry (columns)' + tag)

# k grids (STAGE=dirs): the final runs in this directory, its subdirectories and DIRS
def lab(sfx):          # '' -> no RIM-W, '_rimw_bg_mpa' -> RIM-W+BG+MPA
    return '+'.join({'rimw': 'RIM-W', 'bg': 'BG'}.get(t, t.upper()) for t in sfx.strip('_').split('_')) if sfx else 'no RIM-W'
fin, finf = {}, {}
fs = glob.glob(f'{top}/conv/o-final*.qp') + glob.glob(f'{top}/*/conv/o-final*.qp')
for d in os.environ.get('DIRS', '').split():          # also directories outside this one
    fs += glob.glob(f'{top}/{d}/conv/o-final*.qp')
for f in sorted(set(os.path.normpath(x) for x in fs)):
    m = re.search(r'o-final((?:_rimw)?(?:_bg)?(?:_mpa\d*)?)\.qp$', f)
    d = os.path.relpath(os.path.dirname(os.path.dirname(f)), top)
    if m and gap(f):
        fin.setdefault(d, {})[m.group(1)] = gap(f); finf[(d, m.group(1))] = f
if fin:
    def nk(d):
        try:
            return int(next(l.split(':')[1] for l in open(f'{top}/{d}/r_setup') if 'IBZ K-points' in l))
        except Exception:
            return 0
    cols = sorted({c for v in fin.values() for c in v}, key=lambda c: (c != '', c != '_rimw', c))
    print('\nk grids (final runs): QP gap (eV)')
    print(f'  {"dir":>10s} {"IBZ k":>6s} {"k":>3s} {"KS gap":>8s} ' + ' '.join(f'{lab(c):>12s}' for c in cols)
          + ('   RIM-W shift' if '' in cols and '_rimw' in cols else ''))
    for d in sorted(fin, key=nk):
        r = next(fin[d][c] for c in cols if c in fin[d])
        line = f'  {d:>10s} {nk(d):6d} {r[0]:3d} {r[1]:8.4f} ' + ' '.join(
            f'{fin[d][c][2]:12.4f}' if c in fin[d] else f'{"-":>12s}' for c in cols)
        if '' in fin[d] and '_rimw' in fin[d]:
            line += f'   {(fin[d]["_rimw"][2] - fin[d][""][2])*1000:+8.0f} meV'
        print(line)

# splittings at the gap k: VB-(VB-1) and (CB+1)-CB, for every output with those bands (QPBANDS)
spl = []
for f in sorted(glob.glob(f'{top}/conv/o-*.qp') + [finf[x] for x in finf]):
    f = os.path.normpath(f); vb = vb_of(f); g = gap(f)
    if vb is None or not g:
        continue
    bb = read_qp(f).get(g[0], {})
    if all(b in bb for b in (vb - 1, vb, vb + 1, vb + 2)):
        dv = [bb[vb][i] - bb[vb - 1][i] for i in (0, 1)]; dc = [bb[vb + 2][i] - bb[vb + 1][i] for i in (0, 1)]
        spl.append((os.path.relpath(f, top), g[0], dv, dc))
if spl:
    print('\nSplittings at the gap k (meV): valence VB-(VB-1), conduction (CB+1)-CB')
    print(f'  {"run":>36s} {"k":>3s} {"val KS":>8s} {"val QP":>8s} {"con KS":>8s} {"con QP":>8s}')
    for n, k, dv, dc in sorted(set((a, b, tuple(c), tuple(e)) for a, b, c, e in spl)):
        print(f'  {n:>36s} {k:3d} {dv[0]*1000:8.1f} {dv[1]*1000:8.1f} {dc[0]*1000:8.1f} {dc[1]*1000:8.1f}')
try:
    import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
    pl = [(k, t, u) for k, t, u in (('exx', 'EXXRLvcs (Ry)', ''), ('bands', 'bands N', ''), ('block', 'NGsBlkXp (Ry)', '')) if out[k]]
    if pl:
        fig, ax = plt.subplots(1, len(pl), figsize=(4 * len(pl), 3.2), squeeze=False)
        for a, (k, t, _) in zip(ax[0], pl):
            x = [r[0] for r in out[k]]; y = [r[3] for r in out[k]]
            a.plot(x, y, 'o-'); a.set_xlabel(t); a.set_ylabel('QP gap (eV)')
        fig.tight_layout(); fig.savefig(f'{top}/gw_convergence.png', dpi=150)
        print(f'\nwrote {top}/gw_convergence.png')
except Exception as e:
    print('no plot:', e)
PY
;; esac
