#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=24
#SBATCH --cpus-per-task=2
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=48:00:00
#SBATCH --job-name=bse_conv
#SBATCH --output=log/bse_conv.o%A
#SBATCH --error=log/bse_conv.e%A

# BSE convergence for monolayer WS2 in Yambo (static screening em1s + BSE, q = 0, in-plane light).
# Tracks the lowest bright exciton (A) and its binding energy Eb = (lowest transition of the BSE
# window) - E_A, one parameter at a time with the others fixed:
#
#   STAGE=wbands : bands in the static screening BndsRnXs           list WB_LIST
#   STAGE=wblock : W block in the kernel BSENGBlk (Ry, <= XBLK)     list WBLK_LIST
#   STAGE=exx    : exchange block in the kernel BSENGexx (Ry)       list EXX_LIST
#   STAGE=bands  : BSE bands, nv:nc around the gap                  list BANDS_LIST ("1:2" = VB..CB+1)
#   STAGE=dirs   : the same BSE in other directories (k grids), DIRS="k18 k24 ..." ("." = here),
#                  each with its own SAVE (nscf + p2y + setup done by you), k grids N x N x 1, N / 3
#   STAGE=summary: tables, convergence flags, 1/N extrapolation of Eb over the k grids, plot
#   STAGE=all    : wbands, wblock, exx, bands, summary
#
# Run from the directory with SAVE (after p2y and the yambo setup). Runs go to bconv/: the static
# screening of each BndsRnXs in bconv/w_<bands>_<XBLK>/ (ndb.em1s and the dipoles, reused by
# every BSE run with those bands), each BSE run in bconv/<scan>_<value>/ with its spectrum
# o-<job>.alpha_q1_* (with the 2D cutoff Yambo writes the polarizability; eps otherwise).
#
# BSE band windows should not split degenerate bands (Yambo warns "break level degeneracy", the
# script repeats it). WS2 without SOC, at Gamma: 11/12 and 14/15, 16/17 are pairs, so nv = 1,3,4,6
# and nc = 2,4,5 keep them whole (the defaults); check your r_setup band energies for other cases.
#
# The binding energy is read from the BSE report of the fork's yambo ("[BSE] lowest bright
# excitation", "[BSE] lowest transition energy of the window"); with another yambo only the
# peak of the spectrum is reported. In the Tamm-Dancoff BSE (BSEMOD=resonant, default) Eb does
# not depend on a rigid QP shift, so the convergence needs no QP corrections; SCISSOR (eV) or
# QPDB (an ndb.QP) put the absolute energies on the GW scale. Choose them before the first run:
# runs are skipped by name, so a later change of SCISSOR would mix scales in one table.
#
# Spin-orbit SAVE: the bands come in pairs, use even nv:nc ("2:2" = the two spin-split top
# valence and bottom conduction bands; "4:4" ...). The lowest exciton is then usually dark
# (spin forbidden): A is the lowest bright one.
#
# MPI: the script sets DIP, X_and_IO (screening) and BS (kernel) layouts that fit Yambo's limits
# (see "MPI layouts"), refuses to start if a run cannot fit, and tries each new layout first on a
# cheap screening + Haydock BSE (CHECK=0 skips it). A stage stops at its first failed run.
# The diagonalization (BSSMOD=d) of the BSE matrix runs on one task unless yambo has ScaLAPACK:
# for large grids (many thousands of transitions) use BSSMOD=s (SLEPc, lowest states) if built.
#
# RIM-W (RIMW=1, default): W near q = 0 integrated over the small BZ around each q, as in the GW.
# Without it the q = 0 cell of the kernel takes the screening at a single tiny q, where a 2D layer
# hardly screens (eps^-1 -> 1), so W is nearly bare there and coarse grids overbind badly (test:
# 12x12 WS2 model, Eb 1.31 eV without, 0.64 eV with). Needs the slab cutoff and enough G-vectors in
# the screening (XBLK); job names get _rimw. RIMW=0 for comparison.

STAGE=${STAGE:-all}
WB_LIST=${WB_LIST:-"50 100 200 300"}             # BndsRnXs
WBLK_LIST=${WBLK_LIST:-"1 2 3 4 5"}             # BSENGBlk, Ry
EXX_LIST=${EXX_LIST:-"1 2 4 6 8"}               # BSENGexx, Ry
BANDS_LIST=${BANDS_LIST:-"1:2 3:2 3:4 6:4"}     # nv:nc, windows that keep degenerate pairs whole (WS2, no SOC)
WB0=${WB0:-200}; WBLK0=${WBLK0:-3}; EXX0=${EXX0:-4}; BANDS0=${BANDS0:-1:2}   # fixed values
XBLK=${XBLK:-6}                                 # NGsBlkXs of the static screening, Ry (>= BSENGBlk)
TOL=${TOL:-0.01}                                # eV, on the binding energy
BSEMOD=${BSEMOD:-resonant}                      # resonant (Tamm-Dancoff) or coupling
BSSMOD=${BSSMOD:-d}                             # d diagonalization, s SLEPc
SCISSOR=${SCISSOR:-}                            # eV, rigid QP shift of the conduction bands
QPDB=${QPDB:-}                                  # path to an ndb.QP (instead of SCISSOR)
CUT=${CUT:-'CUTGeo= "slab z"'}
RIM=${RIM:-$'RandQpts= 1000000\nRandGvec= 100 RL'}
RIMW=${RIMW:-1}; RIMW_G=${RIMW_G:-15}                   # RIM-W on by default (see above)
FFTG=${FFTG:-""}
ETA=${ETA:-0.05}; DE=${DE:-0.005}               # eV, broadening and step of the spectrum
EXTRA_BSE=${EXTRA_BSE:-""}                      # any extra input lines of the BSE runs
DIRS=${DIRS:-""}
YFLAGS=${YFLAGS-auto}                            # auto: -nompi for one task, if this yambo has it (MPI builds)
RUNNER=${RUNNER:-""}
JS=""; [ "$RIMW" = 1 ] && JS="_rimw"

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
CHECK=${CHECK:-1}
if [ "$NP" -gt 1 ]; then
  if [ "$NP" = "${SLURM_NTASKS:-}" ]; then
    [ -n "$RUNNER" ] || RUNNER="srun --cpus-per-task=$OMP_NUM_THREADS"
  else
    [ -n "$RUNNER" ] || RUNNER="srun -n $NP --cpus-per-task=$OMP_NUM_THREADS"
  fi
  YFLAGS=""
fi
TOP="${SLURM_SUBMIT_DIR:-$PWD}"
cd "$TOP"; mkdir -p log bconv

# --- system information from the yambo setup (r_setup) of a directory ---
setup_info () {   # dir -> NB_SAVE NK NKBZ NQ VB GAP NGRID
  local d=$1
  [ -f "$d/SAVE/ns.db1" ] || {
    echo "!!! $d/SAVE/ns.db1 not found: run p2y in the nscf .save folder of this grid and put its SAVE in $d"; exit 1; }
  if [ ! -f "$d/r_setup" ]; then (cd "$d" && $RUNNER yambo $YFLAGS > log_setup.out 2>&1); fi
  NB_SAVE=$(awk -F: '/^ *Bands  /{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  NK=$(awk -F: '/IBZ K-points/{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  NKBZ=$(awk -F: '/BZ +K-points/ && !/IBZ/{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  NQ=$(awk -F: '/IBZ Q-points/{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  VB=$(awk -F: '/Filled Bands/{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  GAP=$(awk -F: '/\[X\] Direct Gap  /{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  NGRID=$(awk -F: '/Grid dimensions/{print $2+0; exit}' "$d/r_setup" 2>/dev/null)
  [ -n "$NB_SAVE" ] && [ -n "$NK" ] && [ -n "$NKBZ" ] && [ -n "$NQ" ] && [ -n "$VB" ] && [ -n "$GAP" ] || {
    echo "!!! cannot read bands/k-points/q-points/gap from $d/r_setup (yambo setup failed?)"
    tail -5 "$d/log_setup.out" 2>/dev/null; [ -f "$d/r_setup" ] && tail -5 "$d/r_setup"; ls -t "$d"/l_setup* "$d"/LOG/l_setup* 2>/dev/null | head -1 | xargs -r tail -5
    rm -f "$d/r_setup"; exit 1; }
  CB=$((VB+1))
}

# --- MPI layouts ---
# Yambo aborts ("USER parallel structure does not fit the current run parameters") when a role has
# more MPI tasks than elements (src/parallel/PARALLEL_structure.F); element counts from
# src/parallel/PARALLEL_global_dimensions.F, with DipBands = 1..NBX (the screening bands) in every run:
#   DIPOLES  "k c v"     k: IBZ k   c: NBX-VB   v: VB
#   X_and_IO "q g k c v" q: IBZ q   k: BZ k     c: NBX-VB   (static screening)
#   BS       "k eh t"    k: IBZ k   eh: not checked (transitions per k, nv*nc, are split over it)
#            t: checked against the e/h range of the last role, nc*VB - (VB-nv+1) + 1 (Yambo's formula)
fit () {   # ntasks cap1 cap2 cap3 -> a b c with a*b*c = ntasks, each <= its cap, largest a, then b
  local n=$1 c1=$2 c2=$3 c3=$4 a b c
  for ((a = c1 < n ? c1 : n; a >= 1; a--)); do (( n % a )) && continue
    for ((b = c2 < n / a ? c2 : n / a; b >= 1; b--)); do (( (n / a) % b )) && continue
      c=$((n / a / b)); (( c <= c3 )) && { echo $a $b $c; return 0; }
    done
  done
  return 1
}

par_w () {   # nbx -> DIP and X lines of the screening run (and the DIP line of the BSE runs)
  [ "$NP" -gt 1 ] || return 0
  local nc=$(( $1 - VB )) d x
  d=($(fit $NP $nc $NK $VB)) || { echo "!!! $NP MPI tasks do not fit the dipoles: c <= $nc, k <= $NK, v <= $VB" >&2; return 1; }
  x=($(fit $NP $nc $NKBZ $NQ)) || { echo "!!! $NP MPI tasks do not fit the screening: c <= $nc, k <= $NKBZ, q <= $NQ" >&2; return 1; }
  printf 'DIP_CPU= "%d %d %d"\nDIP_ROLEs= "k c v"\n' ${d[1]} ${d[0]} ${d[2]}
  printf 'X_and_IO_CPU= "%d 1 %d %d 1"\nX_and_IO_ROLEs= "q g k c v"\n' ${x[2]} ${x[1]} ${x[0]}
}

par_bs () {   # nv nc -> BS lines
  [ "$NP" -gt 1 ] || return 0
  local nv=$1 nc=$2 eh t b
  t=$(( nc * VB - (VB - nv + 1) + 1 ))
  # k first, then e/h pairs (up to nv*nc), then t; tasks left over go to eh (not checked by Yambo)
  b=($(fit $NP $NK $((nv * nc)) $t)) || b=($(fit $NP $NK $NP $t))
  printf 'BS_CPU= "%d %d %d"\nBS_ROLEs= "k eh t"\n' ${b[0]} ${b[1]} ${b[2]}
}

# --- inputs ---
w_in () {   # file nbx
  local par; par=$(par_w $2) || return 1
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
 1 | $2 |
%
% BndsRnXs
 1 | $2 |
%
NGsBlkXs= $XBLK Ry
% LongDrXs
 1.0 | 0.0 | 0.0 |
%
EOF
}

bse_in () {   # file nbx exx_Ry wblk_Ry nv nc [solver]
  local f=$1 nbx=$2 exx=$3 blk=$4 nv=$5 nc=$6 bss=${7:-$BSSMOD} pd pb qp="" rw="" lo hi ns
  pd=$(par_w $nbx) || return 1; pd=$(printf '%s\n' "$pd" | grep DIP)
  pb=$(par_bs $nv $nc)
  [ -n "$SCISSOR" ] && qp=$'% KfnQP_E\n '$SCISSOR$' | 1.000000 | 1.000000 |\n%'
  [ -n "$QPDB" ] && qp="KfnQPdb= \"E < $QPDB\""
  [ "$RIMW" = 1 ] && rw=$'rim_w\nRandGvecW= '$RIMW_G' RL'
  read lo hi ns <<< $(python3 -c "
g=$GAP+${SCISSOR:-0}; lo=max(0.05,g-1.2); hi=g+0.6+(2.0 if '$QPDB' else 0)
print(f'{lo:.3f} {hi:.3f} {int(round((hi-lo)/$DE))+1}')")
  cat > $f <<EOF
optics
bse
bss
rim_cut
$rw
K_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
$pd
$pb
$CUT
$RIM
$FFTGL
BSKmod= "SEX"
BSEmod= "$BSEMOD"
BSSmod= "$bss"
BSHayTrs= -0.05
BSENGexx= $exx Ry
BSENGBlk= $blk Ry
% DipBands
 1 | $nbx |
%
% BSEBands
 $((VB - nv + 1)) | $((VB + nc)) |
%
% BEnRange
 $lo | $hi | eV
%
% BDmRange
 $ETA | $ETA | eV
%
BEnSteps= $ns
% BLongDir
 1.0 | 0.0 | 0.0 |
%
BSEprop= "abs"
$qp
$EXTRA_BSE
EOF
}

# --- runs ---
rmjob () {   # dir job : Yambo files of <job> (r-/l-<job>_<runlevels>), not those of <job>_rimw, <job>_bg, ...
  local f
  for f in "$1"/bconv/[rl]-$2_* "$1"/bconv/LOG/l-$2_*; do
    case "${f##*/}" in [rl]-$2_rimw*|[rl]-$2_bg*) ;; *) rm -f "$f" ;; esac
  done
}

fresh () {   # dir job : SAVE link in bconv/, and no leftovers of an earlier failed attempt
  mkdir -p "$1/bconv"; [ -e "$1/bconv/SAVE" ] || ln -s ../SAVE "$1/bconv/SAVE"
  rm -rf "$1/bconv/$2" "$1"/bconv/o-$2.*; rmjob "$1" $2
}

launch () {   # dir job jobdirs done-test : run yambo on bconv/<job>.in; 1, with Yambo's errors, if not done
  local d=$1 job=$2 js=$3 done=$4 e try
  for try in 1 2 3; do
    ( cd "$d/bconv" && $RUNNER yambo $YFLAGS -F $job.in -J "$js" > $job.out 2>&1 )
    ( cd "$d/bconv" && eval "$done" ) && return 0
    # Yambo race: the master writes ndb.cutoff while a slower task re-reads it (coulomb/cutoff_driver.F)
    grep -qs "Reading File .*ndb.cutoff" "$d"/bconv/LOG/l-${job}_* "$d"/bconv/l-${job}_* || break
    [ $try -lt 3 ] || break
    echo "  $job: ndb.cutoff read while being written (Yambo race between MPI tasks): retrying"
    rm -rf "$d/bconv/$job"/ndb.cutoff*; rmjob "$d" $job
  done
  echo "!!! $job failed: see $d/bconv/$job.out and $d/bconv/LOG/l-${job}_*"
  e=$(grep -h "ERROR" "$d"/bconv/LOG/l-${job}_* "$d"/bconv/l-${job}_* 2>/dev/null | sed 's/.*\[ERROR\]/  [ERROR]/' | sort -u | head -5)
  if [ -n "$e" ]; then echo "$e"; else tail -5 "$d/bconv/$job.out"; fi
  return 1
}

run_w () {   # dir nbx [job] : static screening -> bconv/w_<nbx>_<XBLK>/ndb.em1s
  local d=$1 nb=$2 job=${3:-w_${2}_$XBLK}
  if ls "$d/bconv/$job"/ndb.em1s* >/dev/null 2>&1; then return 0; fi
  fresh "$d" $job
  w_in "$d/bconv/$job.in" $nb || return 1
  echo "  $job: static screening, $nb bands, block $XBLK Ry ($(date +%H:%M))"
  launch "$d" $job $job "ls $job/ndb.em1s* >/dev/null 2>&1"
}

run_bse () {   # dir job nbx exx wblk nv:nc [solver]
  local d=$1 job=$2 nb=$3 exx=$4 blk=$5 nv=${6%%:*} nc=${6##*:} bss=${7:-$BSSMOD} w
  w=w_${nb}_$XBLK; [ -n "$8" ] && w=$8
  if ls "$d"/bconv/o-$job.*_q1_*_bse >/dev/null 2>&1; then echo "  $job done"; return 0; fi
  run_w "$d" $nb $w || return 1
  fresh "$d" $job
  bse_in "$d/bconv/$job.in" $nb $exx $blk $nv $nc $bss || return 1
  echo "  $job: BSE bands $((VB - nv + 1))-$((VB + nc)), exchange $exx Ry, W block $blk Ry, screening $nb bands${JS:+, RIM-W} ($(date +%H:%M))"
  launch "$d" $job "$job,$w" "ls o-$job.*_q1_*_bse >/dev/null 2>&1" || return 1
  grep -qs "break level degeneracy" "$d"/bconv/LOG/l-${job}_* "$d"/bconv/l-${job}_* &&
    echo "  note: BSE bands $((VB - nv + 1))-$((VB + nc)) split a degenerate pair at some k (Yambo warning): prefer a window that does not"
  return 0
}

lay () {   # nbx nv nc -> all layouts of a BSE run (with its screening)
  par_w $1 && par_bs $2 $3
}

preflight () {   # dir exx wblk nbx/nv:nc... : every run of the stage must fit the SAVE and have MPI
  # layouts; each layout not used before is tried on a cheap screening (the fewest bands with the same
  # layouts) + Haydock BSE with the stage's smallest blocks, so a bad layout stops the job here
  local d=$1 exx=$2 blk=$3 r nb nvc nv nc msg="" sig nbs ref job t; shift 3
  for r in "$@"; do
    nb=${r%%/*}; nvc=${r#*/}; nv=${nvc%%:*}; nc=${nvc##*:}
    [ "$nb" -le "$NB_SAVE" ] || msg="$msg"$'\n'"!!! $nb screening bands requested, the SAVE in $d has $NB_SAVE"
    [ "$((VB + nc))" -le "$nb" ] || msg="$msg"$'\n'"!!! BSE bands up to $((VB + nc)) need at least as many screening bands ($nb)"
    [ "$nv" -le "$VB" ] || msg="$msg"$'\n'"!!! $nv valence bands requested, there are $VB"
    msg="$msg"$'\n'"$(lay $nb $nv $nc 2>&1 >/dev/null)"
  done
  awk -v a=$blk -v b=$XBLK 'BEGIN{exit !(a > b)}' && msg="$msg"$'\n'"!!! BSENGBlk $blk Ry > XBLK $XBLK Ry (the screening block): raise XBLK"
  msg=$(echo "$msg" | grep . | sort -u)
  [ -z "$msg" ] || { echo "$msg"; echo "!!! nothing was run"; exit 1; }
  [ "$CHECK" = 1 ] && { [ "$NP" -gt 1 ] || [ "$RIMW" = 1 ]; } || return 0
  for r in "$@"; do
    nb=${r%%/*}; nvc=${r#*/}; nv=${nvc%%:*}; nc=${nvc##*:}
    sig=$( { lay $nb $nv $nc; echo "$NK $NQ $VB $XBLK $EXTRA_BSE $RIMW $RIMW_G $blk"; } | md5sum | cut -c1-8)
    grep -qx "$sig" "$d/bconv/layouts_ok" 2>/dev/null && continue
    ref=$(lay $nb $nv $nc); nbs=$nb
    for ((t = VB + nc; t < nb; t++)); do [ "$(lay $t $nv $nc 2>/dev/null)" = "$ref" ] && { nbs=$t; break; }; done
    job=check_$sig
    echo "  check: cheap screening ($nbs bands) + Haydock BSE with the MPI layouts of $nb bands, $nvc, $NP tasks${JS:+, RIM-W} ($(date +%H:%M))"
    printf '%s\n' "$ref" | sed -n '/_CPU=/s/^/    /p'
    ( RIM=$'RandQpts= 1000\nRandGvec= 1 RL'; BSEMOD=resonant
      run_bse "$d" $job $nbs 1 $blk $nvc h w_$job ) || {
      [ "$RIMW" = 1 ] && grep -qs "for RIM-W is higher" "$d"/bconv/LOG/l-${job}_* "$d"/bconv/l-${job}_* &&
        echo "!!! RIM-W needs more screening G-vectors than XBLK = $XBLK Ry: raise XBLK or lower RIMW_G ($RIMW_G RL)"
      echo "!!! the check failed (above): nothing else was run"; exit 1; }
    echo "$sig" >> "$d/bconv/layouts_ok"
    rm -rf "$d/bconv/$job" "$d/bconv/w_$job" "$d"/bconv/[orl]-${job}* "$d"/bconv/[orl]-w_${job}* \
           "$d"/bconv/LOG/l-${job}_* "$d"/bconv/LOG/l-w_${job}_* "$d/bconv/$job".* "$d/bconv/w_$job".*
    echo "  check OK ($(date +%H:%M))"
  done
}

FFTGL=""; [ -n "$FFTG" ] && FFTGL="FFTGvecs= $FFTG"
setup_info "$TOP"
echo "=== SAVE: $NB_SAVE bands, $NK IBZ k-points (${NGRID:-?}x${NGRID:-?}), VB $VB, KS gap $GAP eV; MPI tasks $NP x OMP threads $OMP_NUM_THREADS ==="
minof () { printf '%s\n' "$@" | sort -g | head -1; }

# every stage stops at its first failed run (later runs would fail the same way)
case $STAGE in wbands|all)
  echo "=== screening bands BndsRnXs (BSE $BANDS0, exchange $EXX0 Ry, W block $WBLK0 Ry${JS:+, RIM-W}) ==="
  preflight "$TOP" $EXX0 $WBLK0 $(for n in $WB_LIST; do echo $n/$BANDS0; done)
  for n in $WB_LIST; do run_bse "$TOP" wb_$n$JS $n $EXX0 $WBLK0 $BANDS0 || exit 1; done ;; esac
case $STAGE in wblock|all)
  echo "=== W block BSENGBlk (screening $WB0 bands, block $XBLK Ry; BSE $BANDS0, exchange $EXX0 Ry${JS:+, RIM-W}) ==="
  preflight "$TOP" $EXX0 $(minof $WBLK_LIST) $WB0/$BANDS0
  for b in $WBLK_LIST; do run_bse "$TOP" wblk_$b$JS $WB0 $EXX0 $b $BANDS0 || exit 1; done ;; esac
case $STAGE in exx|all)
  echo "=== exchange BSENGexx (BSE $BANDS0, W block $WBLK0 Ry, screening $WB0 bands${JS:+, RIM-W}) ==="
  preflight "$TOP" $(minof $EXX_LIST) $WBLK0 $WB0/$BANDS0
  for x in $EXX_LIST; do run_bse "$TOP" exx_$x$JS $WB0 $x $WBLK0 $BANDS0 || exit 1; done ;; esac
case $STAGE in bands|all)
  echo "=== BSE bands nv:nc (exchange $EXX0 Ry, W block $WBLK0 Ry, screening $WB0 bands${JS:+, RIM-W}) ==="
  preflight "$TOP" $EXX0 $WBLK0 $(for p in $BANDS_LIST; do echo $WB0/$p; done)
  for p in $BANDS_LIST; do run_bse "$TOP" bands_${p/:/-}$JS $WB0 $EXX0 $WBLK0 $p || exit 1; done ;; esac
case $STAGE in dirs)
  [ -n "$DIRS" ] || { echo "!!! STAGE=dirs needs DIRS=\"dir1 dir2 ...\""; exit 1; }
  # set up and check every folder first, so a missing SAVE in the last one stops the job before any run
  for d in $DIRS; do
    setup_info "$TOP/$d"
    [ "$NB_SAVE" -ge $WB0 ] || { echo "!!! $d has $NB_SAVE bands, fewer than the WB0=$WB0 screening bands"; exit 1; }
  done
  echo "  folders OK: $DIRS"
  for d in $DIRS; do
    setup_info "$TOP/$d"
    echo "=== $d: ${NGRID:-?}x${NGRID:-?}, $NK k-points (screening $WB0 bands, BSE $BANDS0, exchange $EXX0 Ry, W block $WBLK0 Ry${JS:+, RIM-W}) ==="
    preflight "$TOP/$d" $EXX0 $WBLK0 $WB0/$BANDS0
    run_bse "$TOP/$d" final$JS $WB0 $EXX0 $WBLK0 $BANDS0 || exit 1
  done
  setup_info "$TOP" ;; esac

# --- summary ---
case $STAGE in summary|all|wbands|wblock|exx|bands|dirs)
DIRS="$DIRS" python3 - "$TOP" "$TOL" <<'PY2'
import glob, os, re, sys
import numpy as np
top, tol = sys.argv[1], float(sys.argv[2])

def result(d, job):
    """-> (E_lowest, E_A, gap, Eb_A) from the BSE report; E_A from the eps peak if no report lines"""
    el = ea = gap = eb = np.nan
    for f in glob.glob(f'{d}/bconv/r-{job}_*'):
        if os.path.basename(f)[len(f'r-{job}_'):].startswith('rimw'):
            continue          # r-<job>_rimw_* belongs to the RIM-W run of the same scan point
        for l in open(f):
            if '[BSE] lowest excitation energies' in l:
                el = float(l.split(':')[-1].split()[0])
            elif '[BSE] lowest bright excitation' in l:
                ea = float(l.split()[-1])
            elif '[BSE] lowest transition energy of the window' in l:
                gap = float(l.split()[-1])
            elif '[BSE] binding energy, gap - lowest bright' in l:
                eb = float(l.split()[-1])
    if np.isnan(ea):
        fs = glob.glob(f'{d}/bconv/o-{job}.*_q1_*_bse')
        if fs:
            x = np.loadtxt(fs[0]); w, y = x[:, 0], x[:, 1]
            for i in range(1, len(y) - 1):
                if y[i] > 0.05 * y.max() and y[i] >= y[i - 1] and y[i] >= y[i + 1]:
                    ea = w[i]; break
    return el, ea, gap, eb

def table(title, rows, unit, fmt):
    """rows: (value, label, E_low, E_A, gap, Eb); flags the first value after which Eb stays within tol"""
    if not rows:
        return
    print(f'\n{title}')
    print(f'  {"value":>8s} {"E_lowest":>9s} {"E_A":>8s} {"gap":>8s} {"Eb(A)":>8s} {"change":>8s}')
    ch = [np.nan] + [rows[i][5] - rows[i - 1][5] for i in range(1, len(rows))]
    i0 = next((i for i in range(1, len(rows)) if all(abs(c) < tol for c in ch[i:])), None)
    for i, r in enumerate(rows):
        flag = ''
        if i == i0:
            flag = f'  <- converged at {rows[i - 1][1]} {unit} (all later changes < {tol*1000:.0f} meV)'
            if i == len(rows) - 1:
                flag += ': only the last step, add a point to confirm'
        print(f'  {r[1]:>8s} {r[2]:9.4f} {r[3]:8.4f} {r[4]:8.4f} {r[5]:8.4f} {ch[i]:8.4f}{flag}')
    if i0 is None and len(rows) > 1:
        print(f'  not converged within {tol*1000:.0f} meV: extend the list')

out = {}
for sfx, tag in (('', ''), ('_rimw', ' [RIM-W]')):
    for pre, title, unit in (('wb', 'Screening bands BndsRnXs', 'bands'), ('wblk', 'W block BSENGBlk (Ry)', 'Ry'),
                             ('exx', 'Exchange BSENGexx (Ry)', 'Ry'), ('bands', 'BSE bands nv:nc', '')):
        rows = []
        for f in glob.glob(f'{top}/bconv/o-{pre}_*{sfx}.*_q1_*_bse'):
            m = re.search(rf'o-{pre}_([0-9.]+(?:-[0-9]+)?){sfx}\.[a-z]+_q1_', f)
            if not m:
                continue
            v = m.group(1)
            key = tuple(float(x) for x in v.split('-'))
            rows.append((key, v.replace('-', ':'), *result(top, f'{pre}_{v}{sfx}')))
        rows.sort()
        table(title + tag, rows, unit, None)
        if rows and not sfx:
            out[pre] = rows

# k grids: final runs here, in subdirectories and in DIRS
fs = glob.glob(f'{top}/bconv/o-final*_q1_*_bse') + glob.glob(f'{top}/*/bconv/o-final*_q1_*_bse')
for d in os.environ.get('DIRS', '').split():
    fs += glob.glob(f'{top}/{d}/bconv/o-final*_q1_*_bse')
grids = {}
for f in sorted(set(os.path.normpath(x) for x in fs)):
    m = re.search(r'o-final(_rimw)?\.[a-z]+_q1_', f)
    d = os.path.dirname(os.path.dirname(f))
    if not m:
        continue
    n = 0
    try:
        n = int(next(l.split(':')[1].split()[0] for l in open(f'{d}/r_setup') if 'Grid dimensions' in l))
    except Exception:
        pass
    grids.setdefault(m.group(1) or '', []).append((n, os.path.relpath(d, top), *result(d, 'final' + (m.group(1) or ''))))
for sfx, rows in grids.items():
    rows.sort()
    print(f'\nk grids (final runs){" [RIM-W]" if sfx else ""}')
    print(f'  {"dir":>10s} {"grid":>6s} {"E_lowest":>9s} {"E_A":>8s} {"gap":>8s} {"Eb(A)":>8s}')
    for r in rows:
        print(f'  {r[1]:>10s} {r[0]:>3d}x{r[0]:<2d} {r[2]:9.4f} {r[3]:8.4f} {r[4]:8.4f} {r[5]:8.4f}')
    pts = [(r[0], r[5]) for r in rows if r[0] > 0 and np.isfinite(r[5])]
    if len(pts) >= 3:
        N = np.array([p[0] for p in pts], float); e = np.array([p[1] for p in pts])
        A = np.vstack([np.ones_like(N), 1 / N]).T
        (einf, a), *_ = np.linalg.lstsq(A, e, rcond=None)
        res = (A @ [einf, a] - e) * 1000
        print(f'  Eb(A) = Eb_inf + a/N fit: Eb_inf = {einf:.4f} eV (residuals {" ".join(f"{x:+.1f}" for x in res)} meV)')

try:
    import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot as plt
    pl = [(k, t) for k, t in (('wb', 'BndsRnXs'), ('wblk', 'BSENGBlk (Ry)'), ('exx', 'BSENGexx (Ry)'), ('bands', 'nv:nc')) if k in out]
    if pl:
        fig, ax = plt.subplots(1, len(pl), figsize=(4 * len(pl), 3.2), squeeze=False)
        for a, (k, t) in zip(ax[0], pl):
            a.plot(range(len(out[k])), [r[5] for r in out[k]], 'o-')
            a.set_xticks(range(len(out[k]))); a.set_xticklabels([r[1] for r in out[k]])
            a.set_xlabel(t); a.set_ylabel('Eb(A) (eV)')
        fig.tight_layout(); fig.savefig(f'{top}/bse_convergence.png', dpi=150)
        print(f'\nwrote {top}/bse_convergence.png')
except Exception as e:
    print('no plot:', e)
PY2
;; esac
