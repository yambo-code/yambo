#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=normal
#SBATCH --mem=150G
#SBATCH --time=06:00:00
#SBATCH --job-name=fkq_dump
#SBATCH --output=log/fkq_dump.o%A
#SBATCH --error=log/fkq_dump.e%A

# F_tt'(q, w): the exported kernel projected on the transitions (Casida matrix pieces),
# at the q points QS and the frequency window W1..W2 (eV). Uses kx_fq/ndb.Chi of fq_export.sh,
# so run it from 06_TEST_VAL after that job. Needs the fork with the finite-q dump
# (git pull in ~/yambo-cpu + make yambo).
#
#  W1=W2=0     : static kernel only (z = 0 and z = 0 + i eta)
#  VB/CB       : one valence | conduction pair (0 0 = all 2025 transitions; ~1.1 GB of text
#                per frequency file for all of them at one q)
# Output per q in fkq_files_q<iq>/: o-fkq_q<iq>.Ktt_q<iq>_transitions, _antiresonant (q>1), _w<iw>.

BANDS=${BANDS:-"11 | 16"}
NG=${NG:-21}
GINP=${GINP:-1}
QS=${QS:-"1 2 5 8"}
W1=${W1:-0.0}; W2=${W2:-0.0}
VB=${VB:-0}; CB=${CB:-0}
WMAX=${WMAX:-4.0}; NW=${NW:-201}; ETA=${ETA:-0.05}
QP=${QP:-'KfnQPdb= "E < g0w0/ndb.QP"'}
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
grep -q "Ktt_q'//trim(intc(iq))//'_antiresonant" $YSRC/src/bse/K_Chi_Kdump.F || { echo "!!! $YSRC lacks the finite-q dump: git pull + make yambo"; exit 1; }
test -e kx_fq/ndb.Chi || { echo "!!! kx_fq/ndb.Chi missing: run fq_export.sh first"; exit 1; }
EXTRA=""; [ "$GINP" = 1 ] && EXTRA="BSEGinplane"

for q in $QS; do
  j=fkq_q$q
  cat > $j.in <<EOI
optics
bss
bse
dipoles
BSKmod= "CHI"
BSEmod= "coupling"
BSSmod= "d"
BSEChiDyn
$EXTRA
BSENGexx= $NG RL
BSENGfxc= $NG RL
K_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
% BSEQptR
 $q | $q |
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
% BSEChiKoutW
 $W1 | $W2 | eV
%
% BSEChiKoutB
 $VB | $CB |
%
$QP
EOI
  rm -rf $j; rm -f [rlo]-$j[._]*
  yambo $YFLAGS -F $j.in -J "$j,kx_fq" > log/$j.out 2>&1 || { echo "!!! $j failed"; tail -5 log/$j.out; continue; }
  mkdir -p fkq_files_q$q && mv o-$j.Ktt_* fkq_files_q$q/ 2>/dev/null
  echo "=== q$q: $(ls fkq_files_q$q | wc -l) files"; grep -h "Casida matrix written\|explicit anti" r-${j}_* | sed 's/^ */  /'
done
