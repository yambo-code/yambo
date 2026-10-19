#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=48
#SBATCH --partition=normal
#SBATCH --mem=100G
#SBATCH --time=02:00:00
#SBATCH --job-name=finiteq
#SBATCH --output=log/finiteq.o%A
#SBATCH --error=log/finiteq.e%A

# Item 1: does this Yambo build solve the COUPLED BSE at finite momentum q?
# Stock Yambo only (no export yet). Run from 06_TEST_VAL, same settings as binding.sh.
#
#   fq_cpl : coupling (no TDA) at q-point IQ        -> the case the finite-q export needs
#   fq_res : resonant (TDA) at the same q            -> reference: must run, and differ from fq_cpl
#   fq_q1  : coupling at q -> 0 (iq = 1)             -> must reproduce bse_sex_exc of binding.sh
#
# Send back: log/finiteq.o*, r-fq_*, o-fq_*.eps*, and the q list (r_setup or r-fq_cpl_* "Q-points").

BANDS=${BANDS:-"11 | 16"}
NG=${NG:-1}                      # 1 RL is enough for this check and fast
WBLK=${WBLK:-"5 Ry"}
WDIR=${WDIR:-cohsex}
QP=${QP:-'KfnQPdb= "E < g0w0/ndb.QP"'}
IQ=${IQ:-2}                      # smallest finite q of the grid (check the q list in r_setup)
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
test -e $WDIR/ndb.em1s || { echo "!!! $WDIR/ndb.em1s missing"; exit 1; }
rm -rf w_em1s; mkdir -p w_em1s; for f in $WDIR/ndb.em1s*; do ln -s "$PWD/$f" w_em1s/; done

bse_in () {   # file mode iq
  cat > $1 <<EOF
optics
bss
bse
dipoles
BSKmod= "SEX"
BSEmod= "$2"
BSSmod= "d"
BSENGexx= $NG RL
BSENGBlk= $WBLK
K_Threads= $OMP_NUM_THREADS
DIP_Threads= $OMP_NUM_THREADS
% BSEQptR
 $3 | $3 |
%
% BSEBands
 $BANDS |
%
% BEnRange
 0.0 | 4.0 | eV
%
% BDmRange
 0.05 | 0.05 | eV
%
BEnSteps= 401
% BLongDir
 1.0 | 0.0 | 0.0 |
%
BSEprop= "abs"
$QP
EOF
}
run () {   # job
  rm -rf $1; rm -f [rlo]-$1[._]*
  yambo $YFLAGS -F $1.in -J "$1,w_em1s" > log/$1.out 2>&1
  if ls o-$1.eps* >/dev/null 2>&1; then echo "  $1: OK"; else echo "!!! $1 failed (see l-$1* and log/$1.out)"; tail -5 log/$1.out; fi
}

bse_in fq_cpl.in coupling $IQ;  run fq_cpl
bse_in fq_res.in resonant $IQ;  run fq_res
bse_in fq_q1.in  coupling 1;    run fq_q1

echo; echo "=== q point and its |q| ==="
grep -h -i -A1 "Q-point\|BSE q-point\|@q" r-fq_cpl_* 2>/dev/null | head -8
echo "=== lowest excitation energies ==="
grep -h "\[BSE\] lowest excitation energies\|\[BSE\] lowest bright" r-fq_cpl_* r-fq_res_* r-fq_q1_* 2>/dev/null
echo "=== anti-resonant handling / warnings ==="
grep -h -i "anti\|ares\|Lkind\|WARNING\|ERROR" r-fq_cpl_* l-fq_cpl_* 2>/dev/null | sort -u | head -20
ls -l o-fq_*
