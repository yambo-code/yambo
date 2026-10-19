#!/bin/bash
# End-to-end MPI test of gw_converge.sh on a 12x12 non-SOC WS2 SAVE with the cluster's dimensions
# (VB 13, 19 IBZ k, gap at k 19), oversubscribed on this machine.
S=${S:?set S to the folder holding ybuild_mpi, gwmpi and jobs}
D=$S/gwmpi; J=$S/jobs/gw_converge.sh
export PATH=$S/ybuild_mpi/bin:$PATH OMP_NUM_THREADS=1 HDF5_USE_FILE_LOCKING=FALSE
MPIRUN="mpirun --allow-run-as-root --oversubscribe --bind-to none --mca mpi_yield_when_idle 1 -x HDF5_USE_FILE_LOCKING"
# CUTGeo none: oversubscribed ranks race on ndb.cutoff (Yambo writes it while slow ranks re-read it)
export CUT='CUTGeo= "none"' RIM=$'RandQpts= 10000\nRandGvec= 1 RL' FFTG="10 Ry" EXX0=5 BLOCK0=1
cd $D
run () { echo; echo "######## $*"; env "$@" RUNNER="$MPIRUN -np ${NP_T}" bash ${J} 2>&1 | grep -v "^ *$"; echo "######## exit ${PIPESTATUS[0]}"; }

case ${1:-all} in
old|all)
  # the layout of the failed cluster job: SE b over all 48 tasks; must fail in the Fock step
  rm -rf conv/old* conv/LOG; mkdir -p conv; ln -sf ../SAVE conv/SAVE 2>/dev/null
  cat > conv/old.in <<EOF
DIP_CPU= "1 48 1"
DIP_ROLEs= "k c v"
X_and_IO_CPU= "1 1 1 48 1"
X_and_IO_ROLEs= "q g k c v"
SE_CPU= "1 1 48"
SE_ROLEs= "q qp b"
HF_and_locXC
gw0
ppa
el_el_corr
dyson
rim_cut
em1d
CUTGeo= "slab z"
$RIM
FFTGvecs= 10 Ry
EXXRLvcs= 5 Ry
VXCRLvcs= 5 Ry
Chimod= "HARTREE"
% BndsRnXp
 1 | 64 |
%
NGsBlkXp= 1 Ry
% LongDrXp
 1.0 | 1.0 | 0.0 |
%
% GbndRnge
 1 | 64 |
%
%QPkrange
 19|19|13|14|
%
EOF
  echo "######## old layout (SE_CPU 1 1 48)"
  (cd conv && $MPIRUN -np 48 yambo -F old.in -J old > old.out 2>&1)
  ls conv/o-old.qp 2>/dev/null && echo "old layout ran (unexpected)"
  grep -h "ERROR" conv/LOG/l-old_* | sed 's/.*\[ERROR\]/[ERROR]/' | sort -u
  rm -rf conv/old* conv/[rl]-old_* conv/LOG/l-old_* ;;&
exx|all)  NP_T=48 run NP=48 STAGE=exx EXX_LIST="5 10" ;;&
bands|all) NP_T=48 run NP=48 STAGE=bands BANDS_LIST="72 74" ;;&
block|all) NP_T=24 run NP=24 STAGE=block BANDS0=64 BLOCK_LIST="1 2" ;;&
bad|all)
  NP_T=46 run NP=46 STAGE=bands BANDS_LIST="64 70"
  NP_T=48 run NP=48 STAGE=bands BANDS_LIST="64 100" ;;
esac
case ${1:-all} in
catch|all)
  # broken copy of the script: SE over b only (the layout of the failed cluster job)
  B=$S/jobs/broken/gw_converge.sh; mkdir -p $S/jobs/broken
  sed 's|printf .SE_CPU= "%d %d %d"\\nSE_ROLEs= "q qp b"\\n. ${s\[1\]} ${s\[2\]} ${s\[0\]}|printf "SE_CPU= \\"1 1 %d\\"\\nSE_ROLEs= \\"q qp b\\"\\n" $NP|' $J > $B
  grep -n 'SE_CPU= ' $B | head -3
  rm -rf conv/*bands_6[68]* conv/LOG conv/layouts_ok
  # with the check: stops in the check, no real run
  J=$B NP_T=48 run NP=48 STAGE=bands BANDS_LIST="66 68"
  ls conv/bands_66.in conv/bands_68.in 2>/dev/null || echo "-> no real run was started"
  # without the check: the first run fails, the stage stops before bands_68
  J=$B NP_T=48 run NP=48 CHECK=0 STAGE=bands BANDS_LIST="66 68"
  ls conv/bands_68.in 2>/dev/null || echo "-> bands_68 never started"
  # resubmission with the real script: the failed bands_66 is cleaned and redone
  NP_T=48 run NP=48 STAGE=bands BANDS_LIST="66 68"
  ls conv/o-bands_66*.qp conv/o-bands_68*.qp conv/r-bands_66_* ;;
esac
