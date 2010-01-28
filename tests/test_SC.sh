#!/bin/bash

### PATHS AND EXECUTABLE ###
source  tests/load_paths_and_executables.sh

EPS=0.00005   # precision 5e-5

##########################################################
#  Reference data Yambo 3.2.1 r 512                      #
#  compiled with gfortran 4.3.3  (-O3 -mtune=native)     #
#  and ABINIT 5.3.4                                      #
##########################################################
#E_kpt1=(-0.8728241 -0.2853783 -0.1462003 -0.1441232 0.1795345 0.5165715)
#E_kpt2=(-0.7379777 -0.4703602 -0.2650903 -0.1071636 0.2981534 0.487517)

# Updated to the results of rev. 523 where the Fermi level is updated during the SC cycle.
# This results in a rigid shift if compared with the old numbers.

E_kpt1=( -0.7656566 -0.1782096 -0.03903055 -0.03695412 0.2867047 0.6237394 )
E_kpt2=( -0.6308091 -0.3631924 -0.1579197   0          0.4053237 0.5946862 )

cd $prefix/tests
rm -fr SC_dir
mkdir SC_dir
cd $prefix/tests/SC_dir

cat > gs.in << EOF
ndtset 2

nstep 100
kptopt  1        # Option for the automatic generation of k points
nshiftk 1
shiftk  0 0 0
ngkpt1 6 6 1
enunit 1
prteig 1

prtvol1 3
toldfe1 1.0d-9
prtden1 1

symmorphi2 0
iscf2    -2
kptopt2  1        # Option for the automatic generation of k points
nband2  8
nbandkss2 -1
kssform2 3
tolwfr2  1.0d-9
ngkpt2 2 2 2
istwfk2 4*1
getden2 -1

#Definition of the planewave basis set
ecut   25.0        # Minimal kinetic energy cut-off, in Hartree

acell    4.7177372151  4.7177372151 10.0
rprim    1.00000000000000   0.000000000000000   0.00000000000000
        -0.50000000000000   0.866025403784439   0.00000000000000
         0.00000000000000   0.000000000000000   1.00000000000000

ntypat  2
znucl   5 7 
natom   2
typat   1 2 
xcart
  2.3588686075E+00  1.3618934255E+00  0.0000000000E+00
 -2.3588686075E+00 -1.3618934255E+00  0.0000000000E+00
EOF

cat > files << EOF
gs.in
gs.out
gs_i    
gs_o
gs    
../PPs/5b.pspnc
../PPs/7n.pspnc
EOF

cat > yambo.in << EOF
#
scpot                        # [R] Self-Consistent potentials
cohsex                       # [R Xp] COlumb Hole Screened EXchange
HF_and_locXC                 # [R XX] Hartree-Fock Self-energy and Vxc
em1s                         # [R Xs] Static Inverse Dielectric Matrix
EXXRLvcs= 309          RL    # [XX] Exchange RL components
% BndsRnXs
  1 |  6 |                   # [Xs] Polarization function bands
%
NGsBlkXs= 23           RL    # [Xs] Response block size
% LongDrXs
0.1000E-4 | 0.000    | 0.000    |      # [Xs] [cc] Electric Field
%
Potential= "COHSEX"          # [SC] SC Potential
SCBands=  6                  # [SC] Bands
BandMix= 100.0000            # [SC] Band mixing
SCIter= 100                  # [SC] SC Iterations
SCEtresh=   0.00100     eV    # [SC] Energy convergence threshold
SCRhoTresh=0.1000E-5         # [SC] Rho convergence threshold
EOF

### SETUP ####
source  $prefix/tests/setup.sh

### YAMBO ####
$ECHO $ECHO_N " [TESTs] Yambo SC ... "
if (! ${YAMBO_SC} -N -F yambo.in  &> output_yambo) then
 $ECHO " Error running YAMBO SC "
 exit 1;
else
 $ECHO "done"
fi

ncdump SAVE/ndb.scE > data_ndb.scE
head_lines=`grep -n E_sc data_ndb.scE | tail -1 | awk -F: ' { print $1 }'`

test_ok=1

# Check the Energies for the first two k-points

for b in 1 2 3 4 5 6
do
 bm1=$(echo $b-1|bc -l)

 line=`python -c "print ${head_lines} + 1"`      
 E[b]=`head -${line} data_ndb.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_kpt1[bm1]}-${E[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc for  QP particle kpoint 1 band $b: E_diff  =  $diffE"
   test_ok=0
 fi

 line=`python -c "print ${head_lines} + 2"`      
 E[b]=`head -${line} data_ndb.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_kpt2[bm1]}-${E[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc for  QP particle kpoint 2 band $b: E_diff  =  $diffE"
   test_ok=0
 fi

done

if [ "$test_ok" -eq 1 ] ; then
   $ECHO " [TESTs] === Test SC OK ==="
else
   $ECHO " [TESTs] === Test SC FAILED ==="
fi
