#!/bin/bash

### PATHS AND EXECUTABLE ###
source  tests/load_paths_and_executables.sh

EPS=0.00005   # precision 5e-5

#############################################################################
#  Reference data Yambo 3.2.1 r 511                                         #
#  compiled with gfortran 4.3.2  (-g -fbounds-check -Wuninitialized -O)     #
#  and ABINIT 5.8.4                                                         #
#############################################################################
  
#E_up_landau=(   -0.9695717 -0.5407296 -0.2600582 -0.2389626 -0.2385391 -0.0004087206)
#E_down_landau=( -0.9206337 -0.476001  -0.2181368 -0.1754182 -0.1749939  0.07520718  )
#E_up_pauli=(    -0.9693784 -0.540536  -0.2598646 -0.2385588 -0.2385559 -0.0002168596)
#E_down_pauli=(  -0.9208654 -0.4762328 -0.2183685 -0.175439  -0.1754365  0.07517388  )
#E_up_all=(      -0.9695718 -0.5407296 -0.2600582 -0.2389627 -0.2385392 -0.0004087853)
#E_down_all=(    -0.9210591 -0.4764264 -0.2185622 -0.1758436 -0.1754194  0.07478175  )

# Updated to the results of rev. 523 where the Fermi level is updated during the SC cycle.
# This results in a rigid shift if compared with the old numbers.

E_up_landau=(   -0.9695464 -0.5407289 -0.2600741 -0.2389783 -0.2385499 -0.0004147867  )
E_down_landau=( -0.9205958 -0.4760033 -0.2181428 -0.1754237 -0.1749994 0.07521054  )
E_up_pauli=(    -0.9693455 -0.5405279 -0.2598734 -0.2385661 -0.2385606 -1.362886e-05  )
E_down_pauli=( -0.9208202 -0.4762276 -0.2183674 -0.1754372 -0.1754348 0.07518419  )
E_up_all=(    -0.9695467 -0.5407289 -0.2600742 -0.2389783 -0.23855 -0.0004149192   )
E_down_all=(   -0.9210215 -0.4764285 -0.2185682 -0.1758491 -0.1754248 0.07478501  )


cd $prefix/tests
rm -fr magnetic_dir
mkdir magnetic_dir
cd $prefix/tests/magnetic_dir


cat > gs.in << EOF
##########  O2 ground state calculation ##################
ndtset 2

#Definition of the k-point grid
kptopt   0
nkpt     1        # Only one k point is needed for isolated system,
                  # taken by default to be 0.0 0.0 0.0

#Definition of the system
ntypat    1
znucl     8
natom     2
typat     2*1
nsppol    2
occopt    2
nband1    7 5
occ1      1 1 1 1 1 1 1 1 1 1 1 1

#Convergence parameters
ecut    18         # Maximal kinetic energy cut-off

#Definition of theupercell
acell      5 5 6.4  Angstrom
#Definition of the positions of the atoms
 xangst
           0.000   0.000  -0.620
           0.000   0.000   0.620

#DATASET 1: SCF-RUN
nstep1  100               # Maximal number of SCF cycles
toldfe1   1.0d-4          # Will stop when, twice in a row, the difference
diemac1   1.5             # It is worth to precondition the SCF cycle.
diemix1   0.5
prtden1   1               # Print the density, for use by dataset 2


#DATASET 2: KSS GENERATION
iscf2       -2               # Non self-consistent calculation
getden2      1               # Read previous density file
tolwfr2      1.0d-5          # Still get it converged
nstep2     100
nband2       9  9
nbandkss2    9  9
kssform2     3
nbdbuf2     10
istwfk2     1               
EOF


cat > files << EOF
gs.in
gs.out
gs_i    
gs_o
gs    
../PPs/8o.pspnc
EOF

cat > ypp_fix_symm.in << EOF
rsymm                        # [R] Reduce Symmetries
% EField1
 0.000    | 0.000    | 0.000    |      # First external Electric Field
%
% EField1
 0.000    | 0.000    | 0.000    |      # Additional external Electric Field
%
BField= 100.0000       T     # [MAG] Magnetic field modulus
Bpsi= 0.000000         deg   # [MAG] Magnetic field psi angle [degree]
Btheta= 0.000000       deg   # [MAG] Magnetic field theta angle [degree]
#RmAllSymm                   # Remove all symmetries
EOF

cat > yambo_landau.in << EOF
scpot                        # [R] Self-Consistent potentials
magnetic                     # [R] Magnetic fields
Potential= "default"         # [SC] SC Potential
SCBands=  9                  # [SC] Bands
BandMix= 100.0000            # [SC] Band mixing
SCIter= 100                  # [SC] SC Iterations
SCEtresh=   0.0100     eV    # [SC] Energy convergence threshold
SCRhoTresh=0.1000E-4         # [SC] Rho convergence threshold
Hamiltonian= "landau"        # [MAG] Hamiltonian kind [pauli,landau]
B_Field= 100.0000      T     # [MAG] Magnetic field modulus
NonPerDir= "XYZ"             # [MAG] Non periodic chartesian directions (X,Y,Z,XY...)
EOF

cat > yambo_pauli.in << EOF
scpot                        # [R] Self-Consistent potentials
magnetic                     # [R] Magnetic fields
Potential= "default"         # [SC] SC Potential
SCBands=  9                  # [SC] Bands
BandMix= 100.0000            # [SC] Band mixing
SCIter= 100                  # [SC] SC Iterations
SCEtresh=   0.0100     eV    # [SC] Energy convergence threshold
SCRhoTresh=0.1000E-4         # [SC] Rho convergence threshold
Hamiltonian= "pauli"         # [MAG] Hamiltonian kind [pauli,landau]
B_Field= 100.0000      T     # [MAG] Magnetic field modulus
NonPerDir= "XYZ"             # [MAG] Non periodic chartesian directions (X,Y,Z,XY...)
EOF

cat > yambo_all.in << EOF
scpot                        # [R] Self-Consistent potentials
magnetic                     # [R] Magnetic fields
Potential= "default"         # [SC] SC Potential
SCBands=  9                  # [SC] Bands
BandMix= 100.0000            # [SC] Band mixing
SCIter= 100                  # [SC] SC Iterations
SCEtresh=   0.0100     eV    # [SC] Energy convergence threshold
SCRhoTresh=0.1000E-4         # [SC] Rho convergence threshold
Hamiltonian= "all"           # [MAG] Hamiltonian kind [pauli,landau]
B_Field= 100.0000      T     # [MAG] Magnetic field modulus
NonPerDir= "XYZ"             # [MAG] Non periodic chartesian directions (X,Y,Z,XY...)
EOF

### SETUP ####
source  $prefix/tests/setup.sh

### YAMBO ####

$ECHO $ECHO_N " [TESTs] Ypp ..."
if (! ${YPP_RT} -N -F ypp_fix_symm.in  &> output_setup) then
 $ECHO " Error in Ypp_rt fix symmetries "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] 2nd Setup ... "
if (! ${YAMBO_MAGNETIC} -N -F yambo_setup.in  &> output_setup) then
 $ECHO " Error in YAMBO setup2 "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] Landau ... "
if (! ${YAMBO_MAGNETIC} -N -F yambo_landau.in -J MAG_landau  &> output_yambo) then
 $ECHO " Error running Yambo_magnetic (landau) ..... "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] Pauli ... "
if (! ${YAMBO_MAGNETIC} -N -F yambo_pauli.in -J MAG_pauli  &> output_yambo) then
 $ECHO " Error running Yambo_magnetic (pauli) ..... "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] Landau + Pauli ... "
if (! ${YAMBO_MAGNETIC} -N -F yambo_all.in -J MAG_all  &> output_yambo) then
 $ECHO " Error running Yambo_magnetic (all) ..... "
 exit 1;
else
 $ECHO "done"
fi


ncdump MAG_landau/ndb.scE > data_landau.scE
ncdump MAG_pauli/ndb.scE > data_pauli.scE
ncdump MAG_all/ndb.scE > data_all.scE
head_lines_landau=`grep -n E_sc data_landau.scE | tail -1 | awk -F : ' { print $1 }'`
head_lines_pauli=`grep -n E_sc data_pauli.scE | tail -1 | awk -F : ' { print $1 }'`
head_lines_all=`grep -n E_sc data_all.scE | tail -1 | awk -F : ' { print $1 }'`

test_landau=1
test_pauli=1
test_all=1


for b in 1 2 3 4 5 6
do
 # Check the Energies for the two spin channels (landau)
 bm1=$(echo $b-1|bc -l)

 line=`python -c "print ${head_lines_landau} + 1"`
 E_landau[b]=`head -${line} data_landau.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_up_landau[bm1]}-${E_landau[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(landau) for QP particle spin up band $b: E_diff  =  $diffE"
   $test_landau=0
 fi

 line=`python -c "print ${head_lines_landau} + 3"`
 E_landau[b]=`head -${line} data_landau.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_down_landau[bm1]}-${E_landau[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(landau) for QP particle spin down band $b: E_diff  =  $diffE"
   test_landau=0
 fi

 # Check the Energies for the two spin channels (pauli)
 line=`python -c "print ${head_lines_pauli} + 1"`
 E_pauli[b]=`head -${line} data_pauli.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_up_pauli[bm1]}-${E_pauli[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(pauli) for QP particle spin up band $b: E_diff  =  $diffE"
   test_pauli=0
 fi

 line=`python -c "print ${head_lines_pauli} + 3"`
 E_pauli[b]=`head -${line} data_pauli.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_down_pauli[bm1]}-${E_pauli[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(pauli) for QP particle spin down band $b: E_diff  =  $diffE"
   test_pauli=0
 fi


 # Check the Energies for the two spin channels (all)
 line=`python -c "print ${head_lines_all} + 1"`
 E_all[b]=`head -${line} data_all.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_up_all[bm1]}-${E_all[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(all) for QP particle spin up band $b: E_diff  =  $diffE"
   test_all=0
 fi

 line=`python -c "print ${head_lines_all} + 3"`
 E_all[b]=`head -${line} data_all.scE | tail -1 | awk -F, '{ print $'$b'}'`
 diffE=`python -c "print \"%16.12f\" % abs(${E_down_all[bm1]}-${E_all[b]})"`
 if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
   $ECHO " Wrong E_sc(all) for QP particle spin down band $b: E_diff  =  $diffE"
   $test_all=0
 fi
done

$ECHO ""
if [ "$test_landau" -eq 1 ] ; then
   $ECHO " [TESTs] === Test magnetic (Landau) OK ==="
else
   $ECHO " [TESTs] === Test magnetic (Landau) FAILED ==="
fi

if [ "$test_pauli" -eq 1 ] ; then
   $ECHO " [TESTs] === Test magnetic (Pauli) OK ==="
else
   $ECHO " [TESTs] === Test magnetic (Pauli) FAILED ==="
fi

if [ "$test_all" -eq 1 ] ; then
   $ECHO " [TESTs] === Test magnetic (Landau+Pauli) OK ==="
else
   $ECHO " [TESTs] === Test magnetic (Landau+Pauli) FAILED ==="
fi

cd ..
