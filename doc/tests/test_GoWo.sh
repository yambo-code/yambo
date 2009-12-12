#!/bin/bash

EPS=0.00005   # precision 5e-5

##########################################################
#  Reference data Yambo 3.2.1 r 512                      #
#  compiled with gfortran 4.3.3  (-O3 -mtune=native)     #
#  and ABINIT 5.3.4                                      #
##########################################################

# Compare real and imaginary part of 3 quasi-particles

nqp=3

  E[1]=0.7272034
 Eo[1]=0.5044853
  Z[1]=0.9908279

  E[2]=-0.3409601
 Eo[2]=-0.2926725
  Z[2]=0.8137928

  E[3]=-0.0009470499
 Eo[3]=0
  Z[3]=0.846016

  E[4]=0.0004661365
 Eo[4]=0
  Z[4]=0.002187072

  E[5]=0.0007431815
 Eo[5]=0
  Z[5]=-0.001660089
 
  E[6]=0.0005178482
 Eo[6]=0
  Z[6]=-0.001213804

##########################################################


# check whether echo has the -e option
if test "`echo -e`" = "-e" ; then ECHO=echo ; else ECHO="echo -e" ; fi

# run from directory where this script is
cd `echo $0 | sed 's/\(.*\)\/.*/\1/'` # extract pathname
TEST_DIR=`pwd`

$ECHO 
$ECHO " * * * * * * * * * * * * * * * * *"
$ECHO " *        Test GoWo              *"
$ECHO " * * * * * * * * * * * * * * * * *"
$ECHO 

if [ `which abinis | wc -c` -eq 0 ] ; then
  $ECHO " ABINIT is not in your path!"
  exit 1;
fi

if [ `which ncdump | wc -c` -eq 0 ] ; then
  $ECHO " NCDUMP is not in your path!"
  exit 1;
fi

if [ ! -f ../bin/a2y ] ; then
  $ECHO " Compile yambo interfaces before tests "
  exit 1;
fi

if [ ! -f ../bin/yambo ] ; then
  $ECHO " Yambo executable not found "
  exit 1;
fi

if [ -d test_GoWo ] ; then
  $ECHO " WARNING: directory test_GoWo already exists "
  $ECHO ""
fi

rm -rf test_GoWo
mkdir  test_GoWo

cd test_GoWo
$ECHO " Downloading pseudopotentials...... "
if (! wget ftp://ftp.abinit.org/pub/abinitio/Psps/LDA_TM.psps/05/5b.pspnc &> /dev/null) || ( ! wget ftp://ftp.abinit.org/pub/abinitio/Psps/LDA_TM.psps/07/7n.pspnc &> /dev/null ) then
$ECHO " Error downloading pseudo-potentials "
exit 1;
fi

cat > bn_dft.in << EOF
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
nband2  10
nbandkss2 -1
kssform2 3
tolwfr2  1.0d-9
ngkpt2 2 2 1
istwfk2 2*1
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

cat > bn.files << EOF
bn_dft.in
bn_dft.out
bn_dfti
bn_dfto
bn_dft
5b.pspnc
7n.pspnc
EOF

$ECHO " Running ABINIT calculation..... "

if (! abinis < bn.files > output_abinit ) then
$ECHO " Error running ABINIT "
exit 1;
fi

$ECHO " Import WF ..... "

if (! ../../bin/a2y -N -S -F bn_dfto_DS2_KSS &> output_a2y) then
$ECHO " Error running A2Y "
exit 1;
fi

$ECHO " Yambo Setup ..... "

cat > yambo_setup.in << EOF
setup                        # [R INI] Initialization
EOF

if (! ../../bin/yambo -N -F yambo_setup.in  &> output_setup) then
$ECHO " Error in YAMBO setup "
exit 1;
fi

cat > yambo.in << EOF
ppa                          # [R Xp] Plasmon Pole Approximation
gw0                          # [R GW] GoWo Quasiparticle energy levels
HF_and_locXC                 # [R XX] Hartree-Fock Self-energy and Vxc
em1d                         # [R Xd] Dynamical Inverse Dielectric Matrix
EXXRLvcs= 1283         RL    # [XX] Exchange RL components
% QpntsRXp
 1 | 2 |                     # [Xp] Transferred momenta
%
% BndsRnXp
  1 | 10 |                   # [Xp] Polarization function bands
%
NGsBlkXp= 1000  mHa       # [Xp] Response block size
% LongDrXp
 1.000000 | 0.000000 | 0.000000 |      # [Xp] [cc] Electric Field
%
PPAPntXp= 27.21138     eV    # [Xp] PPA imaginary energy
% GbndRnge
  1 | 8 |                   # [GW] G[W] bands range
%
GDamping=  0.10000     eV    # [GW] G[W] damping
dScStep=  0.10000      eV    # [GW] Energy step to evalute Z factors
DysSolver= "n"               # [GW] Dyson Equation solver 
%QPkrange                    # [GW] QP generalized Kpoint/Band indices
  1|  1|  9| 9|
  2|  2|  4| 4|
  2|  2|  2| 2|
%
%QPerange                    # [GW] QP generalized Kpoint/Energy indices
  1|  2| 0.0|-1.0|
%
EOF

$ECHO " Yambo GoWo ..... "

if (! ../../bin/yambo -N -F yambo.in  &> output_yambo) then
$ECHO " Error running YAMBO GoWo "
exit 1;
fi

ncdump SAVE/ndb.QP > data_ndb.QP
head_lines=`grep -n QP_E_Eo_Z data_ndb.QP | tail -1 | awk -F: ' { print $1 }'`

test_ok=1

#
# seq does not work on MacOsX !
#
#for i in `seq 1 $(echo 2*$nqp |bc -l)`   
#
for i in 1 2 3 4 5 6 
do 
    if [ "$i" -le "$nqp" ] ; then
       part="real"
    else
       part="imag"
    fi

  line=`python -c "print ${head_lines} + ${i}"`      
    E=`head -${line} data_ndb.QP | tail -1 | awk -F, '{ print $1 }'`
   Eo=`head -${line} data_ndb.QP | tail -1 | awk -F, '{ print $2 }'`
    Z=`head -${line} data_ndb.QP | tail -1 | awk '{ print $3 }' FS='[,;]'`

    diffE=`python -c "print \"%16.12f\" % abs(${E}-${E[i]})"`
    if [ $(echo "$diffE < $EPS"|bc -l) -eq 0 ] ; then
       $ECHO " Wrong $part(E) for  QP particle numer $i: E_diff  =  $diffE"
       test_ok=0
    fi     

    diffEo=`python -c "print \"%16.12f\" % abs(${Eo}-${Eo[i]})"`
    if [ $(echo "$diffEo < $EPS"|bc -l) -eq 0 ] ; then
       $ECHO " Wrong $part(Eo) for QP particle numer $i: Eo_diff  =  $diffEo"
       test_ok=0
    fi     

    diffZ=`python -c "print \"%16.12f\" % abs(${Z}-${Z[i]})"`
    if [ $(echo "$diffZ < $EPS"|bc -l) -eq 0 ] ; then
       $ECHO " Wrong $part(Z) for  QP particle numer $i: Z_diff  =  $diffZ"
       test_ok=0
    fi     
done

$ECHO ""

if [ "$test_ok" -eq 1 ] ; then
   $ECHO " Test GoWo ==>> OK "
else
   $ECHO " Test GoWo ==>> failed "
fi
cd ..
