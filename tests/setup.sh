#!/bin/bash

$ECHO $ECHO_N " [TESTs] Ground-state ... "
if (! abinis < files > gs.log ) then
 $ECHO " Error running ABINIT "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] A2Y ... "
if (! $A2Y -N -S -F gs_o_DS2_KSS &> output_a2y) then
 $ECHO " Error running A2Y "
 exit 1;
else
 $ECHO "done"
fi

$ECHO $ECHO_N " [TESTs] Yambo Setup ... "
if (! $YAMBO -N -F none &> output_setup) then
 $ECHO " Error in YAMBO setup "
 exit 1;
else
 $ECHO "done"
fi
