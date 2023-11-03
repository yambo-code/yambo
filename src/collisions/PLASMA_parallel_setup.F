!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine PLASMA_parallel_setup(EH_gas)
 !
 use parallel_m,    ONLY:PAR_IND_Plasma,PAR_COM_PLASMA_INDEX,PAR_IND_PLASMA_ID,PAR_PLASMA_index,&
&                        PAR_nPlasma
 use parallel_int,  ONLY:PARALLEL_live_message,PARALLEL_index
 use plasma,        ONLY:plasma_gas
 !
#include<memory.h>
 !
 type(plasma_gas) :: EH_gas
 !
 integer :: i_p
 !
 call PARALLEL_index(PAR_IND_Plasma,(/EH_gas%max_N_poles/),COMM=PAR_COM_PLASMA_INDEX)
 PAR_IND_PLASMA_ID=PAR_COM_PLASMA_INDEX%CPU_id
 PAR_nPlasma=PAR_IND_Plasma%n_of_elements(PAR_IND_PLASMA_ID+1)
 !
 YAMBO_ALLOC(PAR_PLASMA_index,(EH_gas%max_N_poles))
 !
 PAR_PLASMA_index=0
 PAR_nPlasma=0
 !
 do i_p=1,EH_gas%max_N_poles
   if (PAR_IND_Plasma%element_1D(i_p)) then
     PAR_nPlasma=PAR_nPlasma+1
     PAR_PLASMA_index(i_p)=PAR_nPlasma
   endif
 enddo
 !
 call PARALLEL_live_message("Plasma gas",ENVIRONMENT="Real_Time",LOADED=PAR_nPlasma,TOTAL=EH_gas%max_N_poles,&
&                           NCPU=PAR_COM_PLASMA_INDEX%n_CPU)
 !
end subroutine PLASMA_parallel_setup
