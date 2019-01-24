!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
! 
! This file is distributed under the terms of the GNU 
! General Public License. You can redistribute it and/or 
! modify it under the terms of the GNU General Public 
! License as published by the Free Software Foundation; 
! either version 2, or (at your option) any later version.
!
! This program is distributed in the hope that it will 
! be useful, but WITHOUT ANY WARRANTY; without even the 
! implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License 
! for more details.
!
! You should have received a copy of the GNU General Public 
! License along with this program; if not, write to the Free 
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston, 
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine PLASMA_parallel_setup(EH_gas)
 !
 use parallel_m,    ONLY:PAR_IND_Plasma,PAR_COM_PLASMA_INDEX,PAR_IND_PLASMA_ID,PAR_PLASMA_index,&
&                        PAR_nPlasma
 use interfaces,    ONLY:PARALLEL_live_message,PARALLEL_index
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
