!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine INTERPOLATION_driver_do(operation,ID,OUT_k,OUT_E,FineGrid)
 !
 ! INTREPOLATION_driver_do at present interpolates both to the Coarse grid and to the Fine grid
 !
 ! Starting from the coefficients defined previously
 ! interpolates to OUT_k
 !
 ! INPUT: OUT_k,ID
 ! calls INTERPOLATION_BZ
 !  !
 !  calls INTERPOLATION_Fourier
 !  From the values on the shells defines the values on the given k-grid
 !
 use pars,                 ONLY:schlen
 use R_lattice,            ONLY:bz_samp
 use electrons,            ONLY:levels
 use interpolate_tools,    ONLY:INTERPOLATION_nearest_k_report
 use interpolate,          ONLY:INTERP_obj,INTERPOLATE_is_quiet,&
&                               INTERPOLATION_BZ,INTERPOLATION_nearest_k,&
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,&
&                               l_integral_respect,OUT_k_nk,NN_nk_exact
 use IO_m,                 ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 character(*),  intent(in)              :: operation
 integer,       intent(in)              :: ID
 type(bz_samp), intent(inout)           :: OUT_k
 type(levels),  intent(inout), optional :: OUT_E
 character(*),  intent(in)   , optional :: FineGrid
 !
 ! Work Space
 !
 integer            :: n(2),nk1,nk2,dimI
 character(schlen)  :: FG_mode
 !
 if (operation/=trim(INTERP_obj(ID)%what)) call error(' Interopolation ID did not match operation name')
 !
 ! ... FG mode
 FG_mode="ibz"
 if (present(FineGrid)) then
   FG_mode=FineGrid
 endif
 !
 ! ... Allocation
 n =INTERP_obj(ID)%nb
 nk1 =OUT_k%nibz
 dimI=INTERP_obj(ID)%D
 !
 ! Needed for report
 OUT_k_nk=0
 NN_nk_exact=0
 !
 if(nk1>0) OUT_k_nk=OUT_k_nk+OUT_k%nibz
 nk2 = 0
 if(OUT_k%FGibz%N>0) then
   if (trim(FG_mode)=="ibz")  nk2=OUT_k%FGibz%N
   if (trim(FG_mode)=="bz")   nk2=OUT_k%FGbz%N
   if (trim(FG_mode)=="bare") nk2=OUT_k%FGbare%N
   OUT_k_nk=OUT_k_nk+nk2
 endif
 !
 if(nk1>0) then
   YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),nk1,dimI))
 endif
 if(nk2>0) then
   YAMBO_ALLOC(INTERP_obj(ID)%INTERP_FineGd,(n(1):n(2),nk2,dimI))
 endif
 !
 ! ... Interpolation...
 !
 if (INTERP_obj(ID)%use_NN) then
   ! ... Using the Nearest point method
   if(nk1>0) call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%pt      ,INTERP_obj(ID)%INTERP)
   if(nk2>0) then
     if (trim(FG_mode)=="ibz")  then
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGibz%pt,INTERP_obj(ID)%INTERP_FineGd)
     else if (trim(FG_mode)=="bz")  then
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGbz%pt,INTERP_obj(ID)%INTERP_FineGd)
     else
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGbare%pt,INTERP_obj(ID)%INTERP_FineGd)
     endif
   endif
   if (.not.INTERPOLATE_is_quiet) call INTERPOLATION_nearest_k_report(ID)
   !
 else
   !
   ! ... Using BOLTZ method
   if(nk1>0) call INTERPOLATION_BZ(OUT_k,nk1,INTERP_obj(ID)%ID,R2D=INTERP_obj(ID)%INTERP)
   if(nk2>0) call INTERPOLATION_BZ(OUT_k,nk2,INTERP_obj(ID)%ID,R2D=INTERP_obj(ID)%INTERP_FineGd)
   !
 endif
 !
 ! f/dF normalization
 !====================
 if (l_INTERP_f.and.l_integral_respect) then
   if(nk1>0) call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP       ,l_integral_respect,nk1)
   if(nk2>0) call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP_FineGd,l_integral_respect,nk2)
 endif
 !
 ! store f/E/dE in OUT_E
 !=======================
 if (present(OUT_E)) then
   if (l_INTERP_E.or.l_INTERP_dE) then
     YAMBO_ALLOC(OUT_E%E,(n(1):n(2),nk1,dimI))
     if(nk2>0) then
       YAMBO_ALLOC(OUT_E%FG%E,(n(1):n(2),nk2,dimI))
     endif
   else if (l_INTERP_f) then
     YAMBO_ALLOC(OUT_E%f,(n(1):n(2),nk1,dimI))
     if(nk2>0) then
       YAMBO_ALLOC(OUT_E%FG%f,(n(1):n(2),nk2,dimI))
     endif
   endif
   OUT_E%nk       =OUT_k%nibz
   !
   if (trim(FG_mode)=="ibz")  OUT_E%FG%nk=OUT_k%FGibz%N
   if (trim(FG_mode)=="bz")   OUT_E%FG%nk=OUT_k%FGbz%N
   if (trim(FG_mode)=="bare") OUT_E%FG%nk=OUT_k%FGbare%N
   !
   OUT_E%nb=INTERP_obj(ID)%nb(2)
   if ((l_INTERP_E.or.l_INTERP_dE).and.nk1>0) OUT_E%E       =INTERP_obj(ID)%INTERP
   if ((l_INTERP_f               ).and.nk1>0) OUT_E%f       =INTERP_obj(ID)%INTERP
   if ((l_INTERP_E.or.l_INTERP_dE).and.nk2>0) OUT_E%FG%E=INTERP_obj(ID)%INTERP_FineGd
   if ((l_INTERP_f               ).and.nk2>0) OUT_E%FG%f=INTERP_obj(ID)%INTERP_FineGd
 endif
 !
 if (INTERPOLATE_is_quiet) call IO_and_Messaging_switch("+log")
 !
end subroutine INTERPOLATION_driver_do
