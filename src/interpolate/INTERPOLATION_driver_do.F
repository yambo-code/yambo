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
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,INTERP_FineGd,&
&                               l_integral_respect,OUT_k_nk
 use IO_m,                 ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 character(*),  intent(in)              :: operation
 integer,       intent(in)              :: ID
 type(bz_samp), intent(inout)           :: OUT_k
 type(levels),  intent(inout), optional :: OUT_E
 character(*),  intent(inout), optional :: FineGrid
 !
 ! Work Space
 !
 integer           :: n(2),Nk_FineGd,dimI
 character(schlen) :: INTERP_FineGd_save
 !
 if (operation/=trim(INTERP_obj(ID)%what)) call error(' Interpolation ID did not match operation name')
 !
 ! ... FG mode
 INTERP_FineGd="BARE"
 if (present(FineGrid)) then
   INTERP_FineGd=FineGrid
 endif
 INTERP_FineGd_save=INTERP_FineGd
 INTERP_FineGd="NONE"
 !
 ! ... Dimensions & Allocations
 n         =INTERP_obj(ID)%nb
 dimI      =INTERP_obj(ID)%D
 !
 if(SWITCH_the_grid("COARSE")) then
   YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),OUT_k%nibz,dimI))
 endif
 if(SWITCH_the_grid("FINE")) then
   YAMBO_ALLOC(INTERP_obj(ID)%INTERP_FineGd,(n(1):n(2),Nk_FineGd,dimI))
 endif
 !
 ! ... Interpolation...
 !
 if (INTERP_obj(ID)%use_NN) then
   ! ... Using the Nearest point method
   !
   if(SWITCH_the_grid("COARSE")) then
     call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%pt      ,INTERP_obj(ID)%INTERP)
     if (.not.INTERPOLATE_is_quiet) call INTERPOLATION_nearest_k_report(ID)
   endif
   !
   if(SWITCH_the_grid("FINE")) then
     if (trim(INTERP_FineGd)=="IBZ")  then
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGibz%pt,INTERP_obj(ID)%INTERP_FineGd)
     else if (trim(INTERP_FineGd)=="BZ")  then
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGbz%pt,INTERP_obj(ID)%INTERP_FineGd)
     else if (trim(INTERP_FineGd)=="BARE")  then
       call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%FGbare%pt,INTERP_obj(ID)%INTERP_FineGd)
     endif
     if (.not.INTERPOLATE_is_quiet) call INTERPOLATION_nearest_k_report(ID)
   endif
   !
 else
   !
   ! ... Using BOLTZ method
   if (SWITCH_the_grid("COARSE")) call INTERPOLATION_BZ(OUT_k,OUT_k%nibz,INTERP_obj(ID)%ID,R2D=INTERP_obj(ID)%INTERP)
   if (SWITCH_the_grid("FINE")  ) call INTERPOLATION_BZ(OUT_k,Nk_FineGd,INTERP_obj(ID)%ID,R2D=INTERP_obj(ID)%INTERP_FineGd)
   !
 endif
 !
 ! f/dF normalization
 !====================
 if (l_INTERP_f.and.l_integral_respect) then
   if (SWITCH_the_grid("COARSE")) then
     call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP       ,l_integral_respect,OUT_k%nibz)
   endif
   if (SWITCH_the_grid("FINE")) then
     call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP_FineGd,l_integral_respect,Nk_FineGd)
   endif
 endif
 !
 ! store f/E/dE in OUT_E
 !=======================
 if (present(OUT_E)) then
   !
   OUT_E%nb=INTERP_obj(ID)%nb(2)
   !
   if (SWITCH_the_grid("COARSE")) then
     OUT_E%nk       =OUT_k%nibz
     if (l_INTERP_E.or.l_INTERP_dE) then
       YAMBO_ALLOC(OUT_E%E,(n(1):n(2),OUT_k%nibz,dimI))
       OUT_E%E=INTERP_obj(ID)%INTERP
     endif
     if (l_INTERP_f) then
       YAMBO_ALLOC(OUT_E%f,(n(1):n(2),OUT_k%nibz,dimI))
       OUT_E%f=INTERP_obj(ID)%INTERP
     endif
   endif
   if (SWITCH_the_grid("FINE")) then
     if (trim(INTERP_FineGd)=="IBZ")  OUT_E%FG%nk=OUT_k%FGibz%N
     if (trim(INTERP_FineGd)=="BZ")   OUT_E%FG%nk=OUT_k%FGbz%N
     if (trim(INTERP_FineGd)=="BARE") OUT_E%FG%nk=OUT_k%FGbare%N
     if (l_INTERP_E.or.l_INTERP_dE) then
       YAMBO_ALLOC(OUT_E%FG%E,(n(1):n(2),Nk_FineGd,dimI))
       OUT_E%FG%E=INTERP_obj(ID)%INTERP_FineGd
     endif
     if (l_INTERP_f) then
       YAMBO_ALLOC(OUT_E%FG%f,(n(1):n(2),Nk_FineGd,dimI))
     endif
     OUT_E%FG%f=INTERP_obj(ID)%INTERP_FineGd
   endif
   !
 endif
 !
 if (INTERPOLATE_is_quiet) call IO_and_Messaging_switch("+log")
 !
 contains
   !
   function SWITCH_the_grid(what)
     !
     character(*) :: what
     logical      :: SWITCH_the_grid
     !
     if (what=="COARSE") then
       SWITCH_the_grid=OUT_k%nibz>0
       OUT_k_nk=OUT_k%nibz
       INTERP_FineGd="NONE"
       return
     endif
     !
     if (what=="FINE") then
       INTERP_FineGd=INTERP_FineGd_save
       if (trim(INTERP_FineGd)=="NONE") Nk_FineGd=0
       if (trim(INTERP_FineGd)=="IBZ")  Nk_FineGd=OUT_k%FGibz%N
       if (trim(INTERP_FineGd)=="BZ")   Nk_FineGd=OUT_k%FGbz%N
       if (trim(INTERP_FineGd)=="BARE") Nk_FineGd=OUT_k%FGbare%N
       SWITCH_the_grid=Nk_FineGd>0
       OUT_k_nk=Nk_FineGd
     endif
     !
   end function
   !
end subroutine INTERPOLATION_driver_do
