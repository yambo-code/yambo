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
subroutine INTERPOLATION_driver_do(operation,ID,OUT_k,OUT_E)
 !
 ! BOLTZ structure:
 !
 ! INTERPOLATION_BZ
 !  |
 !  .-INTERPOLATION_fourier
 !
 !
 use R_lattice,            ONLY:bz_samp
 use electrons,            ONLY:levels
 use interpolate_tools,    ONLY:INTERPOLATION_nearest_k_report
 use interpolate,          ONLY:INTERP_obj,INTERPOLATE_is_quiet,&
&                               INTERPOLATION_BZ,INTERPOLATION_nearest_k,&
&                               l_INTERP_f,l_INTERP_E,l_INTERP_dE,&
&                               l_integral_respect
 use IO_m,                 ONLY:IO_and_Messaging_switch
 !
#include<memory.h>
 !
 character(*),  intent(in)              :: operation
 integer,       intent(in)              :: ID
 type(bz_samp), intent(inout)           :: OUT_k
 type(levels),  intent(inout), optional :: OUT_E
 !
 ! Work Space
 !
 integer               :: n(2),nk
 !
 if (operation/=trim(INTERP_obj(ID)%what)) call error(' Interopolation ID did not match operation name')
 !
 ! ... Allocation
 !
 n =INTERP_obj(ID)%nb
 nk=INTERP_obj(ID)%nk
 !
 YAMBO_ALLOC(INTERP_obj(ID)%INTERP,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
 !
 ! ... Interpolation...
 !
 if (INTERP_obj(ID)%use_NN) then
   ! ... Using the Nearest point method
   call INTERPOLATION_nearest_k(INTERP_obj(ID)%k,INTERP_obj(ID)%SEED,OUT_k%pt,INTERP_obj(ID)%INTERP)
   if (.not.INTERPOLATE_is_quiet) call INTERPOLATION_nearest_k_report(ID)
   !
 else
   !
   ! ... Using BOLTZ method
   call INTERPOLATION_BZ(K=OUT_k,R2D=INTERP_obj(ID)%INTERP,ID=INTERP_obj(ID)%ID)
   !
   ! f/dF normalization
   !====================
   if (l_INTERP_f.and.l_integral_respect) call INTERPOLATION_integral_respect(ID,OUT_k,INTERP_obj(ID)%INTERP)
   !
 endif
 !
 ! store f/E/dE in OUT_E
 !=======================
 if (present(OUT_E)) then
   if (l_INTERP_E.or.l_INTERP_dE) then
     YAMBO_ALLOC(OUT_E%E,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
   else if (l_INTERP_f) then
     YAMBO_ALLOC(OUT_E%f,(n(1):n(2),OUT_k%nibz,INTERP_obj(ID)%D))
   endif
   OUT_E%nk=OUT_k%nibz
   OUT_E%nb=INTERP_obj(ID)%nb(2)
   if (l_INTERP_E.or.l_INTERP_dE) OUT_E%E=INTERP_obj(ID)%INTERP
   if (l_INTERP_f)                OUT_E%f=INTERP_obj(ID)%INTERP
 endif
 !
 if (INTERPOLATE_is_quiet) call IO_and_Messaging_switch("+log")
 !
end subroutine INTERPOLATION_driver_do
