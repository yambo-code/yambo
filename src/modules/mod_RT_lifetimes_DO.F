!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM 
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
module RT_lifetimes_DO
 !
 use pars,           ONLY:SP,rZERO
 use RT_lifetimes,   ONLY:RT_lifetime,RT_EP_emit_life,RT_EE_life,RT_EH_life,&
&                         RT_EP_REF_life,RT_EE_REF_life,RT_EH_REF_life
 !
#include<memory.h>
 !
 contains
   !
   real(SP) function ABS_RT_life_REF(ib,ik)
     integer                       :: ib,ik
     ABS_RT_life_REF=rZERO
     if (RT_EP_emit_life%active) then
       ABS_RT_life_REF=RT_EP_REF_life%abs(ib,ik)
       if (RT_EP_emit_life%STIMULATED_components) then
         ABS_RT_life_REF=ABS_RT_life_REF+RT_EP_REF_life%STIM_emit(ib,ik)
       endif
     endif
     if (RT_EE_life%active) then
       ABS_RT_life_REF=ABS_RT_life_REF+RT_EE_REF_life%abs(ib,ik)
     endif
     if (RT_EH_life%active) then
       ABS_RT_life_REF=ABS_RT_life_REF+RT_EH_REF_life%abs(ib,ik)
     endif
   end function
   !
   real(SP) function EMIT_RT_life_REF(ib,ik)
     integer                       :: ib,ik
     EMIT_RT_life_REF=rZERO
     if (RT_EP_emit_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EP_REF_life%emit(ib,ik)
       if (RT_EP_emit_life%STIMULATED_components) then
         EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EP_REF_life%STIM_abs(ib,ik)
       endif
     endif
     if (RT_EE_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EE_REF_life%emit(ib,ik)
     endif
     if (RT_EH_life%active) then
       EMIT_RT_life_REF=EMIT_RT_life_REF+RT_EH_REF_life%emit(ib,ik)
     endif
   end function
   !
   real(SP) function ABS_RT_life_dGAMMA(ib,ik)
     integer                       :: ib,ik
     ABS_RT_life_dGAMMA=rZERO
     if (RT_EP_emit_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EP_emit_life%abs(ib,ik)
       if (RT_EP_emit_life%STIMULATED_components) then
         ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EP_emit_life%STIM_emit(ib,ik)
       endif
     endif
     if (RT_EE_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EE_life%abs(ib,ik)
     endif
     if (RT_EH_life%active) then
       ABS_RT_life_dGAMMA=ABS_RT_life_dGAMMA+RT_EH_life%abs(ib,ik)
     endif
   end function
   !
   real(SP) function EMIT_RT_life_dGAMMA(ib,ik)
     integer                       :: ib,ik
     EMIT_RT_life_dGAMMA=rZERO
     if (RT_EP_emit_life%active) then
       EMIT_RT_life_dGAMMA=RT_EP_emit_life%emit(ib,ik)
       if (RT_EP_emit_life%STIMULATED_components) then
         EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EP_emit_life%STIM_abs(ib,ik)
       endif
     endif
     if (RT_EE_life%active) then
       EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EE_life%emit(ib,ik)
     endif
     if (RT_EH_life%active) then
       EMIT_RT_life_dGAMMA=EMIT_RT_life_dGAMMA+RT_EH_life%emit(ib,ik)
     endif
   end function
   !
   real(SP) function ABS_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
     type(RT_lifetime), intent(in) :: LIFE,LIFE_ref
     integer                       :: ib,ik
     ABS_RT_life_one_KIND=rZERO
     if (.not.LIFE%active) return
     ABS_RT_life_one_KIND=LIFE%abs(ib,ik)+LIFE_ref%abs(ib,ik)
     if (.not.LIFE%STIMULATED_components) return
     ABS_RT_life_one_KIND=ABS_RT_life_one_KIND+LIFE%STIM_emit(ib,ik)+LIFE_ref%STIM_emit(ib,ik)
   end function
   !
   real(SP) function EMIT_RT_life_one_KIND(LIFE,LIFE_ref,ib,ik)
     type(RT_lifetime), intent(in) :: LIFE,LIFE_ref
     integer                       :: ib,ik
     EMIT_RT_life_one_KIND=rZERO
     if (.not.LIFE%active) return
     EMIT_RT_life_one_KIND=LIFE%emit(ib,ik)+LIFE_ref%emit(ib,ik)
     if (.not.LIFE%STIMULATED_components) return
     EMIT_RT_life_one_KIND=EMIT_RT_life_one_KIND+LIFE%STIM_abs(ib,ik)+LIFE_ref%STIM_abs(ib,ik)
   end function
   !
end module
