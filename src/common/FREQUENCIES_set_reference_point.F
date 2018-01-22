!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine FREQUENCIES_set_reference_point(omega,n_freq)
 !
 ! Look for the W(iw_ref) closest 0
 ! This is needed by the velocity gauge to impose the diamagnetic sum-rule
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:iw_ref,global_gauge,skip_cond_sum_rule
 use parser_m,      ONLY:parser
 use com,           ONLY:warning
 !
 implicit none
 !
 integer,     intent(in)        :: n_freq
 complex(SP), intent(inout)     :: omega(n_freq)
 !
 ! Work space
 !
 integer :: iw
 !
 iw_ref=1
 !
 if(trim(global_gauge)=="velocity") call parser('NoCondSumRule',skip_cond_sum_rule)
 !
 if(all(abs(real(omega(:)))>0.01) .and. trim(global_gauge)=="velocity".and..not.skip_cond_sum_rule) then
   call warning(' No w close to 0 in the frequency grid. Length gauge imposed')
   skip_cond_sum_rule=.false.
   global_gauge='length'
   return
 endif
 !
 do iw=1,n_freq
   if(abs(real(omega(iw)))>0.01) cycle
   if(abs(omega(iw))<abs(omega(iw_ref))) iw_ref=iw
 enddo
 !
 if(trim(global_gauge)=="velocity" .and. abs(real(omega(iw_ref)))==0._SP) omega(iw_ref)=cmplx(1.E-10,aimag(omega(iw_ref)))
 !
end subroutine FREQUENCIES_set_reference_point
