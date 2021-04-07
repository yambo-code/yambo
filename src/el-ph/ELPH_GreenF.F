!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine ELPH_GreenF(i_qp,iq_bz,iq_mem,en,Sc_W)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels
 use frequency,     ONLY:w_samp
 use ELPH,          ONLY:gsqF_fan,gsqF_dw ,gsqF_ca_corr ,gsqF_life_bose ,gsqF_life_f ,&
&                        gsqF_energy_steps,elph_branches,gsqF_dw,eval_G_using_KK
 !
#include<memory.h>
 !
 integer,      intent(in) :: i_qp,iq_bz,iq_mem
 type(levels), intent(in) :: en
 type(w_samp), intent(in) :: Sc_W
 !
 ! Work Space
 !
 integer :: iw,il
 !
 do il=elph_branches(1),elph_branches(2)
   !
   ! Frequency Loop
   !
   do iw=1,gsqF_energy_steps
      !
      ! Define gsqF denominators
      !
      if (eval_G_using_KK) cycle
      !
   enddo
   !
   ! The DW term comes from a perturbation theory expansion
   ! which does not allow zero-energy transitions between the
   ! perturbed stated and itself
   !
 enddo
 !
end subroutine
