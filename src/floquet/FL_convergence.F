!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_check_convergence(FL_pol,l_sc_reached,diff_mod)
 !
 use pars,           ONLY:SP,DP
 use fl_optics,      ONLY:FL_threshold,n_tot_fl_modes,max_fl_mode,FL_Chi_order
 !
 complex(SP), intent(inout) :: FL_pol(3,FL_Chi_order+1,2) !second dimension for previous iteration
 logical,intent(out) :: l_sc_reached
 real(DP),intent(out)    :: diff_mod
 !
 ! Work Space
 integer :: id,FL_Pi_order
 real :: diff_real,diff_complex
 !
 FL_Pi_order = FL_Chi_order + 1
 !
 !aim137 prelim
 do id=1,2!aim137 change to 3
   !
   !aim137 add condition - if direction non periodic --> cycle
   !
   if ( FL_Pi_order == 2 .and. id > 1 ) cycle
   !
   diff_real    =  abs(( real(FL_pol(id,FL_Pi_order,1)) &
                     & - real(FL_pol(id,FL_Pi_order,2))) &
                     & / real(FL_pol(id,FL_Pi_order,1)))
   diff_complex =  abs((aimag(FL_pol(id,FL_Pi_order,1)) &
                     & -aimag(FL_pol(id,FL_Pi_order,2))) &
                     & /aimag(FL_pol(id,FL_Pi_order,1)))
   !
   !
   diff_mod=diff_complex
  !diff_mod =  abs((FL_pol(id,FL_Pi_order,1)*conjg(FL_pol(id,FL_Pi_order,1)) &
  !             & -(FL_pol(id,FL_Pi_order,2)*conjg(FL_pol(id,FL_Pi_order,2)))) &
  !              & /(FL_pol(id,FL_Pi_order,1)*conjg(FL_pol(id,FL_Pi_order,1))))
   !
   !print*, id,'--------------------'
   !print*, FL_pol(id,FL_Pi_order,1)
   !print*, FL_pol(id,FL_Pi_order,2)
   !print*, 'Relative error in mod(Chi) = ',diff_mod
   !
   !aim137 prelim - choose what condition to use
   if (diff_real < FL_threshold .and. diff_complex < FL_threshold) then
   !if (diff_mod < FL_threshold) then
     l_sc_reached = .true.
   else
     l_sc_reached = .false.
     exit !if one direction is not converged, then FL_pol is not converged
   endif
   !
 enddo
!
end subroutine FL_check_convergence
