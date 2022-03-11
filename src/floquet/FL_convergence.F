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
subroutine FL_check_convergence(FL_pol,l_sc_reached,accuracy)
 !
 use pars,           ONLY:SP,DP
 use fl_optics,      ONLY:FL_threshold,n_tot_fl_modes,max_fl_mode,FL_Chi_order
 !
 complex(SP), intent(inout) :: FL_pol(3,FL_Chi_order+1,2) !second dimension for previous iteration
 logical,intent(out) :: l_sc_reached
 real(DP),intent(out)    :: accuracy
 !
 ! Work Space
 integer  :: id,FL_Pi_order
 logical  :: l_1order_sc_reached,l_2order_sc_reached
 real(SP) :: diff_real,diff_imag,diff_complex
 !
 FL_Pi_order = FL_Chi_order + 1
 !
 !aim137 prelim
 !
 ! First order convergence
 ! =======================
 ! only id = 1 in first order
 !
 diff_real  =  abs(  real(FL_pol(1,2,1)) &
                 & - real(FL_pol(1,2,2)))
 diff_imag  =  abs( aimag(FL_pol(1,2,1)) &
                 & -aimag(FL_pol(1,2,2)))

 !print*, 'diff_real =',diff_real ,'diff_imag =',diff_imag 
 if (diff_real < FL_threshold .and. diff_imag < FL_threshold) then
   l_1order_sc_reached = .true.
 else
   l_1order_sc_reached = .false.
 endif
 !
 !
 ! Second order convergence
 ! =======================
 !
 if (l_1order_sc_reached) then
   !
   if (FL_Chi_order > 1) then
     !
     do id=1,2!aim137 change to 3
       !
       !aim137 add condition - if direction non periodic --> cycle
       !
       diff_real  =  abs(  real(FL_pol(id,3,1)) &
                       & - real(FL_pol(id,3,2)))
       diff_imag  =  abs( aimag(FL_pol(id,3,1)) &
                       & -aimag(FL_pol(id,3,2)))
       !
       !aim137 prelim - improve threshold? link to input variable?
       if (diff_real < 1.E-6_SP .and. diff_imag < 1.E-6_SP) then
         l_2order_sc_reached = .true.
       else
         l_2order_sc_reached = .false.
         exit !if one direction is not converged, then FL_pol is not converged
       endif
       !
     enddo
     !
   else
     !
     l_2order_sc_reached = .true. ! 2nd order is not requested
     !
   endif
   !
 else
   !
   l_2order_sc_reached = .false. ! skip 2nd order if 1st not converged
   !
 endif
 !
 !
 ! Gather convergence
 ! ==================
 !
 l_sc_reached = (l_1order_sc_reached .and. l_2order_sc_reached)
 accuracy = diff_real
 !
end subroutine FL_check_convergence
