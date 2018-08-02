!
!        Copyright (C) 2000-2018 the YAMBO team
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
module y_interpolate
 !
 implicit none
 !
 interface
   !
   subroutine INTERPOLATION_nearest_k(K_in,K_out,V_out,SHAPE_out,&
&                                     Kw_in,QP_in,RT_in,V_in,V1_in,SHAPE_in,Kw_out,&
&                                     SAME_SUM,N_neigh,QUIET)
     use real_time,      ONLY:RT_t
     use QP_m,           ONLY:QP_t
     use pars,           ONLY:SP
     !
     real(SP)                :: K_in(:,:)
     real(SP)                :: K_out(:,:)
     real(SP)                :: V_out(:,:,:) 
     integer                 :: SHAPE_out(2)
     !
     ! Possible IN's
     real(SP)     , optional :: V_in(:,:,:)
     real(SP)     , optional :: V1_in(:)
     type(RT_t)   , optional :: RT_in
     type(QP_t)   , optional :: QP_in
     integer      , optional :: SHAPE_in(2)
     !
     ! Weights
     real(SP)     , optional :: Kw_in(:)
     real(SP)     , optional :: Kw_out(:)
     !
     logical      , optional :: SAME_SUM,QUIET
     integer      , optional :: N_neigh
     !
   end subroutine
   !
 end interface
 !
end module
