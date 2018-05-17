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
subroutine check_periodic_directions()
 !
 use pars,           ONLY:SP
 use com,            ONLY:msg,warning
 use X_m,            ONLY:eps_2_alpha,alpha_dim,eval_alpha
 use D_lattice,      ONLY:DL_vol,a,non_periodic_directions
 use vec_operate,    ONLY:v_norm
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: non_p_dir(3),i1,ia,i_check
 real(SP) :: non_p_dot_product(3),periodic_length,non_periodic_length
 logical  :: l_0D,l_1D,l_2D,l_3D
 !
 non_p_dir=0
 if (index(non_periodic_directions,'x')+index(non_periodic_directions,'X')/=0) non_p_dir(1)=1
 if (index(non_periodic_directions,'y')+index(non_periodic_directions,'Y')/=0) non_p_dir(2)=1
 if (index(non_periodic_directions,'z')+index(non_periodic_directions,'Z')/=0) non_p_dir(3)=1
 !
 l_0D=all(non_p_dir/=0)
 l_1D=sum(non_p_dir)==2
 l_2D=sum(non_p_dir)==1
 l_3D=all(non_p_dir==0)
 !
 if (l_3D) return
 !
 ! 0D case
 !
 if (l_0D) then 
   alpha_dim='volume'
   eps_2_alpha=DL_vol
   eval_alpha=.TRUE.
 endif
 !
 ! 2D case. Note that to be a correct geometric configuration
 ! there must be one cell unit vector _|_ to the 2D non periodic plane that must contain
 ! both the other two cell vectors
 !
 ! 1D case. Note that to be a correct geometric configuration
 ! there must be two cell unit vectors _|_ to the 1D non periodic direction that must be
 ! parallel to the third unit cell vector
 !
 if (l_1D.or.l_2D) then 
   do ia=1,3
     non_p_dot_product(ia)=0.
     do i1=1,3
       if (non_p_dir(i1)==1) non_p_dot_product(ia)=non_p_dot_product(ia)+abs(a(ia,i1))
     enddo
   enddo
   i_check=0
   do ia=1,3
     if ( abs(non_p_dot_product(ia)-sum(abs(a(ia,:)))) < 1.E-5 ) then
       i_check=i_check+1
       if (l_2D) non_periodic_length=v_norm(a(ia,:))
     else if (non_p_dot_product(ia)<1.E-5) then
       i_check=i_check+1
       if (l_1D) periodic_length=v_norm(a(ia,:))
     endif
     if (i_check==3) then
       if (l_1D) then
         eps_2_alpha=DL_vol/periodic_length
         alpha_dim='area'
       endif
       if (l_2D) then
         eps_2_alpha=non_periodic_length
         alpha_dim='length'
       endif
       eval_alpha=.TRUE.
     endif
   enddo
 endif
 !
 if (.not.eval_alpha) then
   call warning('Non periodic geometry and unit cell are incompatible')
   return
 endif
 !
 call msg('r', 'Non-periodic chartesian directions :',trim(non_periodic_directions))
 call msg('r', 'Optical renormalization        [au]:',eps_2_alpha)
 call msg('rn','Polarizability dimension           :',trim(alpha_dim))
 !
end subroutine
