!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine Pseudo_KB_gauge_factor(gauge_factor)
 !
 use pars,          ONLY:SP
 use units,         ONLY:SPEED_OF_LIGHT
 use electrons,     ONLY:n_spinor
 use FFT_m,         ONLY:fft_size
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos,x_cc,a
 use com,           ONLY:warning
#if defined _RT
 use drivers,       ONLY:l_real_time
 use fields,        ONLY:A_tot
#endif
 !
#include<memory.h>
 !
 complex(SP)             :: gauge_factor(fft_size,n_atoms)
 !
 ! Work Space
 !
 real(SP)                :: int_A_dl(fft_size),r_atom_pos(fft_size),r_x(fft_size)
 real(SP)                :: cos_delta_phi(fft_size),sin_delta_phi(fft_size)
 real(SP)                :: Phi_over_Phio
 !
 ! Dummies
 integer                 :: is,ia,i_atom
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
 !
 i_atom=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     !
     i_atom=i_atom+1
     !
     !
#if defined _RT
     if(l_real_time) then
       ! int_A_dl (atom_pos-->r) = A*r
       int_A_dl(:)= (A_tot%vecpot(1)*x_cc(:,1,i_atom)+A_tot%vecpot(2)*x_cc(:,2,i_atom)+A_tot%vecpot(3)*x_cc(:,3,i_atom))
       int_A_dl   = int_A_dl/SPEED_OF_LIGHT
       gauge_factor(:,i_atom)=cmplx( cos(int_A_dl(:)),sin(int_A_dl(:)) )
     endif
#endif
     !
   enddo   ! ia
 enddo  ! is
 !
 !
end subroutine Pseudo_KB_gauge_factor
