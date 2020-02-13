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
#if defined _MAGNETIC
 use drivers,       ONLY:l_sc_magnetic
 use magnetic,      ONLY:Bz,phase_trick,MAG_radius,n_loops
#endif
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
#if defined _MAGNETIC
 if(l_sc_magnetic) then
   YAMBO_ALLOC(x_cc,(fft_size,3,1))
   call load_cc(.true.,(/0._SP,0._SP,0._SP/),1)
 endif
#endif
 !
 i_atom=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     !
     i_atom=i_atom+1
     !
#if defined _MAGNETIC
     if(l_sc_magnetic) then
       if(MAG_radius==0) then
         ! int_A_dl (R-->r') = 0.5 B_0/c (x_R y' - y_R x') (landau)
         int_A_dl(:)= 0.5_SP*Bz/SPEED_OF_LIGHT*(x_cc(:,2,1)*atom_pos(1,ia,is)-atom_pos(2,ia,is)*x_cc(:,1,1))
         gauge_factor(:,i_atom)=cmplx( cos(int_A_dl(:)),sin(int_A_dl(:)) )
       else
         gauge_factor(:,i_atom)=1._SP
       endif
       if(MAG_radius>0._SP .or. (phase_trick.and.n_loops>0) ) then
         r_atom_pos(:)=sqrt(atom_pos(1,ia,is)**2+atom_pos(2,ia,is)**2)
         r_x(:)=sqrt(x_cc(:,1,1)**2+x_cc(:,2,1)**2)
         ! cos(\delta_phi)= (R,r') / (||a|| ||b||)
         cos_delta_phi(:)=(atom_pos(1,ia,is)*x_cc(:,1,1)+atom_pos(2,ia,is)*x_cc(:,2,1))/(r_atom_pos(:)*r_x(:))
         ! sin(\delta_phi)= R x r' / (||a|| ||b||)
         sin_delta_phi(:)=(x_cc(:,2,1)*atom_pos(1,ia,is)-atom_pos(2,ia,is)*x_cc(:,1,1))/(r_atom_pos(:)*r_x(:))
         ! e^(i/c*int_A_dl) (R-->r') = e^i( \Phi/\Phi_0 (\phi'-\phi_R) (AB)
         if(MAG_radius>0.) Phi_over_Phio=0.5*Bz*MAG_radius**2/SPEED_OF_LIGHT
         if(phase_trick.and.n_loops>0.and.MAG_radius>0._SP) Phi_over_Phio=Phi_over_Phio-n_loops
         if(phase_trick.and.n_loops>0.and.MAG_radius==0._SP) Phi_over_Phio=-n_loops
         gauge_factor(:,i_atom)=gauge_factor(:,i_atom)*cmplx(  cos_delta_phi(:) , sin_delta_phi(:)   )**Phi_over_Phio
       endif
     endif
#endif
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
#if defined _MAGNETIC
 if(l_sc_magnetic) then
   YAMBO_FREE(x_cc)
 endif
#endif
 !
end subroutine Pseudo_KB_gauge_factor
