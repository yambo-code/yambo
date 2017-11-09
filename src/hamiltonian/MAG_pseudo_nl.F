!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine MAG_pseudo_nl(kbv)
 !
 use pars,          ONLY:SP,DP
 use units,         ONLY:SPEED_OF_LIGHT
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:wf_ng
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use pseudo,        ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos,x_cc
 use com,           ONLY:warning
 use magnetic,      ONLY:Bz,new_pp,old_pp,phase_trick,MAG_radius,n_loops
 !
#include<memory.h>
 !
 ! Work Space
 !
 complex(SP)             :: kbv(wf_ng,pp_kbv_dim,4,n_spinor)
 complex(DP)             :: kbv_DP(fft_size)
 complex(SP)             :: gauge_factor(fft_size)
 real(SP)                :: int_A_dl(fft_size),r_atom_pos(fft_size),r_x(fft_size)
 real(SP)                :: cos_delta_phi(fft_size),sin_delta_phi(fft_size)
 real(SP)                :: Phi_over_Phio
 !
 ! Dummies
 integer                 :: i1,is,ia,il,im,i_pp,pp_spin,i_spinor
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
 call load_cc(.true.,(/0._SP,0._SP,0._SP/))
 !
 i1=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     !
     if(MAG_radius==0) then
       ! int_A_dl (R-->r') = 0.5 B_0/c (x_R y' - y_R x') (landau)
       int_A_dl(:)= 0.5*Bz/SPEED_OF_LIGHT*(x_cc(:,2)*atom_pos(1,ia,is)-atom_pos(2,ia,is)*x_cc(:,1))
       gauge_factor(:)=cmplx( cos(int_A_dl(:)),sin(int_A_dl(:)) )
     else
       gauge_factor(:)=1.
     endif
     if(MAG_radius>0. .or. (phase_trick.and.n_loops>0) ) then
       r_atom_pos(:)=sqrt(atom_pos(1,ia,is)**2+atom_pos(2,ia,is)**2)
       r_x(:)=sqrt(x_cc(:,1)**2+x_cc(:,2)**2)
       ! cos(\delta_phi)= (R,r') / (||a|| ||b||)
       cos_delta_phi(:)=(atom_pos(1,ia,is)*x_cc(:,1)+atom_pos(2,ia,is)*x_cc(:,2))/(r_atom_pos(:)*r_x(:))
       ! sin(\delta_phi)= R x r' / (||a|| ||b||)
       sin_delta_phi(:)=(x_cc(:,2)*atom_pos(1,ia,is)-atom_pos(2,ia,is)*x_cc(:,1))/(r_atom_pos(:)*r_x(:))
       ! e^(i/c*int_A_dl) (R-->r') = e^i( \Phi/\Phi_0 (\phi'-\phi_R) (AB)
       if(MAG_radius>0.) Phi_over_Phio=0.5*Bz*MAG_radius**2/SPEED_OF_LIGHT
       if(phase_trick.and.n_loops>0.and.MAG_radius>0.) Phi_over_Phio=Phi_over_Phio-n_loops
       if(phase_trick.and.n_loops>0.and.MAG_radius==0.) Phi_over_Phio=-n_loops
       gauge_factor(:)=gauge_factor(:)*cmplx(  cos_delta_phi(:) , sin_delta_phi(:)   )**Phi_over_Phio
     endif
     !
     do i_spinor=1,n_spinor
       ! 
       do i_pp = 1, pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         pp_spin = pp_table(3,is,i_pp)   ! neglect the so-term for abinit KB
         if(pp_spin>1) cycle
         !
         do im = 1,2*(il-1)+1
           !
           i1=i1+2-i_spinor
           !
           kbv_DP=(0._DP,0._DP)
           kbv_DP(fft_g_table(1:wf_ng,1))=conjg(kbv(1:wf_ng,i1,1,i_spinor))*fft_norm
           !
           ! Here I need to fourier tranform kbv(G,i1,1)
#if defined _FFTW
           fftw_plan = 0
           call fft_3d(kbv_DP,fft_dim,1,fftw_plan)
#else
           call fft_3d(kbv_DP,fft_dim,1)
#endif
           !
           old_pp(:,i1,i_spinor)=conjg(kbv_DP(:))
           new_pp(:,i1,i_spinor)=conjg(kbv_DP(:))*gauge_factor(:)
           !       
         enddo   ! im
       enddo   ! i_pp
     enddo   ! i_spinor
   enddo   ! ia
 enddo  ! is
 !
 YAMBO_FREE(x_cc)
 !
end subroutine
