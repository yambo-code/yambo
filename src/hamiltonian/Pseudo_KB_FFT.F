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
subroutine Pseudo_KB_FFT(kbv,kbv_real_space,kpt)
 !
 use drivers,       ONLY:l_real_time,l_sc_magnetic
 use pars,          ONLY:SP,DP,cI,cONE
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:wf_ng
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use pseudo,        ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos,x_cc,a
 use com,           ONLY:warning
 !
#include<memory.h>
 !
 complex(SP)             :: kbv(wf_ng,pp_kbv_dim,4,n_spinor),kbv_real_space(fft_size,n_spinor,pp_kbv_dim)
 real(SP)                :: kpt(3)
 !
 ! Work Space
 !
 complex(DP)             :: kbv_DP(fft_size)
 real(SP)                :: k_dot_x(fft_size)
 complex(SP)             :: kpt_factor(fft_size)
 !
 ! Dummies
 !
 integer                 :: i1,is,ia,i_atom,il,im,i_pp,pp_spin,i_spinor
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
 i1=0
 i_atom=0
 !
#if defined _RT
 if(l_real_time) then
   YAMBO_ALLOC(x_cc,(fft_size,3,n_atoms))
 endif
#endif
 !
#if defined _MAGNETIC
 if(l_sc_magnetic)  kpt_factor=cONE
#endif
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     !
     i_atom=i_atom+1
     !
#if defined _RT
     if(l_real_time) then
       call load_cc(.false.,atom_pos(:,ia,is),i_atom)
       k_dot_x(:)=(kpt(1)*x_cc(:,1,i_atom)+kpt(2)*x_cc(:,2,i_atom)+kpt(3)*x_cc(:,3,i_atom))
       kpt_factor(:)  =cmplx( cos(k_dot_x(:)),sin(k_dot_x(:)) )
     endif
#endif
     !
     do i_pp = 1, pp_n_l_times_proj_max
       il = pp_table(1,is,i_pp)
       pp_spin = pp_table(3,is,i_pp)   ! neglect the so-term for abinit KB
       if(pp_spin>1) cycle
       !
       do im = 1,2*(il-1)+1
         !
         do i_spinor=1,n_spinor
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
           kbv_real_space(:,i_spinor,i1)=conjg(cmplx(kbv_DP(:),kind=SP))*kpt_factor(:)
           !
         enddo   ! i_spinor
       enddo   ! im
     enddo   ! i_pp
     !
   enddo   ! ia
 enddo  ! is
 !
end subroutine Pseudo_KB_FFT
