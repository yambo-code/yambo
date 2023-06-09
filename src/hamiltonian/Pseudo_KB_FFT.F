!
!        Copyright (C) 2000-2023 the YAMBO team
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
subroutine Pseudo_KB_FFT(kbv,kbv_real_space,kpt,pp_range,npp,i_atom,is,ia,ik)
 !
 use drivers,       ONLY:l_real_time,l_sc_magnetic
 use pars,          ONLY:SP,DP,cONE
 use electrons,     ONLY:n_spinor
 use wave_func,     ONLY:wf_ng,wf_ncx,wf_igk,wf_nc_k
 use FFT_m,         ONLY:fft_size,fft_dim,fft_g_table,fft_norm,fftw_plan
 use pseudo,        ONLY:pp_kbv_dim_yambo,pp_n_l_times_proj_max,pp_table,pp_kbv_table
 use D_lattice,     ONLY:atom_pos,x_cc,a,n_atoms
 !
#include<memory.h>
 !
 integer,     intent(in)  :: npp,pp_range(2),i_atom,is,ia,ik
 complex(SP), intent(in)  :: kbv(wf_ncx,n_spinor,4,npp)
 complex(SP), intent(out) :: kbv_real_space(fft_size,n_spinor,pp_range(1):pp_range(2))
 real(SP),    intent(in)  :: kpt(3)
 !
 ! Work Space
 !
 complex(DP)             :: kbv_DP(fft_size)
 real(SP)                :: k_dot_x(fft_size)
 complex(SP)             :: kpt_factor(fft_size)
 !
 ! Dummies
 !
 integer                 :: ig,ic,i_pp,i_kbv,i_spinor
 !
 if (n_spinor>1) call warning('new kb implementation to be tested for the case n_spinor>1')
 !
#if defined _RT
 if(l_real_time) then
   YAMBO_ALLOC(x_cc,(fft_size,3,n_atoms))
 endif
#endif
 !
#if defined _SC
 if(l_sc_magnetic)  kpt_factor=cONE
#endif
 !
     !
#if defined _RT
     if(l_real_time) then
       call error("call to load cc to be fixed")
       call load_cc(.false.,atom_pos(:,ia,is),i_atom)
       k_dot_x(:)=(kpt(1)*x_cc(:,1,i_atom)+kpt(2)*x_cc(:,2,i_atom)+kpt(3)*x_cc(:,3,i_atom))
       kpt_factor(:)  =cmplx( cos(k_dot_x(:)),sin(k_dot_x(:)) )
     endif
#endif
     !
     do i_pp = pp_range(1),pp_range(2)
       i_kbv=i_pp-pp_range(1)+1
       do i_spinor=1,n_spinor
           !
           kbv_DP=(0._DP,0._DP)
           do ic=1,wf_nc_k(ik)
             ig=wf_igk(ic,ik)
             kbv_DP(fft_g_table(ig,1))=conjg(kbv(ic,i_spinor,1,i_kbv))*fft_norm
           enddo
           !
           ! Here I need to fourier tranform kbv(G,i1,1)
#if defined _FFTW
           fftw_plan = 0
           call fft_3d(kbv_DP,fft_dim,1,fftw_plan)
#else
           call fft_3d(kbv_DP,fft_dim,1)
#endif
           !
           kbv_real_space(:,i_spinor,i_pp)=conjg(cmplx(kbv_DP(:),kind=SP))*kpt_factor(:)
           !
       enddo ! i_spinor
     enddo   ! i_pp
     !
 !
end subroutine Pseudo_KB_FFT
