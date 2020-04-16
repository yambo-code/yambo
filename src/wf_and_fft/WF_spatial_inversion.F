!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine WF_spatial_inversion(en,Xk)
 !
 ! Check if the Inversion is a DL symmetry operation either
 ! using the wavefunctions or the atomic positions.
 !
 !  i_space_inv=0 (it is not)
 !  i_space_inv=1 (it is)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:levels,n_spinor
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:WF
 use com,           ONLY:msg
 use D_lattice,     ONLY:i_time_rev,i_space_inv,mag_syms
 use R_lattice,     ONLY:bz_samp
 use interfaces,    ONLY:el_density_and_current
 !
 implicit none
 !
 type(levels) ::en
 type(bz_samp)::Xk
 !
 ! Work Space
 !
 integer                 ::i1,i_spinor
 real(SP),   allocatable ::rho_si(:),rho_nsi(:)
 complex(SP),allocatable ::cv(:)
 complex(SP)             ::mv(2)
 !
 ! Already set
 !
 if (i_space_inv>=0) return
 !
 ! WF procedure
 !
 if (.not.allocated(WF%c).or.WF%space/='R') return
 !
 ! When using wf's the space inv is tested using the first
 ! nsym/(i_time_rev+1) and all the nsym. If there is not
 ! TR this procedure is not possible.
 !
 ! When mag_syms, t_rev and space_inv are two different syms
 ! and this procedure is meaningless.
 if (i_time_rev==0.or.mag_syms) then 
   i_space_inv=0
 else
   !
   allocate(rho_si(fft_size),rho_nsi(fft_size),cv(fft_size))
   call el_density_and_current(en,Xk,rho=rho_si,force_spatial_inversion=.true.)
   call el_density_and_current(en,Xk,rho=rho_nsi)
   !
   cv=cZERO
   mv(1:2)=0.0_SP
   !
#if !defined _OPENMP
   !
   forall(i1=1:fft_size) cv(i1)=conjg(WF%c(i1,i_spinor,1))*WF%c(i1,i_spinor,1)
   mv(1)=sum(rho_nsi(:)*cv(:))
   mv(2)=sum(rho_si(:)*cv(:))
   !
#else
   !
   !$omp parallel do default(shared), private(i1,i_spinor), reduction(+:mv)
   do i_spinor=1,n_spinor
     do i1=1,fft_size
       cv(i1)=conjg(WF%c(i1,i_spinor,1))*WF%c(i1,i_spinor,1)
       mv(1)=mv(1) +rho_nsi(i1)*cv(i1)
       mv(2)=mv(2) +rho_si(i1) *cv(i1)
     enddo
   enddo
   !$omp end parallel do
   !
#endif
   !
   if (abs(mv(1)-mv(2))<abs(mv(1))/1.E5) then
     i_space_inv=1
   else
     i_space_inv=0
   endif
   !
   deallocate(rho_nsi,rho_si)
 endif
 !
 if (i_space_inv==1) call msg('r','[SYMs] Inversion symmetry is used')
 if (i_space_inv==0) call msg('r','[SYMs] Inversion symmetry is NOT used')
 !
end subroutine
