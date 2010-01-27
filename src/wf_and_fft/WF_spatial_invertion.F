!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine WF_spatial_invertion(en,Xk)
 !
 ! Check if the Inversion is a DL symmetry operation either
 ! using the wavefunctions or the atomic positions.
 !
 !  i_space_inv=0 (it is not)
 !  i_space_inv=1 (it is)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels
 use FFT_m,         ONLY:fft_size
 use wave_func,     ONLY:wf,wf_space
 use com,           ONLY:msg
 use D_lattice,     ONLY:i_time_rev,i_space_inv
 use R_lattice,     ONLY:bz_samp
 implicit none
 !
 type(levels) ::en
 type(bz_samp)::Xk
 !
 ! Work Space
 !
 integer                 ::i1
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
 if (.not.allocated(wf).or.wf_space/='R') return
 !
 ! When using wf's the space inv is tested using the first
 ! nsym/(i_time_rev+1) and all the nsym. If there is not
 ! TR this procedure is not possible.
 !
 if (i_time_rev==0) then 
   i_space_inv=0
 else
   !
   allocate(rho_si(fft_size),rho_nsi(fft_size),cv(fft_size))
   call el_density(en,Xk,rho_si,.true.)
   call el_density(en,Xk,rho_nsi,.false.)
   !
   cv=(0.,0.)
   forall(i1=1:fft_size) cv(i1)=conjg(wf(i1,1))*wf(i1,1)
   mv(1)=sum(rho_nsi(:)*cv(:))
   mv(2)=sum(rho_si(:)*cv(:))
   if (abs(mv(1)-mv(2))<abs(mv(1))/1.E5) then
     i_space_inv=1
   else
     i_space_inv=0
   endif
   !
   deallocate(rho_nsi,rho_si,cv)
 endif
 !
 if (i_space_inv==1) call msg('r','[SYMs] Inversion symmetry is used')
 if (i_space_inv==0) call msg('r','[SYMs] Inversion symmetry is NOT used')
 !
end subroutine
