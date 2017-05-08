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
subroutine MAG_H_pseudo(Xk,Xen)
 !
 use pars,          ONLY:SP,cZERO
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use wrapper,       ONLY:V_dot_V
 use X_m,           ONLY:X_t
 use wave_func,     ONLY:WF,wf_ng
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim
 use R_lattice,     ONLY:bz_samp
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_ref_bands,Hzero
 use magnetic,      ONLY:new_pp,old_pp
 use interfaces,    ONLY:WF_load,WF_free
 use io_m,          ONLY:RD,RD_CL,io_control
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(X_t)     :: X
 !
 complex(SP),allocatable :: kbv(:,:,:,:),kbv_ks_space(:,:)
 complex(SP)             :: H_pseudo(H_ref_bands(2),H_ref_bands(2)), &
&                           H_pseudo_tot(H_ref_bands(2),H_ref_bands(2),Xk%nibz,n_sp_pol)
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spin,i_spinor,i_sp_pol,iwf(n_spinor)
 !
 ! I / O
 integer                 :: ID,io_err(3),io_KB_pwscf_err,io_KB_abinit_err,io_Vnl_err
 integer, external       :: io_KB_abinit,io_KB_pwscf
 !
 !
 call DIPOLE_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err     = io_err(3)
 !
 if(io_KB_abinit_err/=0.and.io_KB_pwscf_err/=0) return
 !
 call fft_setup(0,1,.true.)  ! fft_size
 !
 YAMBO_ALLOC(kbv,(wf_ng,pp_kbv_dim,4,n_spinor))
 YAMBO_ALLOC(new_pp,(fft_size,pp_kbv_dim,n_spinor))
 YAMBO_ALLOC(old_pp,(fft_size,pp_kbv_dim,n_spinor))
 YAMBO_ALLOC(kbv_ks_space,(H_ref_bands(1):H_ref_bands(2),2))
 !
 H_pseudo_tot=cZERO
 !
 call live_timing('MAG-pseudo',Xk%nibz)        
 !
 do ik=1,Xk%nibz
   !
   call WF_load(WF,0,1,(/1,H_ref_bands(2)/),(/ik,ik/),title='-Pseudo',impose_free_and_alloc=.TRUE.)
   !
   if (io_KB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_pwscf_err=io_KB_pwscf(ID)
   endif
   !
   if (io_KB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_abinit_err=io_KB_abinit(ID)
   endif
   !
   do i_sp_pol=1,n_sp_pol
     !  
     ! Pseudo(k+G,k+G',i1) = kbv^*(k+G,i1,1) kbv(k+G',i1,1)
     ! i1 runs over atomic_species, atoms_per_specie , l_per_atom, m=-l,-l+1,...,l-1,sl
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     ! 
     call MAG_pseudo_nl(kbv)
     !
     do i1=1,pp_kbv_dim
       !     
       kbv_ks_space=cZERO
       !
       do ib=H_ref_bands(1),H_ref_bands(2)
         !
         do i_spinor=1,n_spinor
           !
           i_spin=i_sp_pol+i_spinor-1
           iwf(i_spinor)=WF%index(ib,ik,i_spin)
           !
           kbv_ks_space(ib,1)=kbv_ks_space(ib,1)+V_dot_V(fft_size,WF%c(:,iwf(i_spinor)),new_pp(:,i1,i_spinor))
           kbv_ks_space(ib,2)=kbv_ks_space(ib,2)+V_dot_V(fft_size,WF%c(:,iwf(i_spinor)),old_pp(:,i1,i_spinor))
           !
         enddo
         !
         do ibp=H_ref_bands(1),ib
           !
           H_pseudo(ibp,ib)=conjg(kbv_ks_space(ibp,1))*kbv_ks_space(ib,1)-conjg(kbv_ks_space(ibp,2))*kbv_ks_space(ib,2)
           if(ib/=ibp) H_pseudo(ib,ibp)=conjg(H_pseudo(ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo_tot(:,:,ik,i_sp_pol)=H_pseudo_tot(:,:,ik,i_sp_pol)+H_pseudo(:,:)
       !
     enddo   ! i1
     !
   enddo   ! i_sp_pol
   !
   call live_timing(steps=1)
   !
   call WF_free(WF)
   !
 enddo  ! ik
 !
 call live_timing()
 !
 Hzero(:,:,:,:)=Hzero(:,:,:,:)+H_pseudo_tot(:,:,:,:)
 !
 ! Free mem
 YAMBO_FREE(kbv_ks_space)
 YAMBO_FREE(kbv)
 YAMBO_FREE(new_pp)
 YAMBO_FREE(old_pp)
 !
end subroutine
