!
! Copyright (C) 2000-2016 D. Sangalli and the YAMBO team 
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
subroutine MAG_H_pseudo(Xk,Xen)
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:X_t
 use electrons,     ONLY:n_spin,n_spinor,n_sp_pol,levels
 use wrapper,       ONLY:V_dot_V
 use wave_func,     ONLY:wf,WF_load,WF_free,wf_state,wf_n_states,wf_ng
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim
 use R_lattice,     ONLY:bz_samp
 use timing,        ONLY:live_timing
 use SC,            ONLY:SC_bands,Ho
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms,atom_pos
 use memory_m,      ONLY:mem_est
 use magnetic,      ONLY:new_pp,old_pp
 use IO_m,          ONLY:OP_RD_CL,OP_RD,RD,RD_CL,io_control,REP,VERIFY
 !
 implicit none
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(X_t)     :: X
 !
 complex(SP),allocatable :: kbv(:,:,:),kbv_ks_space(:,:)
 complex(SP)             :: H_pseudo(SC_bands(2),SC_bands(2))
 complex(SP)             :: H_pseudo_tot(SC_bands(2),SC_bands(2),Xk%nibz,n_spin)
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spin,iwf,iwfp,ifft,is,ia,il,im,pp_spin,i_pp
 integer                 :: alloc_err
 !
 ! I / O
 integer                 :: ID,io_KB_abinit_err,io_KB_pwscf_err,io_err(3)
 integer, external       :: ioKB_abinit,ioKB_pwscf
 !
 call Dipole_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 !io_Vnl_err     = io_err(3)
 !
 if(io_KB_abinit_err/=0.and.io_KB_pwscf_err/=0) return
 !
 call fft_setup(0,1,.true.)  ! fft_size
 !
 allocate(kbv(wf_ng,pp_kbv_dim,4),stat=alloc_err)
 !
 call mem_est("KBV",(/size(kbv)/),errors=(/alloc_err/))
 !
 allocate(new_pp(fft_size,pp_kbv_dim),old_pp(fft_size,pp_kbv_dim))
 call mem_est('new_pp old_pp',(/fft_size,fft_size,fft_size,fft_size/))
 !
 allocate(kbv_ks_space(SC_bands(2),2))
 call mem_est('kbv_ks_space',(/2*SC_bands(2)/))
 !
 H_pseudo_tot=(0._SP,0._SP)
 !
 call live_timing('MAG-pseudo',Xk%nibz)        
 !
 do ik=1,Xk%nibz
   !
   call WF_load(0,1,(/1,SC_bands(2)/),(/ik,ik/),title='-Pseudo',impose_free_and_alloc=.TRUE.)
   !
   if (io_KB_pwscf_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_pwscf_err=ioKB_pwscf(ID)
   endif
   !
   if (io_KB_abinit_err==0) then
     if (ik<Xk%nibz  ) call io_control(ACTION=RD,SEC=(/ik+1/),ID=ID)
     if (ik==Xk%nibz ) call io_control(ACTION=RD_CL,SEC=(/ik+1/),ID=ID)
     io_KB_abinit_err=ioKB_abinit(ID)
   endif
   !
   do i_spin=1,n_spin
     !  
     ! Pseudo(k+G,k+G',i1) = kbv^*(k+G,i1,1) kbv(k+G',i1,1)
     ! i1 runs over atomic_species, atoms_per_specie , l_per_atom, m=-l,-l+1,...,l-1,sl
     pp_spin=i_spin
     if (io_KB_abinit_err==0.and.n_spinor==2) pp_spin=1
     if (io_KB_pwscf_err ==0.and.n_sp_pol==2) pp_spin=1
     if (io_KB_abinit_err==0) call Dipole_kb_abinit_comp(ik,pp_spin,Xk,kbv)
     if (io_KB_pwscf_err ==0) call Dipole_kb_pwscf_comp(ik,pp_spin,Xk,kbv)
     ! 
     call MAG_pseudo_nl(kbv)
     !
     do i1=1,pp_kbv_dim
       !     
       do ib=1,SC_bands(2)
         !
         iwf=wf_state(ib,ik,i_spin)
         !
         kbv_ks_space(ib,1)=V_dot_V(fft_size,wf(:,iwf),new_pp(:,i1))
         kbv_ks_space(ib,2)=V_dot_V(fft_size,wf(:,iwf),old_pp(:,i1))
         !
         do ibp=1,ib
           !
           iwfp=wf_state(ibp,ik,i_spin)
           !
           H_pseudo(ibp,ib)=conjg(kbv_ks_space(ibp,1))*kbv_ks_space(ib,1)-conjg(kbv_ks_space(ibp,2))*kbv_ks_space(ib,2)
           if(ib/=ibp) H_pseudo(ib,ibp)=conjg(H_pseudo(ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo_tot(:,:,ik,i_spin)=H_pseudo_tot(:,:,ik,i_spin)+H_pseudo(:,:)
       !
     enddo   ! i1
     !
   enddo   ! i_spin
   !
   call live_timing(steps=1)
   !
   call WF_free()
   !
 enddo  ! ik
 !
 call live_timing()
 !
 Ho(:,:,:,:)=Ho(:,:,:,:)+H_pseudo_tot(:,:,:,:)
 !
 ! Free mem
 deallocate(kbv_ks_space)
 call mem_est('kbv_ks_space')
 !
 deallocate(kbv)
 call mem_est('kbv')
 !
 deallocate(new_pp,old_pp)
 call mem_est('new_pp old_pp')
 !
end subroutine
