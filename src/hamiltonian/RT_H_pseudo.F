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
subroutine RT_H_pseudo(Xk,Xen)
 !
 use pars,          ONLY:SP,cZERO,cI
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use wrapper,       ONLY:V_dot_V
 use X_m,           ONLY:X_t
 use wave_func,     ONLY:WF,wf_ng
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim
 use R_lattice,     ONLY:bz_samp
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_ref_bands,Hzero
 use real_time,     ONLY:H_pseudo_order1,H_pseudo_order2
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
 complex(SP),allocatable :: kbv_r0(:,:,:),kbv_r1(:,:,:,:),kbv_r2(:,:,:,:,:)
 complex(SP),allocatable :: kbv(:,:,:,:),kbv_ks_space0(:,:),kbv_ks_space1(:,:,:),kbv_ks_space2(:,:,:,:)
 complex(SP)             :: H_pseudo_tmp_order1(3,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2)), &
&                           H_pseudo_tmp_order2(3,3,H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2))
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spin,i_spinor,i_sp_pol,iwf,i_dir,j_dir
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
 YAMBO_ALLOC(kbv_r0,(fft_size,pp_kbv_dim,n_spinor))
 YAMBO_ALLOC(kbv_r1,(fft_size,3,pp_kbv_dim,n_spinor))
 YAMBO_ALLOC(kbv_r2,(fft_size,3,3,pp_kbv_dim,n_spinor))
 YAMBO_ALLOC(kbv_ks_space0,(H_ref_bands(1):H_ref_bands(2),n_spinor))
 YAMBO_ALLOC(kbv_ks_space1,(3,H_ref_bands(1):H_ref_bands(2),n_spinor))
 YAMBO_ALLOC(kbv_ks_space2,(3,3,H_ref_bands(1):H_ref_bands(2),n_spinor))
 !
 H_pseudo_order1=cZERO
 H_pseudo_order2=cZERO
 !
 call live_timing('RT-pseudo',Xk%nibz)        
 !
 do ik=1,Xk%nibz
   !
   !call WF_load(WF,0,1,(/H_ref_bands(1),H_ref_bands(2)/),(/ik,ik/),title='-Pseudo',impose_free_and_alloc=.TRUE.)
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
     call RT_KB_pseudo(kbv,kbv_r0,kbv_r1,kbv_r2)
     !
     !write(*,*) kbv_r0(1:10,1,1)
     !write(*,*) kbv_r1(1:10,:,1,1)
     !write(*,*) kbv_r2(1:10,:,:,1,1)
     !
     do i1=1,pp_kbv_dim
       !     
       kbv_ks_space0=cZERO
       kbv_ks_space1=cZERO
       kbv_ks_space2=cZERO
       !
       do ib=H_ref_bands(1),H_ref_bands(2)
         !
         iwf=WF%index(ib,ik,i_sp_pol)
         !
         do i_spinor=1,n_spinor
           !
           kbv_ks_space0(ib,i_spinor)=V_dot_V(fft_size,WF%c(:,i_spinor,iwf),kbv_r0(:,i1,i_spinor))
           do i_dir=1,3
             kbv_ks_space1(i_dir,ib,i_spinor)=V_dot_V(fft_size,WF%c(:,i_spinor,iwf),kbv_r1(:,i_dir,i1,i_spinor))
             !do j_dir=1,3
             !  kbv_ks_space2(j_dir,i_dir,ib,i_spinor)= &
&            !    V_dot_V(fft_size,WF%c(:,i_spinor,iwf),kbv_r2(:,j_dir,i_dir,i1,i_spinor))
             !enddo
           enddo
           !
         enddo
         !
         !write(*,*) '0',ik,ib,kbv_ks_space0(ib)
         !write(*,*) '1',ik,ib,kbv_ks_space1(1:2,ib)
         !write(*,*) '2',ik,ib,kbv_ks_space2(1:2,1:2,ib)
         !
         do ibp=H_ref_bands(1),ib
           !
           H_pseudo_tmp_order1(:,ibp,ib)= -(conjg(kbv_ks_space1(:,ibp,1))*kbv_ks_space0(ib,1)  &
&                                            +conjg(kbv_ks_space0(ibp,1))*kbv_ks_space1(:,ib,1) )
           if(ib/=ibp) H_pseudo_tmp_order1(:,ib,ibp)=conjg(H_pseudo_tmp_order1(:,ibp,ib))
           !
!           do i_dir=1,3
!             H_pseudo_tmp_order2(:,i_dir,ibp,ib)=cI**2/2._SP*( conjg(kbv_ks_space2(:,i_dir,ibp))*kbv_ks_space0(ib)  &
!&                                                       -2._SP*conjg(kbv_ks_space1(:,ibp))*kbv_ks_space1(i_dir,ib)  &
!&                                                         +conjg(kbv_ks_space0(ibp))*kbv_ks_space2(:,i_dir,ib)      )
!           enddo
!           if(ib/=ibp) H_pseudo_tmp_order2(:,:,ib,ibp)=conjg(H_pseudo_tmp_order2(:,:,ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo_order1(:,:,:,ik,i_sp_pol)  =H_pseudo_order1(:,:,:,ik,i_sp_pol)  +H_pseudo_tmp_order1(:,:,:)
       H_pseudo_order2(:,:,:,:,ik,i_sp_pol)=H_pseudo_order2(:,:,:,:,ik,i_sp_pol)+H_pseudo_tmp_order2(:,:,:,:)
       !
     enddo   ! i1
     !
   enddo   ! i_sp_pol
   !
   call live_timing(steps=1)
   !
   !call WF_free(WF)
   !
 enddo  ! ik
 !
 call live_timing()
 !
 ! Free mem
 YAMBO_FREE(kbv_ks_space0)
 YAMBO_FREE(kbv_ks_space1)
 YAMBO_FREE(kbv_ks_space2)
 YAMBO_FREE(kbv_r0)
 YAMBO_FREE(kbv_r1)
 YAMBO_FREE(kbv_r2)
 YAMBO_FREE(kbv)
 !
end subroutine RT_H_pseudo
