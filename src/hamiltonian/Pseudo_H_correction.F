!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine Pseudo_H_correction(Xk,Xen)
 !
 use pars,          ONLY:SP,cZERO
 use drivers,       ONLY:l_sc_magnetic,l_real_time
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use wrapper,       ONLY:V_dot_V
 use X_m,           ONLY:X_t
 use wave_func,     ONLY:WF,wf_ng
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:n_atoms
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_ref_bands,Hzero
#if defined _RT
 use real_time,     ONLY:Ho_plus_sigma
#endif
 use interfaces,    ONLY:WF_load,WF_free
 use io_m,          ONLY:OP_RD,RD,RD_CL,io_control
 use timing_m,      ONLY:timing
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
 complex(SP),allocatable :: kbv_real_space(:,:,:),kbv_ks_space(:,:),kbv_gauged(:,:,:),gauge_factor(:,:),kpt_factor(:,:)
 complex(SP)             :: H_pseudo(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2)), &
&                           H_pseudo_corr(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),Xk%nibz,n_sp_pol)
 integer                 :: kbv_real_space_table(pp_kbv_dim)
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spin,i_spinor,i_sp_pol,iwf,i_atom,ifrag
 !
 ! I / O
 integer                 :: ID,io_KB_real_space_err
 integer, external       :: io_KB_real_space
 !
#if defined _MAGNETIC
 if(l_sc_magnetic) call fft_setup(0,1,.true.)  ! fft_size
#endif
 !
 YAMBO_ALLOC(kbv_real_space,(fft_size,n_spinor,pp_kbv_dim))
 !
 call io_control(ACTION=OP_RD,SEC=(/1/),ID=ID)
 io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
 !
 if(io_KB_real_space_err/=0) then
   call io_control(ACTION=RD_CL,SEC=(/1/),ID=ID)
   io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
   YAMBO_FREE(kbv_real_space)
   return
 endif
 !
 YAMBO_ALLOC(kbv_ks_space,(H_ref_bands(1):H_ref_bands(2),2))
 YAMBO_ALLOC(kbv_gauged,(fft_size,n_spinor,2))
 YAMBO_ALLOC(kpt_factor,(fft_size,n_atoms))
 YAMBO_ALLOC(gauge_factor,(fft_size,n_atoms))
 !
 H_pseudo_corr=cZERO
 !
#if defined _MAGNETIC
 if(l_sc_magnetic) call live_timing('MAG-pseudo',Xk%nibz*n_sp_pol)
#endif
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz
     !
     ifrag=ik+(i_sp_pol-1)*Xk%nibz
     !
#if defined _MAGNETIC
     if(l_sc_magnetic) call WF_load(WF,0,1,(/H_ref_bands(1),H_ref_bands(2)/),(/ik,ik/),title='-Pseudo',impose_free_and_alloc=.TRUE.)
#endif
     !
#if defined _RT
     if(l_real_time) call timing('Pseudo kbv I/O',OPR='start')
#endif
     !
     if (ifrag< n_sp_pol*Xk%nibz) call io_control(ACTION=RD,SEC=(/ifrag+1/),ID=ID)
     if (ifrag==n_sp_pol*Xk%nibz) call io_control(ACTION=RD_CL,SEC=(/ifrag+1/),ID=ID)
     io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
     !
#if defined _RT
     if(l_real_time) then
       call timing('Pseudo kbv I/O',OPR='stop')
       call timing('Pseudo_correction',OPR='start')
     endif
#endif
     !
     call Pseudo_KB_gauge_factor(Xk%pt(ik,:),gauge_factor,kpt_factor)
     !
     do i1=1,pp_kbv_dim
       !     
       kbv_ks_space=cZERO
       !
       i_atom=kbv_real_space_table(i1)
       !
       do i_spinor=1,n_spinor
         kbv_gauged(:,i_spinor,1)=kbv_real_space(:,i_spinor,i1)*kpt_factor(:,i_atom)*gauge_factor(:,i_atom)
         kbv_gauged(:,i_spinor,2)=kbv_real_space(:,i_spinor,i1)*kpt_factor(:,i_atom)
       enddo
       !
       do ib=H_ref_bands(1),H_ref_bands(2)
         !
         iwf=WF%index(ib,ik,i_sp_pol)
         !
         kbv_ks_space(ib,1)=V_dot_V(fft_size*n_spinor,WF%c(:,:,iwf),kbv_gauged(:,:,1))
         kbv_ks_space(ib,2)=V_dot_V(fft_size*n_spinor,WF%c(:,:,iwf),kbv_gauged(:,:,2))
         !
         do ibp=H_ref_bands(1),ib
           !
           H_pseudo(ibp,ib)=conjg(kbv_ks_space(ibp,1))*kbv_ks_space(ib,1)-conjg(kbv_ks_space(ibp,2))*kbv_ks_space(ib,2)
           if(ib/=ibp) H_pseudo(ib,ibp)=conjg(H_pseudo(ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo_corr(:,:,ik,i_sp_pol)=H_pseudo_corr(:,:,ik,i_sp_pol)+H_pseudo(:,:)
       !
     enddo   ! i1
     !
#if defined _RT
     if(l_real_time) call timing('Pseudo_correction',OPR='stop')
#endif
   !
#if defined _MAGNETIC
   if(l_sc_magnetic) then
     call live_timing(steps=1)
     call WF_free(WF)
   endif
#endif
     !
   enddo   ! ik
   !
 enddo  ! i_sp_pol
 !
#if defined _MAGNETIC
 if(l_sc_magnetic) then
   call live_timing()
   Hzero(:,:,:,:)=Hzero(:,:,:,:)+H_pseudo_corr(:,:,:,:)
 endif
#endif
 !
#if defined _RT
 if(l_real_time) Ho_plus_sigma=Ho_plus_sigma+H_pseudo_corr(:,:,:,:)
#endif
 !
 ! Free mem
 YAMBO_FREE(kbv_ks_space)
 YAMBO_FREE(kbv_real_space)
 YAMBO_FREE(kbv_gauged)
 YAMBO_FREE(kpt_factor)
 YAMBO_FREE(gauge_factor)
 !
end subroutine Pseudo_H_correction
