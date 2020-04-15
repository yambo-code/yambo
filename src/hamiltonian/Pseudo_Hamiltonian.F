!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine Pseudo_Hamiltonian(Xk,Xen,l_equilibrium)
 !
 use pars,          ONLY:SP,cZERO
 use drivers,       ONLY:l_sc_magnetic,l_real_time
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use wrapper,       ONLY:V_dot_V
 use X_m,           ONLY:X_t
 use wave_func,     ONLY:WF
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:n_atoms
 use LIVE_t,        ONLY:live_timing
 use hamiltonian,   ONLY:H_ref_bands,Hzero
#if defined _RT
 use real_time,     ONLY:Ho_plus_sigma,H_pseudo_eq
#endif
 use interfaces,    ONLY:WF_load,WF_free
 use io_m,          ONLY:OP_RD_CL,OP_RD,RD,RD_CL,io_control
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 logical       :: l_equilibrium
 !
 ! Work Space
 !
 type(X_t)     :: X
 !
 complex(SP),allocatable :: kbv_real_space(:,:,:),kbv_ks_space(:),kbv_gauged(:,:),gauge_factor(:,:)
 complex(SP)             :: H_pseudo_ik(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2)), &
&                           H_pseudo(H_ref_bands(1):H_ref_bands(2),H_ref_bands(1):H_ref_bands(2),Xk%nibz,n_sp_pol)
 integer                 :: kbv_real_space_table(pp_kbv_dim)
 !
 ! Dummies
 integer                 :: i1,ib,ibp,ik,i_spin,i_spinor,i_sp_pol,iwf,i_atom,ifrag
 !
 ! I / O
 integer                 :: ID,io_KB_real_space_err,ACT
 integer, external       :: io_KB_real_space
 !
#if defined _MAGNETIC
 if(l_sc_magnetic) call fft_setup(0,1,.true.)  ! fft_size
#endif
 !
 YAMBO_ALLOC(kbv_real_space,(fft_size,n_spinor,pp_kbv_dim))
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1/),ID=ID)
 io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
 !
 if(io_KB_real_space_err/=0) then
   call io_control(ACTION=OP_RD_CL,SEC=(/1/),ID=ID)
   io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
   YAMBO_FREE(kbv_real_space)
   return
 endif
 !
 YAMBO_ALLOC(kbv_ks_space,(H_ref_bands(1):H_ref_bands(2)))
 YAMBO_ALLOC(kbv_gauged,(fft_size,n_spinor))
 YAMBO_ALLOC(gauge_factor,(fft_size,n_atoms))
 !
 H_pseudo=cZERO
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
     if(l_sc_magnetic) then
       call WF_load(WF,0,1,(/H_ref_bands(1),H_ref_bands(2)/),(/ik,ik/),title='-Pseudo',impose_free_and_alloc=.TRUE.)
       ACT=OP_RD_CL
     endif
#endif
     !
#if defined _RT
     if(l_real_time) then
       call timing('Pseudo kbv I/O',OPR='start')
       if (ifrag==1               ) ACT=OP_RD
       if (ifrag< n_sp_pol*Xk%nibz) ACT=RD
       if (ifrag==n_sp_pol*Xk%nibz) ACT=RD_CL
      endif
#endif
     !
     call io_control(ACTION=ACT,SEC=(/ifrag+1/),ID=ID)
     io_KB_real_space_err=io_KB_real_space(ID,kbv_real_space,kbv_real_space_table)
     !
#if defined _RT
     if(l_real_time) then
       call timing('Pseudo kbv I/O',OPR='stop')
       call timing('Pseudo correction',OPR='start')
     endif
#endif
     !
     if(.not.l_equilibrium) call Pseudo_KB_gauge_factor(gauge_factor)
     !
     do i1=1,pp_kbv_dim
       !     
       kbv_ks_space=cZERO
       !
       i_atom=kbv_real_space_table(i1)
       !
       do i_spinor=1,n_spinor
         if(.not.l_equilibrium) kbv_gauged(:,i_spinor)=kbv_real_space(:,i_spinor,i1)*gauge_factor(:,i_atom)
         if(     l_equilibrium) kbv_gauged(:,i_spinor)=kbv_real_space(:,i_spinor,i1)
       enddo
       !
       do ib=H_ref_bands(1),H_ref_bands(2)
         !
         iwf=WF%index(ib,ik,i_sp_pol)
         !
         kbv_ks_space(ib)=V_dot_V(fft_size*n_spinor,WF%c(:,:,iwf),kbv_gauged)
         !
         do ibp=H_ref_bands(1),ib
           !
           H_pseudo_ik(ibp,ib)=conjg(kbv_ks_space(ibp))*kbv_ks_space(ib)
           if(ib/=ibp) H_pseudo_ik(ib,ibp)=conjg(H_pseudo_ik(ibp,ib))
           !
         enddo   ! ibp
       enddo   ! ib
       !
       H_pseudo(:,:,ik,i_sp_pol)=H_pseudo(:,:,ik,i_sp_pol)+H_pseudo_ik(:,:)
       !
     enddo   ! i1
     !
#if defined _RT
     if(l_real_time) call timing('Pseudo correction',OPR='stop')
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
   if(.not.l_equilibrium) Hzero=Hzero+H_pseudo
   if(     l_equilibrium) Hzero=Hzero-H_pseudo
 endif
#endif
 !
#if defined _RT
 if(l_real_time) then
   if(.not.l_equilibrium) Ho_plus_sigma=Ho_plus_sigma+(H_pseudo-H_pseudo_eq)
   if(     l_equilibrium) H_pseudo_eq  =H_pseudo
 endif
#endif
 !
 ! Free mem
 YAMBO_FREE(kbv_ks_space)
 YAMBO_FREE(kbv_real_space)
 YAMBO_FREE(kbv_gauged)
 YAMBO_FREE(gauge_factor)
 !
end subroutine Pseudo_Hamiltonian
