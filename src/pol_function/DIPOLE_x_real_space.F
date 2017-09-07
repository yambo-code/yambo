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
subroutine DIPOLE_x_real_space(Xen,Xk,X,idir_todo)
 !
 use drivers,            ONLY:l_sc_run
 use pars,               ONLY:SP,cI,cZERO
 use D_lattice,          ONLY:x_cc
 use wave_func,          ONLY:WF
 use interfaces,         ONLY:WF_load,WF_free
 use wrapper_omp,        ONLY:Vstar_dot_V_omp
 use FFT_m,              ONLY:fft_size
 use R_lattice,          ONLY:bz_samp
 use electrons,          ONLY:levels,n_sp_pol,n_spinor
 use X_m,                ONLY:X_t,DIP_iR,DIP_P,force_v_g_space_approach,l_X_terminator
 use com,                ONLY:error
 use LIVE_t,             ONLY:live_timing
 use parallel_m,         ONLY:PAR_IND_DIPk_ibz,PAR_DIPk_ibz_index
 !
#include<memory.h>
 !
 type(bz_samp), intent(in)    :: Xk
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(in)    :: X
 logical,       intent(in)    :: idir_todo(3)
 !
 ! Work space
 !
 integer                      :: ik,ik_mem,i_sp_pol,i_spinor,ic,iv,ic_min,&
&                                ic_wf,iv_wf,idir,n_LT_steps
 logical                      :: l_mat_eval_DIP(X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 real(SP)                     :: Ev_m_Ec
 complex(SP)                  :: DIP_tmp(3,X%ib_lim(2):X%ib(2),X%ib(1):X%ib_lim(1))
 !
 call load_cc(.false.,.true.)
 !
 ! WF load
 ! Here I avoid the deallocation of states_to_load as I ASSUME that AFTER this routine 
 ! there will be other WF_load calls using the present WF distribution. 
 !
 call WF_load(WF,0,1,(/1,X%ib(2)/),(/1,Xk%nibz/),space='R',title='-Oscillators/R space',&
&             force_WFo=l_sc_run,keep_states_to_load=.TRUE.)
 !
 call DIPOLE_build_parallelization_mask(X,l_mat_eval_DIP,n_LT_steps) 
 !
 if (n_LT_steps>0) call live_timing('Dipoles in real space',n_LT_steps)
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz 
     !
     if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
     !
     ik_mem=PAR_DIPk_ibz_index(ik)
     !
     DIP_tmp=cZERO
     !
     do iv=X%ib(1),X%ib_lim(1) 
       !
       ic_min=max(iv,X%ib_lim(2))
       if (l_X_terminator) ic_min=iv
       !
       do ic=ic_min,X%ib(2)
         !
         if (.not.l_mat_eval_DIP(ic,iv)) cycle
         !
         iv_wf=WF%index(iv,ik,i_sp_pol)
         ic_wf=WF%index(ic,ik,i_sp_pol)
         !
         if (iv_wf==0) call error(" Error in parallel wf distribution (Dipole iv)")
         if (ic_wf==0) call error(" Error in parallel wf distribution (Dipole ic)")
         !
         do i_spinor=1,n_spinor
           do idir=1,3
             !
             ! Here I assume that for the non periodic direction id_red==id_cartesian
             !
             if(.not.idir_todo(idir)) cycle
             !
             DIP_tmp(idir,ic,iv) = DIP_tmp(idir,ic,iv) + &
&                       Vstar_dot_V_omp(fft_size,WF%c(1:fft_size,i_spinor,iv_wf), &
&                             x_cc(1:fft_size,idir)*WF%c(1:fft_size,i_spinor,ic_wf) )
             !
           enddo
         enddo
         !
         call live_timing(steps=1)
         !
       enddo 
       !
     enddo
     !
     !======
     ! <iR>
     !======
     do idir=1,3
       if(.not.idir_todo(idir)) cycle
       DIP_iR(idir,:,:,ik_mem,i_sp_pol)=cI*DIP_tmp(idir,:,:)
     enddo
     !
     !=====
     ! <P>
     !=====
     if(.not.force_v_g_space_approach) then
       do iv=X%ib(1),X%ib_lim(1) 
         do ic=max(iv,X%ib_lim(2)),X%ib(2)
           if (.not.l_mat_eval_DIP(ic,iv)) cycle
           ! This is directly p_eff, see Dipole transverse and PRB 48, 11789 (1993)
           Ev_m_Ec=Xen%E(iv,ik,i_sp_pol)-Xen%E(ic,ik,i_sp_pol)
           do idir=1,3
             if(.not.idir_todo(idir)) cycle
             DIP_P(idir,ic,iv,ik_mem,i_sp_pol)=DIP_iR(idir,ic,iv,ik_mem,i_sp_pol)*Ev_m_Ec
           enddo
         enddo
       enddo
     endif
     !
   enddo 
   !
 enddo
 !
 call live_timing()
 !
 YAMBO_FREE(x_cc)
 !
 call WF_free(WF)
 !
end subroutine Dipole_x_real_space
