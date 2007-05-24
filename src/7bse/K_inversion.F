!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine K_inversion(iq,W,E,k,q) 
 !
 use pars,           ONLY:SP,pi
 use drivers,        ONLY:l_ph_corr
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,BS_not_const_eh_f,&
&                         BS_K_coupling,BS_eh_table,BS_eh_E,BS_bands,Kernel_by_V
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:spin_occ,levels,BZ_RIM_nbands
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm,bz_samp
 use com,            ONLY:isec,warning
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,ncpu,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD,RD_CL,REP,VERIFY
#if defined PJ_PH
 use ELPH,           ONLY:Rnd_Qpts_at,Rnd_Qpts_corresp,EXCPH_mat
#endif
 implicit none
 type(w_samp) :: W
 integer      :: iq
 type(levels) :: E
 type(bz_samp):: k,q
 !
 ! Work Space...
 !
 type(pp_indexes) ::px
 integer          ::i1,i2,ik_bz,iv,ic,iw,icv,i_err,of_mode,i_sp
 logical          ::use_RIM,start_from_diago
 complex(SP)      ::Co
 integer, external::pert_inversion,eval_ELPH_K
 complex(SP)      ::Lo_RIM(BS_K_dim,W%n(1))
 !
#if defined _DOUBLE
 complex(DP)::zdotu,zdotc
#else
 complex(SP)::cdotu,cdotc
#endif
#if defined PJ_PH
 complex(SP)::WS(BS_K_dim,BS_K_dim)
 logical    ::add_ELPH_K
#endif
 !
 ! RIM support ?
 !
 use_RIM=BZ_RIM_nbands>=BS_bands(2)
 !
 ! Sectioning
 !
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
 if (.not.use_RIM) call warning('Inversion solver: only with a RIM database')
 if (BS_not_const_eh_f) call warning('Inversion solver: fractional occupations unsupported')
 if (BS_K_coupling) call warning('Inversion solver: unsupported for non-resonant K')
 if (any((/BS_not_const_eh_f,BS_K_coupling/))) return
 !
 !  mem est
 !
#if defined PJ_PH
 call mem_est("Lo_RIM WS",(/size(Lo_RIM),BS_K_dim**2/))
#else
 call mem_est("Lo_RIM WS",(/size(Lo_RIM),2*BS_K_dim**2/))
#endif
 !
 ! Kernel loading
 !
 call Kernel_by_V(iq)
 !
 forall(i1=1:BS_K_dim) BS_mat(i1,i1)=BS_mat(i1,i1)-BS_eh_E(i1)
 !
#if defined PJ_PH
 !
 ! SAVE BS_mat & calculaye ELPH_K
 !
 WS=BS_mat
 add_ELPH_K=.false.
 if (l_ph_corr) add_ELPH_K=eval_ELPH_K(E,k,q,.TRUE.,(0.,0.))==0
#endif
 !
 ! Initialize the output file 
 !
 of_mode=3
#if defined PJ_PH
 if (add_ELPH_K) of_mode=4
#endif
 call K_dump_to_o_file(iq,-of_mode)
 !
 ! Green`s function and non-interacting eps2
 !
 X_epsilon=(0.,0.)
 Lo_RIM=(0.,0.)
 Co=real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
 !
 call live_timing('Green`s function',BS_K_dim)
 !
 do i1=1,BS_K_dim
   !
   ik_bz=BS_eh_table(i1,1)
   iv   =BS_eh_table(i1,2)
   ic   =BS_eh_table(i1,3)
#if defined PJ_SPIN
   i_sp=BS_eh_table(i1,4)
#else
   i_sp=1
#endif
   !
   if (.not.use_RIM) then
     Lo_RIM(i1,:)=1./(W%p(:)-BS_eh_E(i1))
   else
     call X_RIM_Gf(1,(/ik_bz,iv,ic,i_sp/),(/1,W%n(1)/),W,E,k,Lo_RIM(i1,:),"r")
   endif
   X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*Lo_RIM(i1,:)*Co
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! Perturbative Invertion
 !========================
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/W%n(1)/))
 !
 ! Without starting from excitonic wavefunctions: the kernel is 
 ! used as perturbation
 !
 call live_timing('Frequency loop',px%stps(myid+1))
 !
 do iw=1,W%n(1)
   !
#if defined PJ_PH
   if (add_ELPH_K) then
     BS_mat=WS
     call static2dyn_ELPH_K(E,k,q,W%p(iw),BS_mat)
   endif
#endif
   !
   if (.not.px%i1p(iw)) cycle
   !
   i_err=pert_inversion(BS_K_dim,BSS_rhoq0,-1.*Co,X_epsilon(2,iw),&
&                       Lo_RIM(:,iw),BS_mat,.TRUE.)
   !
   X_epsilon(2,iw)=1.+X_epsilon(2,iw)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 call pp_redux_wait(X_epsilon)
 X_epsilon(1,:)=W%p(:)
 !
 call K_dump_to_o_file(iq,of_mode)
 !
1 continue
 !
 ! CLEAN
 !
 call pp_indexes_reset(px)
 !
 deallocate(BS_mat)
 call mem_est("BS_mat Lo_RIM WS")
#if defined PJ_PH
 if (allocated(EXCPH_mat)) then
   deallocate(Rnd_Qpts_corresp,Rnd_Qpts_at,EXCPH_mat)
   call mem_est("EXCPH_mat")
 endif
#endif
 !
end subroutine
