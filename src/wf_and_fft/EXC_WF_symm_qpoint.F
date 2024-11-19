!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine EXC_WF_symm_qpoint(Xk,qpt,first_state,nstates,iq,isq,igq,&
&                                        ID,S_index,EXC_WFs,l_min_mem)
 !
 use pars,          ONLY:SP,cZERO,cONE
 use electrons,     ONLY:n_sp_pol,n_max_deg
 use D_lattice,     ONLY:idt_index,i_time_rev,nsym,sop_inv,sop_tab
 use R_lattice,     ONLY:bz_samp,qindx_X,qindx_C
 use BS_solvers,    ONLY:BS_mat,BSS_n_eig,BSS_eh_table_m1
 use BS,            ONLY:BS_bands,BS_H_dim
 use wave_func,     ONLY:io_WF_phases,WF_phases_b_map,WF_phases
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:DUMP,REP,RD
 !
#include<y_memory.h>
 !
 type(bz_samp), intent(in)   :: Xk,qpt
 integer,       intent(in)   :: first_state,nstates
 integer,       intent(in)   :: iq,isq,igq
 integer,       intent(inout):: ID
 complex(SP),   intent(out)  :: EXC_WFs(BS_H_dim,first_state:nstates)
 integer,       intent(in)   :: S_index(BSS_n_eig)
 logical,       intent(in)   :: l_min_mem
 !
 ! Work space
 !
 logical                 :: t_rev
 integer :: iexc,il,ik,istark,isk,ikbz,ipbz,ipmqbz,ipmqg0,iv1,ic1,iv2,ic2,&
 &          i_v1_bnd,i_v1_grp,i_c1_bnd,i_c1_grp,i_v2_bnd,i_v2_grp,i_c2_bnd,i_c2_grp,&
 &          ikmqbz,ikmqg0,iqbz,i_sp_pol,ip,ipmq,istarp,istarpmq,i1,i2,&
 &          neh1,neh2,io_err,nsz(5),V_bands(2),C_bands(2),ipmq_last
 complex(SP) :: phase_vv,phase_cc
 complex(SP), allocatable :: WF_phases_cc(:,:,:,:,:),WF_phases_vv(:,:,:,:,:)
 !
 if(isq==idt_index) then
   do iexc=first_state,nstates
     il=S_index(iexc)
     EXC_WFs(:,iexc)=BS_mat(:,il)
   enddo
   return
 endif
 !
 t_rev= (isq>nsym/(i_time_rev+1))
 !
 iqbz=qpt%k_table(iq,isq)
 !
 !V_bands=(/BS_bands(1),maxval(Ken%nbm)/)
 !C_bands=(/minval(Ken%nbf)+1,BS_bands(2)/)
 !
 EXC_WFs=cZERO
 !
 ! First get A(l,R*q)= Ry * A(l,q)
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz
     !
     ! For the phases I need:
     ! - R p_BZ = k_BZ
     ! - the symmetry used to reach k_BZ, e.g. isk
     !ik_ref=ikbz
     !is_ref=isk
     !
     ipmq_last=0
     ! 
     if (l_min_mem) then
       !
       nsz(1:2)=maxval(WF_phases_b_map(:,ik,i_sp_pol,1))
       nsz(3)  =maxval(WF_phases_b_map(:,ik,i_sp_pol,2))
       nsz(4)  =nsym
       nsz(5)  =Xk%nstar(ik)
       !
       YAMBO_ALLOC(WF_phases_cc,(nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)))
       !
       call io_control(ACTION=RD,COM=REP,SEC=(/3/),MODE=DUMP,ID=ID)
       io_err=io_WF_phases(BS_bands,ik,0,i_sp_pol,ID,nsz,WF_phases_cc)
       !
     endif
     !
     do istark=1,Xk%nstar(ik)
       !
       isk =Xk%star(ik,istark)
       ikbz=Xk%k_table(ik,isk)
       !
       ! p_BZ = (R)^-1 k_BZ = (R)^-1 (Rk) k_IBZ
       ipbz=Xk%k_table(ik,sop_tab(sop_inv(isq),isk))
       !
       ip    =Xk%sstar(ipbz,1)
       istarp=Xk%sstar(ipbz,3)
       !
       if (ip/=ik) call error("WF_phases: rotation of ik got a different ip in the IBZ")
       !
       ! (R)^-1 k_BZ - q_IBZ = p_BZ - q_IBZ
       ipmqbz=qindx_X(iq,ipbz,1)
       ipmqg0=qindx_X(iq,ipbz,2)
       !
       ipmq    =Xk%sstar(ipmqbz,1)
       istarpmq=Xk%sstar(ipmqbz,3)
       !
       ! For the phases I need R pmq_BZ = k_BZ - R q_IBZ = k_BZ - q_BZ
       !ikmqbz=qindx_C(ikbz,iqbz,1)
       !ikmqg0=qindx_C(ikbz,iqbz,2)
       !
       if (ipmq/=ipmq_last.and.l_min_mem) then
         !
         if (ipmq_last>0) then
           YAMBO_FREE(WF_phases_vv)
         endif
         !
         nsz(1:2)=maxval(WF_phases_b_map(:,ipmq,i_sp_pol,1))
         nsz(3)  =maxval(WF_phases_b_map(:,ipmq,i_sp_pol,2))
         nsz(4)  =nsym
         nsz(5)  =Xk%nstar(ipmq)
         !
         YAMBO_ALLOC(WF_phases_vv,(nsz(1),nsz(2),nsz(3),nsz(4),nsz(5)))
         !
         if (ipmq/=ip) then
           call io_control(ACTION=RD,COM=REP,SEC=(/3/),MODE=DUMP,ID=ID)
           io_err=io_WF_phases(BS_bands,ipmq,0,i_sp_pol,ID,nsz,WF_phases_vv)
           ipmq_last=ipmq
         else
           WF_phases_vv=WF_phases_cc
           ipmq_last=ip
         endif
         !
       endif
       !
       do iexc=first_state,nstates
         il=S_index(iexc)
         !
         do iv1 = BS_bands(1),BS_bands(2) 
           !
           i_v1_bnd=WF_phases_b_map(iv1,ipmq,i_sp_pol,1)
           i_v1_grp=WF_phases_b_map(iv1,ipmq,i_sp_pol,2)
           !
           do iv2 = BS_bands(1),BS_bands(2) 
             !
             i_v2_bnd=WF_phases_b_map(iv2,ipmq,i_sp_pol,1)
             i_v2_grp=WF_phases_b_map(iv2,ipmq,i_sp_pol,2)
             !
             if (i_v1_grp/=i_v2_grp) cycle
             !
             do ic1 = BS_bands(1),BS_bands(2)
               !
               i_c1_bnd=WF_phases_b_map(ic1,ip,i_sp_pol,1)
               i_c1_grp=WF_phases_b_map(ic1,ip,i_sp_pol,2)
               !
               neh1=BSS_eh_table_m1(ikbz,iv1,ic1,i_sp_pol,1)
               !
               if (neh1==0) cycle
               !
               do ic2 = BS_bands(1),BS_bands(2) 
                 !
                 i_c2_bnd=WF_phases_b_map(ic2,ip,i_sp_pol,1)
                 i_c2_grp=WF_phases_b_map(ic2,ip,i_sp_pol,2)
                 !
                 if (i_c1_grp/=i_c2_grp) cycle
                 !
                 neh2=BSS_eh_table_m1(ipbz,iv2,ic2,i_sp_pol,1)
                 !
                 if (neh2==0) cycle
                 !
                 if (l_min_mem) then
                   phase_vv=conjg(WF_phases_vv(i_v1_bnd,i_v2_bnd,i_v1_grp,isq,istarpmq))
                   phase_cc=      WF_phases_cc(i_c1_bnd,i_c2_bnd,i_c1_grp,isq,istarp)
                 else
                   i1=1 ; i2=1
                   if(ipmq>0) i1= sum(Xk%nstar(:ipmq-1))+istarpmq
                   if(ip  >0) i2= sum(Xk%nstar(:ip  -1))+istarp
                   phase_vv=conjg(WF_phases(i_v1_bnd,i_v2_bnd,i_v1_grp,isq,i1))
                   phase_cc=      WF_phases(i_c1_bnd,i_c2_bnd,i_c1_grp,isq,i2)
                 endif
                 !
                 if(.not.t_rev) EXC_WFs(neh1,iexc)=EXC_WFs(neh1,iexc)+      BS_mat(neh2,il) *phase_vv*phase_cc
                 if(     t_rev) EXC_WFs(neh1,iexc)=EXC_WFs(neh1,iexc)+conjg(BS_mat(neh2,il))*phase_vv*phase_cc
                 !
               enddo
             enddo
             !
           enddo
         enddo
         !
       enddo
       !
     enddo
     !
     if (l_min_mem) then
       YAMBO_FREE(WF_phases_cc)
       YAMBO_FREE(WF_phases_vv)
     endif
     !
   enddo
 enddo
 !
end subroutine EXC_WF_symm_qpoint
