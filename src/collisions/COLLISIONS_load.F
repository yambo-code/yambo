!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA DS
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
subroutine COLLISIONS_load(CHECK_EXISTENCE_ONLY)
 !
 ! Load all collisions contribution from any kind of self-energy 
 ! exchange, COHSEX etc.. 
 !
 use drivers,         ONLY:l_sc_fock,l_sc_coh,l_sc_sex,l_sc_exx,l_use_collisions,l_sc_srpa,&
&                          l_elel_scatt,l_elphoton_scatt,l_elphoton_corr,l_sc_hartree,&
&                          l_use_Hxc_collisions,l_real_time,l_use_COH_collisions
 use LIVE_t,          ONLY:live_timing
 use QP_m,            ONLY:QP_QED_ng
 use R_lattice,       ONLY:nqbz
 use plasma,          ONLY:N_plasma_poles_global
 use hamiltonian,     ONLY:H_kind
 use global_XC,       ONLY:H_SE_FOCK,H_SE_COH,H_SE_SEX,QP_SE_GW_NEQ,H_SE_HARTREE
 use collision_ext,   ONLY:HXC_COLL_element,COH_COLL_element,COLLISIONS_have_HARTREE,&
&                          COH_collisions,HXC_collisions,COLLISIONS_group,COLLISIONS_naming
 use IO_int,          ONLY:io_control
 use IO_m,            ONLY:OP_RD,RD,RD_CL,OP_RD_CL,VERIFY,REP,NONE
 use parallel_m,      ONLY:PAR_IND_Plasma,PAR_IND_Q_bz,PAR_Q_bz_index,PAR_PLASMA_index
 use COLL_interfaces, ONLY:io_COLLISIONS
 use timing_m,        ONLY:timing
#if defined _QED || defined _RT
 use global_XC,       ONLY:QP_SE_GW_QED
 use collision_ext,   ONLY:P_COLL_element,COLL_bands,P_collisions,GW_NEQ_collisions,&
&                          GW_NEQ_COLL_element
#endif
 !
 ! Define io_COLLs_header to handle either serial or parallel I/O
#include<collisions.h>
 !
 implicit none
 !
 logical            :: CHECK_EXISTENCE_ONLY
 !
 ! Work Space
 !
 integer            :: COLL_kind(4),ID,io_err(4),COM_,i1,i_sec,IO_ACT
 logical            :: l_use_scatt_collisions,l_warning,l_error,l_use_P_collisions
 !
#if defined _PAR_IO
 integer, external  :: io_COLLISIONS_header
#endif
 !
 io_err      =0
 !
 if (.not.any((/l_sc_fock,l_sc_coh,l_sc_hartree,l_sc_sex,l_sc_exx,l_sc_srpa,l_elel_scatt,l_elphoton_scatt,l_elphoton_corr/))) return
 !
 call timing('COLLISIONS_load',OPR='start')
 !
 ! Initial Setup & EXISTENCE
 !============================
 !
 COM_=NONE
 if (CHECK_EXISTENCE_ONLY) COM_=REP
 !
 if (l_sc_coh) then
   call COLLISIONS_basic_operations( "alloc", COH_collisions )
   call io_control(ACTION=OP_RD_CL,COM=COM_,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_err(1)=io_COLLs_header(ID,COH_collisions)
 endif
 !
 if (l_sc_sex.or.l_sc_fock.or.l_sc_hartree.or.l_sc_exx.or.l_sc_srpa) then
   COLL_kind=(/H_kind,H_kind-H_SE_COH,H_kind-H_SE_HARTREE,H_kind-H_SE_COH-H_SE_HARTREE/)
   io_err(2)=1
   do i1=1,4
     !
     call COLLISIONS_naming( COLL_kind(i1) , "HXC" )
     !
     if (HXC_collisions%kind==0) cycle
     !
     call COLLISIONS_basic_operations( "alloc", HXC_collisions )
     if (i1>1) COM_=NONE 
     call io_control(ACTION=OP_RD_CL,COM=COM_,MODE=VERIFY,SEC=(/1/),ID=ID)
     io_err(2)=io_COLLs_header(ID,HXC_collisions)
     !
     if (io_err(2)==0) exit
     !
   enddo
   !
   if (CHECK_EXISTENCE_ONLY.and.io_err(2)==0.and.HXC_collisions%kind<H_SE_Hartree) then
     call warning("[COLLISIONS] Found (and using) an Hartree-free collisions database")
   endif
   !
 endif
 !
#if defined _RT
 if (l_elel_scatt) then
   call COLLISIONS_basic_operations( "alloc", GW_NEQ_collisions )
   call io_control(ACTION=OP_RD_CL,COM=COM_,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_err(3)=io_COLLs_header(ID,GW_NEQ_collisions)
 endif
#endif
 !
#if defined _QED
 if (l_elphoton_scatt.or.l_elphoton_corr) then
   call COLLISIONS_basic_operations( "alloc", P_collisions )
   call io_control(ACTION=OP_RD_CL,COM=COM_,MODE=VERIFY,SEC=(/1/),ID=ID)
   io_err(4)=io_COLLs_header(ID,P_collisions)
 endif
#endif
 !
 if (CHECK_EXISTENCE_ONLY) then
   !
   l_use_COH_collisions  =io_err(1)==0.and.l_sc_coh
   l_use_Hxc_collisions  =io_err(2)==0.and.(l_sc_sex.or.l_sc_fock.or.l_sc_hartree.or.l_sc_exx.or.l_sc_srpa)
   l_use_P_collisions    =io_err(4)==0.and.(l_elphoton_scatt.or.l_elphoton_corr)
   !
   l_warning=.not.l_use_Hxc_collisions.and..not.l_use_COH_collisions.and..not.l_use_P_collisions
   l_error  =l_warning.and.l_real_time.and.(l_sc_sex.or.l_sc_fock)
   !
   if (l_use_COH_collisions.and.l_sc_sex.and..not.l_use_Hxc_collisions) l_error=.TRUE.
   !
   if (l_error)   call error(  ' HXC collisions not found / not compatible')
   if (l_warning) call warning(' HXC collisions not found. Potential is computed on the fly.')
   !
   l_use_scatt_collisions=all(io_err(3:4)==0).and.(l_elphoton_scatt.or.l_elphoton_corr.or.l_elel_scatt)
   if (io_err(3)/=0)                 call error(' el-el scatt collisions not found / not compatible') 
   if (io_err(4)/=0.and.l_real_time) call error(' el-photon   collisions not found / not compatible')
   !
   l_use_collisions=l_use_Hxc_collisions.or.l_use_scatt_collisions.or.l_use_COH_collisions
   COLLISIONS_have_HARTREE=io_err(2)==0.and.HXC_collisions%kind>=H_SE_HARTREE
   !
 endif
 !
 if (.not.l_use_collisions.or.CHECK_EXISTENCE_ONLY) then
   !
   call timing('COLLISIONS_load',OPR='stop')
   return
   !
 endif
 !
 ! Actual I/O
 !============
 !
 if (l_use_COH_collisions)                 call local_IO( COH_collisions )
 !
 if (l_use_Hxc_collisions)                 call local_IO( HXC_collisions )
 !
#if defined _RT
 if (l_elel_scatt)                         call local_IO( GW_NEQ_collisions )
#endif
 !
#if defined _QED
 if (l_elphoton_scatt.or.l_elphoton_corr)  call local_IO( P_collisions )
#endif
 !
 call timing('COLLISIONS_load',OPR='stop')
 !
 contains
   !
   subroutine local_IO( COLL )
     !
     implicit none
     !
     type(COLLISIONS_group) :: COLL
     integer :: iqbz,i_J,ic,i_coll,&
&               i_coll_mem,i_q_mem,i_J_mem,i_b,i_g
     logical :: l_COH,l_HXC,l_NGW,l_QED,l_NGW_init,l_QED_init
     !
     call live_timing(trim(COLL%name)//' collisions I/O',COLL%PAR_N)
     !
     l_COH=(COLL%kind==H_SE_COH)
     l_HXC=(COLL%kind==H_SE_FOCK.or.COLL%kind==H_SE_SEX.or.COLL%kind>=H_SE_HARTREE)
#if defined _QED
     l_QED=(COLL%kind==QP_SE_GW_QED)
#endif
#if defined _RT
     l_NGW=(COLL%kind==QP_SE_GW_NEQ)
#endif
     !
     l_NGW_init=l_NGW
     l_QED_init=l_QED
     !
     if (l_COH.or.l_HXC) then
       call COLLISIONS_alloc_and_free(trim(COLL%name),"collision","alloc",0)
     endif
     !
#if defined _QED
     if (l_QED) then
       call COLLISIONS_alloc_and_free("Momentum","collision","alloc",0)
       call COLLISIONS_alloc_and_free("Momentum","v3","alloc ",0)
       l_QED      = .not.allocated(P_COLL_element(1)%v_r)
       l_QED_init = .not.allocated(P_COLL_element(1)%v_r)
     endif
#endif
     !
#if defined _RT
     if (l_NGW) then
       call COLLISIONS_alloc_and_free("GW_NEQ","collision","alloc",0)
       call COLLISIONS_alloc_and_free("GW_NEQ","v3","alloc distribute",0)
       l_NGW      = .not.allocated(GW_NEQ_COLL_element(1)%v_r)
       l_NGW_init = .not.allocated(GW_NEQ_COLL_element(1)%v_r)
     endif
#endif
     !
     ! Header
     !
     call io_control(ACTION=OP_RD_CL,SEC=(/1/),ID=ID)
     !
     if (l_COH) io_err(1)=io_COLLs_header(ID,COLL)
     if (l_HXC) io_err(1)=io_COLLs_header(ID,COLL)
     if (l_NGW) io_err(1)=io_COLLs_header(ID,COLL)
     if (l_QED) io_err(1)=io_COLLs_header(ID,COLL)
     !
#if defined _PAR_IO
     call io_control(ACTION=OP_RD,SEC=(/1/),ID=ID)
     if (l_COH) io_err(1)=io_COLLISIONS(ID,COLL,COH_COLL_element(1))
     if (l_HXC) io_err(1)=io_COLLISIONS(ID,COLL,HXC_COLL_element(1))
# if defined _RT
     if (l_NGW) io_err(1)=io_COLLISIONS(ID,COLL,GW_NEQ_COLL_element(1))
# endif
# if defined _QED
     if (l_QED) io_err(1)=io_COLLISIONS(ID,COLL,P_COLL_element(1))
# endif
     IO_ACT=RD
#endif
     !
     do i_coll=1,COLL%N
       !
       if (.not.COLL%PAR_IND%element_1D(i_coll)) cycle
       !
       i_coll_mem=COLL%PAR_map(i_coll)
       !
#if !defined _PAR_IO
       IO_ACT=OP_RD
#endif
       !
       call io_control(ACTION=IO_ACT,SEC=(/2/),ID=ID)
       !
       if (l_COH) io_err(1)=io_COLLISIONS(ID,COLL,COH_COLL_element(i_coll_mem))
       if (l_HXC) io_err(1)=io_COLLISIONS(ID,COLL,HXC_COLL_element(i_coll_mem))
#if defined _RT
       if (l_NGW_init) io_err(1)=io_COLLISIONS(ID,COLL,GW_NEQ_COLL_element(1),i_coll=i_coll)
#endif
#if defined _QED
       if (l_QED_init) io_err(1)=io_COLLISIONS(ID,COLL,P_COLL_element(1),i_coll=i_coll)
#endif
       !
       if (l_COH) call COLLISIONS_alloc_and_free(trim(COLL%name),"v1 table","alloc",i_coll_mem)
       if (l_HXC) call COLLISIONS_alloc_and_free(trim(COLL%name),"v1 table","alloc",i_coll_mem)
       if (l_NGW_init) call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","alloc",1)
       if (l_QED_init) call COLLISIONS_alloc_and_free("Momentum","v1 table","alloc",1)
       !
       l_NGW_init=.false.
       l_QED_init=.false.
       !
#if !defined _PAR_IO
       IO_ACT=RD_CL
#endif
       call io_control(ACTION=IO_ACT,SEC=(/3,4/),ID=ID)
       !
       if (l_COH) io_err(1)=io_COLLISIONS(ID,COLL,COH_COLL_element(i_coll_mem))
       if (l_HXC) io_err(1)=io_COLLISIONS(ID,COLL,HXC_COLL_element(i_coll_mem))
#if defined _RT
       if (l_NGW) io_err(1)=io_COLLISIONS(ID,COLL,GW_NEQ_COLL_element(1),i_coll=i_coll)
#endif
#if defined _QED
       if (l_QED) io_err(1)=io_COLLISIONS(ID,COLL,P_COLL_element(1),i_coll=i_coll)
#endif
       !
       ! Now the transfer %v_r => %v3_r
       !
#if defined _RT || defined _QED
       if (COLL%kind==QP_SE_GW_NEQ.or.COLL%kind==QP_SE_GW_QED) then
         !
         ic=0
         !
         do iqbz=1,nqbz
           !
           i_q_mem=PAR_Q_bz_index(iqbz)
           !
           if (COLL%kind==QP_SE_GW_NEQ) then
             !
             if (.not.PAR_IND_Q_bz%element_1D(iqbz)) then
               ic=ic+N_plasma_poles_global
               cycle
             endif
             do i_J=1,N_plasma_poles_global
               ic=ic+1
               if (.not.PAR_IND_Plasma%element_1D(i_J)) cycle
               i_J_mem  =PAR_PLASMA_index(i_J)
               GW_NEQ_COLL_element(i_coll_mem)%v3_r(i_J_mem,1,i_q_mem)=GW_NEQ_COLL_element(1)%v_r(ic)
             enddo
           else
             do i_b=COLL_bands(1),COLL_bands(2)
               do i_g=1,QP_QED_ng
                 ic=ic+1
                 P_COLL_element(i_coll_mem)%v3_r(i_g,i_b,iqbz)=P_COLL_element(1)%v_r(ic)
               enddo
             enddo
           endif
           !
         enddo
       endif
#endif
       !
       call live_timing(steps=1)
       !
     end do
     !
#if defined _PAR_IO
     ! close file
     call io_control(ACTION=RD_CL,SEC=(/5/),ID=ID)
     io_err(1)=io_COLLISIONS(ID,COLL)
#endif
     !
     ! CLEAN
     !
#if defined _RT
     if (COLL%kind==QP_SE_GW_NEQ) call COLLISIONS_alloc_and_free("GW_NEQ","v1 table","clean",1)
#endif
#if defined _QED
     if (COLL%kind==QP_SE_GW_QED) call COLLISIONS_alloc_and_free("Momentum","v1 table","clean",1)
#endif
     !
     call live_timing()
     !
   end subroutine
   !
end subroutine COLLISIONS_load
