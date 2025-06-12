!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM DS
!
subroutine COLLISIONS_compose_nl(G_in)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} STATIC_collisions(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use pars,             ONLY:SP,cZERO,cI
 use drivers,          ONLY:l_sc_fock,l_sc_coh,l_sc_sex,l_sc_exx,l_sc_hartree!l_sc_hf,l_sc_cohsex,l_sc_exx
 use collision_ext,    ONLY:HXC_COLL_element,COLL_bands,HXC_collisions,COLLISIONS_load_SP,&
 &                          COLLISIONS_CV_only,COLLISIONS_FEW_only,COLL_hole_bands,COLL_elec_bands
 use electrons,        ONLY:spin,n_met_bands,n_full_bands,n_sp_pol
 use R_lattice,        ONLY:nXkibz
 use parallel_m,       ONLY:HEAD_QP_cpu,PAR_COM_Xk_ibz_INDEX
 use parallel_int,     ONLY:PP_redux_wait
 use real_time,        ONLY:RT_Vnl_xc,RT_bands
#if defined _TIMING
 use timing_m,         ONLY:timing
#endif
 !
 implicit none
 !
 complex(SP), intent(in)   :: G_in(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),nXkibz,n_sp_pol)
 !
 ! Work Space
 !
 integer           :: i_coll,ib,ibp,i_k,i_k_NL,i_kmq,i_comp,i_coll_mem,i_sp,ip_sp
 integer           :: i_n,i_m
 complex(SP)       :: COLL
 logical           :: do_collisions(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),n_sp_pol),coll_sp
 !
 if(.not.l_sc_fock.and..not.l_sc_coh.and..not.l_sc_sex.and..not.l_sc_exx.and..not.l_sc_hartree) return
 !
#if defined _TIMING
 call timing('NL COLL_compose',OPR='start')
#endif
 !
 do_collisions=.false.
 coll_sp=COLLISIONS_load_SP
 !
 do i_sp=1,n_sp_pol
 do ibp=COLL_bands(1),COLL_bands(2)
   if ( COLLISIONS_FEW_only .and. ib<COLL_hole_bands(1) ) cycle
   if ( COLLISIONS_FEW_only .and. ib>COLL_hole_bands(2) .and. ib<COLL_elec_bands(1) ) cycle
   if ( COLLISIONS_FEW_only .and. ib>COLL_elec_bands(2) ) cycle
   do ib=COLL_bands(1),COLL_bands(2)
     if ( COLLISIONS_CV_only .and. (ib> n_met_bands(i_sp)  .and. ibp> n_met_bands(i_sp) ) ) cycle
     if ( COLLISIONS_CV_only .and. (ib<=n_full_bands(i_sp) .and. ibp<=n_full_bands(i_sp)) ) cycle
     if ( COLLISIONS_FEW_only .and. ibp<COLL_hole_bands(1) ) cycle
     if ( COLLISIONS_FEW_only .and. ibp>COLL_hole_bands(2) .and. ibp<COLL_elec_bands(1) ) cycle
     if ( COLLISIONS_FEW_only .and. ibp>COLL_elec_bands(2) ) cycle
     do_collisions(ib,ibp,i_sp)=.true.
   enddo
 enddo
 enddo
 !
 RT_Vnl_xc=cZERO
 !
 do i_coll=1,HXC_collisions%N
   !
   if (.not.(HXC_collisions%PAR_IND%element_1D(i_coll).and.HEAD_QP_cpu)) cycle
   !
   i_k       =HXC_collisions%state(i_coll,3)
   i_n       =HXC_collisions%state(i_coll,1)
   i_m       =HXC_collisions%state(i_coll,2)
   i_sp      =HXC_collisions%state(i_coll,4)
   i_coll_mem=HXC_collisions%PAR_map(i_coll)
   !
   i_comp=1
   !
   i_k_NL=i_k
   !
   do ip_sp=1,n_sp_pol
   do i_kmq=1,nXkibz
     !
      do ibp=COLL_bands(1),COLL_bands(2)
       do ib=COLL_bands(1),COLL_bands(2)
         !
         if (HXC_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq,ip_sp)=='f') cycle
         !
         if ( .not.do_collisions(ib,ibp,ip_sp) ) cycle
         !
         if(coll_sp) then
           COLL=cmplx(HXC_COLL_element(i_coll_mem)%v_c_SP(i_comp),kind=SP)
         else
           COLL=cmplx(HXC_COLL_element(i_coll_mem)%v_c(i_comp),kind=SP)
         endif
         RT_Vnl_xc(i_n,i_m,i_k_NL,i_sp)=RT_Vnl_xc(i_n,i_m,i_k_NL,i_sp)-cI*COLL*G_in(ib,ibp,i_kmq,ip_sp)
         i_comp=i_comp+1
         !
       enddo
     enddo
   enddo
   enddo
   !
 enddo
 !
 ! Symmetrization
 !================
 do i_coll=1,HXC_collisions%N
   !
   if (.not.(HXC_collisions%PAR_IND%element_1D(i_coll).and.HEAD_QP_cpu)) cycle
   i_k       =HXC_collisions%state(i_coll,3)
   i_n       =HXC_collisions%state(i_coll,1)
   i_m       =HXC_collisions%state(i_coll,2)
   i_sp      =HXC_collisions%state(i_coll,4)
   !
   RT_Vnl_xc(i_n,i_n,i_k,i_sp) =real(RT_Vnl_xc(i_n,i_n,i_k,i_sp),SP)
   RT_Vnl_xc(i_m,i_n,i_k,i_sp)=conjg(RT_Vnl_xc(i_n,i_m,i_k,i_sp))
   ! 
 enddo
 !
 call PP_redux_wait(RT_Vnl_xc,COMM=PAR_COM_Xk_ibz_INDEX%COMM)
 !
#if defined _TIMING
 call timing('NL COLL_compose',OPR='stop')
#endif
 !
end subroutine COLLISIONS_compose_nl
