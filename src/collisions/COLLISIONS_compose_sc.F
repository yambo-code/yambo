!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM DS
!
subroutine COLLISIONS_compose_sc(G_sex,G_coh)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} Collisions(n,n',q,qp_state) x G_sex/coh(n,n',k-q)
 ! 
 use pars,             ONLY:SP,cZERO,cI
 use drivers,          ONLY:l_sc_fock,l_sc_coh,l_sc_sex,l_sc_exx,l_sc_hartree
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_states,QP_Sc,QP_Vnl_xc
 use collision_ext,    ONLY:COH_COLL_element,HXC_COLL_element,COLL_bands,HXC_collisions,&
&                           COH_collisions,COLLISIONS_have_HARTREE
 use parallel_m,       ONLY:PAR_IND_QP,HEAD_b_cpu,PAR_IND_G_k
 use parallel_int,     ONLY:PP_redux_wait
 use timing_m,         ONLY:timing
 implicit none
 !
 complex(SP), intent(in)   :: G_sex(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),QP_nk)
 complex(SP), intent(in)   :: G_coh(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer           :: i_qp,ib,ibp,i_kmq,ic1,ic2,i_coll_mem
 logical           :: l_sex
 !
 if(.not.l_sc_fock.and..not.l_sc_coh.and..not.l_sc_sex.and..not.l_sc_exx.and.&
&   .not.(l_sc_hartree.and.COLLISIONS_have_HARTREE)) return
 !
 call timing('COLLISIONS_compose_sc',OPR='start')
 !
 ! In _SC QP_Sc is calculated on-the-fly when the collision are not used.
 ! Here all contributions are packed in QP_Vnl_xc
 !
 if (.not.allocated(QP_Vnl_xc))  allocate(QP_Vnl_xc(QP_n_states))
 !
 l_sex    =l_sc_sex.or.l_sc_fock.or.(l_sc_hartree.and.COLLISIONS_have_HARTREE)
 QP_Vnl_xc=cZERO
 !
 if (allocated(QP_Sc)) then 
   QP_Sc  =cZERO
 endif
 !
 do i_qp=1,QP_n_states
   !
   if (.not. PAR_IND_QP%element_1D(i_qp)) cycle
   if (.not. HEAD_b_cpu) cycle
   !
   if (l_sex)    i_coll_mem=HXC_collisions%PAR_map(i_qp)
   if (l_sc_coh) i_coll_mem=COH_collisions%PAR_map(i_qp)
   !
   ic1=1
   ic2=1
   !
   do i_kmq=1,QP_nk
     !
     if (.not.PAR_IND_G_k%element_1D(i_kmq)) then 
       if (l_sc_coh) then
         ic1=ic1+count(COH_COLL_element(i_coll_mem)%table(COLL_bands(1):QP_nb,COLL_bands(1):QP_nb,i_kmq)=='t')
       endif
       if (l_sex) then
         ic2=ic2+count(HXC_COLL_element(i_coll_mem)%table(COLL_bands(1):QP_nb,COLL_bands(1):QP_nb,i_kmq)=='t')
       endif
       cycle
     endif
     !
     do ibp=COLL_bands(1),QP_nb
       do ib=COLL_bands(1),QP_nb
         !
         ! COH part
         !
         if (l_sc_coh) then
           if (COH_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq)=='t') then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-cI*COH_COLL_element(i_coll_mem)%v_c(ic1)*G_coh(ib,ibp,i_kmq)
             ic1=ic1+1
           endif
         endif
         !
         ! SEX/HF/HARTREE parts
         !
         if (l_sex) then
           if (HXC_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq)=='t') then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-cI*HXC_COLL_element(i_coll_mem)%v_c(ic2)*G_sex(ib,ibp,i_kmq)
             ic2=ic2+1
           endif
         endif
         !
       enddo
     enddo
     !
   enddo
 enddo
 !
 call PP_redux_wait(QP_Vnl_xc)
 !
 call timing('COLLISIONS_compose_sc',OPR='stop')
 !
end subroutine COLLISIONS_compose_sc
