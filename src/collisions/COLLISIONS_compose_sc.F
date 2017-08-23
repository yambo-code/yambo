!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA AM DS
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
subroutine COLLISIONS_compose_sc(G_sex,G_coh)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} Collisions(n,n',q,qp_state) x G_sex/coh(n,n',k-q)
 ! 
 use pars,             ONLY:SP,cZERO,cI
 use drivers,          ONLY:l_sc_fock,l_sc_coh,l_sc_sex,l_sc_exx,l_sc_hartree
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_states,QP_Sc,QP_Vnl_xc
 use collision_ext,    ONLY:COH_COLL_element,HXC_COLL_element,COLL_bands,HXC_collisions,&
&                           COH_collisions,COLLISIONS_have_HARTREE
 use parallel_m,       ONLY:PAR_IND_QP,PP_redux_wait,HEAD_QP_cpu
 !
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 complex(SP), intent(in)   :: G_sex(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),QP_nk)
 complex(SP), intent(in)   :: G_coh(COLL_bands(1):COLL_bands(2),COLL_bands(1):COLL_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer           :: i_qp,ib,ibp,i_kmq,ic1,ic2,i_coll_mem
 !
 if(.not.l_sc_fock.and..not.l_sc_coh.and..not.l_sc_sex.and..not.l_sc_exx.and.&
&   .not.(l_sc_hartree.and.COLLISIONS_have_HARTREE)) return
 !
#if defined _TIMING
 call timing('COLLISIONS_compose_sc',OPR='start')
#endif
 ! In _SC QP_Sc is calculated on-the-fly when the collision are not used.
 ! Here all contributions are packed in QP_Vnl_xc
 !
 if (.not.allocated(QP_Vnl_xc))  allocate(QP_Vnl_xc(QP_n_states))
 !
 QP_Vnl_xc=cZERO
 !
 if (allocated(QP_Sc)) then 
   QP_Sc  =cZERO
 endif
 !
 do i_qp=1,QP_n_states
   !
   if (.not.(PAR_IND_QP%element_1D(i_qp).and.HEAD_QP_cpu)) cycle
   !
   if (l_sc_sex.or.l_sc_fock.or.(l_sc_hartree.and.COLLISIONS_have_HARTREE)) i_coll_mem=HXC_collisions%PAR_map(i_qp)
   if (l_sc_coh)                                                            i_coll_mem=COH_collisions%PAR_map(i_qp)
   !
   ic1=1
   ic2=1
   !
   do i_kmq=1,QP_nk
     !
     do ibp=COLL_bands(1),QP_nb
       do ib=COLL_bands(1),QP_nb
         !
         ! COH part
         !
         if (l_sc_coh) then
           if (COH_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq)/=0) then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-cI*COH_COLL_element(i_coll_mem)%v_c(ic1)*G_coh(ib,ibp,i_kmq)
             ic1=ic1+1
           endif
         endif
         !
         ! SEX/HF/HARTREE parts
         !
         if (l_sc_sex.or.l_sc_fock.or.(l_sc_hartree.and.COLLISIONS_have_HARTREE)) then
           if (HXC_COLL_element(i_coll_mem)%table(ib,ibp,i_kmq)/=0) then
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
#if defined _TIMING
 call timing('COLLISIONS_compose_sc',OPR='stop')
#endif
 !
end subroutine COLLISIONS_compose_sc
