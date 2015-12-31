!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine COLLISIONS_compose_rt(G_input)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} STATIC_collisions(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use pars,             ONLY:SP,cZERO,cI
 use drivers,          ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx
 use QP_m,             ONLY:QP_table,QP_nk,QP_n_states
 use SC,               ONLY:SC_bands
 use collision,        ONLY:Fock_collisions,SEX_collisions,nFock_collisions,nSEX_collisions
 use electrons,        ONLY:spin
 use parallel_m,       ONLY:PAR_QP_index,PAR_IND_QP,PP_redux_wait,HEAD_QP_cpu
 use real_time,        ONLY:RT_nk,RT_ibz_coo,RT_Nk_ibz,RT_Vnl_xc,l_RT_uses_E_DbGd
 !
 implicit none
 !
 complex(SP), intent(in)   :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),RT_nk)
 !
 ! Work Space
 !
 logical           :: l_expand
 integer           :: i_qp,ib,ibp,i_k,i_kmq,ic1,ic2,i_qp_mem,i_spin
 integer           :: i_n,i_m,i_k_RT,i_kmq_RT
 complex(SP)       :: G_avg(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 !
 if(.not.l_sc_hf.and..not.l_sc_cohsex.and..not.l_sc_exx) return
 !
 G_avg=cZERO
 !
 do i_kmq=1,QP_nk
   do i_kmq_RT=RT_ibz_coo(i_kmq,1),RT_ibz_coo(i_kmq,2)
     G_avg(:,:,i_kmq)=G_avg(:,:,i_kmq)+G_input(:,:,i_kmq_RT)
   enddo
   !
   ! The 1/real(RT_Nk_ibz(i_kmq),SP) factor is needed here because I'm integrating
   ! over kmq. With the DbGd the size of the integrating factor has to be reduced
   ! 
   G_avg(:,:,i_kmq)=G_avg(:,:,i_kmq)/real(RT_Nk_ibz(i_kmq),SP)
   !
 enddo
 !
 RT_Vnl_xc=cZERO
 !
 do i_qp=1,QP_n_states
   !
   if (.not.(PAR_IND_QP%element_1D(i_qp).and.HEAD_QP_cpu)) cycle
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   i_k=QP_table(i_qp,3)
   i_n=QP_table(i_qp,1)
   i_m=QP_table(i_qp,2)
   !
   ic1=1
   ic2=1
   !
   i_spin=spin(QP_table(i_qp,:))
   !
   i_k_RT=RT_ibz_coo(i_k,1)
   !
   do i_kmq=1,QP_nk
     !
     do ibp=SC_bands(1),SC_bands(2)
       do ib=SC_bands(1),SC_bands(2)
         !
         if (nFock_collisions>0) then
           if (Fock_collisions(i_qp_mem)%table(ib,ibp,i_kmq)/=0) then
             RT_Vnl_xc(i_n,i_m,i_k_RT,i_spin)=RT_Vnl_xc(i_n,i_m,i_k_RT,i_spin) &
&                 -cI*Fock_collisions(i_qp_mem)%v_c(ic1)*G_avg(ib,ibp,i_kmq)
             ic1=ic1+1
           endif
         endif
         if (nSEX_collisions>0) then
           if (SEX_collisions(i_qp_mem)%table(ib,ibp,i_kmq)/=0) then
             RT_Vnl_xc(i_n,i_m,i_k_RT,i_spin)=RT_Vnl_xc(i_n,i_m,i_k_RT,i_spin) &
&                 -cI*SEX_collisions(i_qp_mem)%v_c(ic2)*G_avg(ib,ibp,i_kmq)
             ic2=ic2+1
           endif
         endif
         !
       enddo
     enddo
     !
   enddo
   !
   if(.not.l_RT_uses_E_DbGd) cycle
   !
   l_expand=.false.
   if(nFock_collisions>0) l_expand=l_expand.or.any(Fock_collisions(i_qp_mem)%table/=0)
   if(nSEX_collisions >0) l_expand=l_expand.or.any(SEX_collisions(i_qp_mem)%table /=0)
   !
   if(l_expand) RT_Vnl_xc(i_n,i_m,RT_ibz_coo(i_k,1)+1:RT_ibz_coo(i_k,2),i_spin)=RT_Vnl_xc(i_n,i_m,i_k_RT,i_spin)
   !
 enddo
 !
 ! Symmetrization
 !================
 do i_qp=1,QP_n_states
   !
   if (.not.(PAR_IND_QP%element_1D(i_qp).and.HEAD_QP_cpu)) cycle
   !
   i_k=QP_table(i_qp,3)
   i_n=QP_table(i_qp,1)
   i_m=QP_table(i_qp,2)
   !
   RT_Vnl_xc(i_n,i_n,RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:)=real(RT_Vnl_xc(i_n,i_n,RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:))
   RT_Vnl_xc(i_m,i_n,RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:)=conjg(RT_Vnl_xc(i_n,i_m,RT_ibz_coo(i_k,1):RT_ibz_coo(i_k,2),:))
   !
 enddo
 !
 call PP_redux_wait(RT_Vnl_xc)
 !
end subroutine COLLISIONS_compose_rt
