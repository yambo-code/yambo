!
! Copyright (C) 2000-2014 C. Attaccalite and the YAMBO team
!              http://www.yambo-code.org
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
subroutine COLLISIONS_compose(G_hf_input,G_xc_input)
 !
 ! HF(qp_state) = - (0.,1.) * \sum_{n,n',q} STATIC_collisions(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use drivers,          ONLY:l_sc_hf,l_sc_cohsex,l_sc_exx,l_sc_srpa
 use pars,             ONLY:SP
 use QP_m,             ONLY:QP_nk,QP_nb,QP_n_states,QP_Vnl_xc,QP_Sc
 use SC,               ONLY:SC_bands
 use collision,        ONLY:HF_collisions,COHSEX_collisions,nHF_collisions,nCOHSEX_collisions
 use parallel_m,       ONLY:PAR_QP_index,PAR_IND_QP,PP_redux_wait,HEAD_QP_cpu
 !
 implicit none
 !       
 complex(SP), intent(in)   :: G_hf_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 complex(SP), intent(in)   :: G_xc_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer           :: i_qp,ib,ibp,ik,ic1,ic2,i_qp_mem
 !
 if(.not.l_sc_hf.and..not.l_sc_cohsex.and..not.l_sc_exx) return
 !
 if (.not.allocated(QP_Vnl_xc))  allocate(QP_Vnl_xc(QP_n_states))
 QP_Vnl_xc=(0._SP,0._SP)
 !
 ! In _SC QP_Sc is calculated on-the-fly when the collision are not used.
 ! Here all contributions are packed in QP_Vnl_xc
 ! 
 if (l_sc_cohsex.or.l_sc_srpa) QP_Sc=(0._SP,0._SP)
 !
 do i_qp=1,QP_n_states
   !
   if (.not.(PAR_IND_QP%element_1D(i_qp).and.HEAD_QP_cpu)) cycle
   !
   i_qp_mem=PAR_QP_index(i_qp)
   !
   ic1=1
   ic2=1
   !
   do ib=SC_bands(1),QP_nb
     do ibp=SC_bands(1),QP_nb
       !
       do ik=1,QP_nk
         if (nHF_collisions>0) then
           if (HF_collisions(i_qp_mem)%table(ib,ibp,ik)/=0) then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-(0._SP,1._SP)*HF_collisions(i_qp_mem)%v(ic1)*G_hf_input(ib,ibp,ik)
             ic1=ic1+1
           endif
         endif
         if (nCOHSEX_collisions>0) then
           if (COHSEX_collisions(i_qp_mem)%table(ib,ibp,ik)/=0) then
             QP_Vnl_xc(i_qp)=QP_Vnl_xc(i_qp)-(0._SP,1._SP)*COHSEX_collisions(i_qp_mem)%v(ic2)*G_xc_input(ib,ibp,ik)
             ic2=ic2+1
           endif
         endif
       enddo
     enddo
   enddo
   !
 enddo
 !
 call PP_redux_wait(QP_Vnl_xc)
 !
end subroutine 
