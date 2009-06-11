!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team
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
subroutine RT_make_QP_Sx(ik,k,G_input)
 !
 ! QP_Sx(qp_state) = - (0.,1.) * \sum_{n,n',q} rho_rho_(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_nk,QP_table_by_k,QP_n_states_for_k,QP_Sx
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:rho_rho_gamp
 use R_lattice,      ONLY:bz_samp,qindx_S
 !
 implicit none
 !       
 complex(SP), intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 integer, intent(in)       :: ik
 type(bz_samp), intent(in) :: k
 !
 ! Work Space
 !
 integer    :: i1,i2,ib,ibp,iqp,iq,ik_minus_q,SC_bands2
 !
#if defined _DOUBLE
 complex(DP)   :: zdotu
#else
 complex(SP)   :: cdotu
#endif
 !
 SC_bands2=SC_bands**2
 !
 do i1=1,QP_n_states_for_k(ik)
   !
   ib =QP_table_by_k(ik,i1,1)
   ibp=QP_table_by_k(ik,i1,2)
   iqp=QP_table_by_k(ik,i1,3)
   !
   QP_Sx(iqp)=(0.,0.)
   !
   do ik_minus_q=1,QP_nk
     !
#if defined _DOUBLE
     QP_Sx(iqp)=QP_Sx(iqp)-(0.,1.)*zdotu(SC_bands2,rho_rho_gamp(:,:,ik_minus_q,iqp),1,G_input(:,:,ik_minus_q),1)
#else 
     QP_Sx(iqp)=QP_Sx(iqp)-(0.,1.)*cdotu(SC_bands2,rho_rho_gamp(:,:,ik_minus_q,iqp),1,G_input(:,:,ik_minus_q),1)
#endif
     !
   enddo
   !
 enddo
 !
end subroutine 
