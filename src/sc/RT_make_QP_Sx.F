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
subroutine RT_make_QP_Sx(G_input)
 !
 ! QP_Sx(qp_state) = - (0.,1.) * \sum_{n,n'} rho_rho_(n,n',qp_state) x G_lesser(n,n',k)
 ! 
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_nk,QP_table,QP_n_states,QP_Sx
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:rho_rho_gamp
 !
 implicit none
 !       
 complex(SP), intent(in) :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer    :: i1,ib,ibp,ik
 !
#if defined _DOUBLE
 complex(DP)   :: zdotu
#else
 complex(SP)   :: cdotu
#endif
 !
 do i1=1,QP_n_states
   !
   ib =QP_table(i1,1)
   ibp=QP_table(i1,2)
   ik =QP_table(i1,3)     
   !
#if defined _DOUBLE
   QP_Sx(i1)=-(0.,1.)*zdotu(SC_bands*SC_bands,rho_rho_gamp(:,:,i1),1,G_input(:,:,ik),1)
#else 
   QP_Sx(i1)=-(0.,1.)*cdotu(SC_bands*SC_bands,rho_rho_gamp(:,:,i1),1,G_input(:,:,ik),1)
#endif
   !
 enddo
 !
end subroutine 
