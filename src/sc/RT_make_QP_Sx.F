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
 ! QP_Sx(qp_state) = - (0.,1.) * \sum_{n,n',q} rho_rho_(n,n',q,qp_state) x G_lesser(n,n',k-q)
 ! 
 use pars,           ONLY:SP
 use QP_m,           ONLY:QP_nk,QP_n_states,QP_Sx
 use SC,             ONLY:SC_bands
 use oscillators_IO, ONLY:rho_VX_rho,n_rho_states,rho_table,ioRHO_RHO_VX 
 use par_proc_m,     ONLY:pp_indexes,pp_redux_wait,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL
 !
 implicit none
 !       
 complex(SP), intent(in)   :: G_input(SC_bands,SC_bands,QP_nk)
 !
 ! Work Space
 !
 integer           :: i1,ib,ibp,ik,ic
 integer           :: io_err,ID       
 type(pp_indexes)  :: px
 !
 call pp_indexes_reset(px)   
 !
 call par_indexes(px,(/QP_n_states/))
 !
 QP_Sx=(0.,0.)
 !
 do i1=1,QP_n_states 
   !
   if(.not.px%i1p(i1)) cycle  
   !
   call io_control(ACTION=OP_RD_CL,SEC=(/i1+1/),ID=ID)
   io_err=ioRHO_RHO_VX(i1,ID)
   !
   do ic=1,n_rho_states
     !      
     ib =rho_table(1,ic)
     ibp=rho_table(2,ic)
     ik =rho_table(3,ic)
     !
     QP_Sx(i1)=QP_Sx(i1)-(0.,1.)*rho_VX_rho(ic)*G_input(ib,ibp,ik)      
     !
   enddo
   !
 enddo
 !
 call pp_redux_wait(QP_Sx)
 call pp_indexes_reset(px)
 ! 
end subroutine 
