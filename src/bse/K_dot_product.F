!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine K_dot_product(dot,V,W)
 !
 ! Dot product between distributed vectors 
 !
 use pars,           ONLY:SP,cZERO
 use BS,             ONLY:BS_blk,n_BS_blks,Haydock_vector,BS_nT_grps,BS_K_Coupling,&
&                         Haydock_parallel_group_comm,Haydock_parallel_col_comm 
 use parallel_m, ONLY:PP_bcast,PP_redux_wait
 !
 implicit none
 !
 complex(SP),          intent(out):: dot
 type(Haydock_vector), intent(in) :: V(:),W(:) 
 !
 ! Workspace
 !
 integer       :: i_b,i_Tk,i_Tp,i_g
 complex(SP)   :: dot_
 !
 dot_ = cZERO
 ! 
 ! NOTE FOR DEVELOPERS:
 ! Anti-block contains the same vector components as the corrisponding 
 ! res-block. Sum on all blocks will give twice the value in the case
 ! with coupling.
 !
 do i_b=1,n_BS_blks
   if (BS_blk(i_B)%mode == "A") cycle 
   i_Tk  =BS_blk(i_b)%iT_k
   i_Tp  =BS_blk(i_b)%iT_p
   if (i_Tk.ne.1) cycle
   dot_ = dot_ + dot_product(V(i_Tp)%fragment,W(i_Tp)%fragment)
 end do
 !
#if defined _MPI
 !
 ! Reduce all (sum) on first row
 !
 call PP_redux_wait(dot_,COMM=Haydock_parallel_group_comm(1))
 !
 ! BCast on all other procs along 'the columns'
 !
 do i_g = 2,BS_nT_grps
   call PP_bcast(dot_,0,COMM=Haydock_parallel_col_comm(i_g))
 end do
 !
#endif
 !
 dot = dot_
 !
end subroutine K_dot_product

