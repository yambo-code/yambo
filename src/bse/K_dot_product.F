!
!        Copyright (C) 2000-2023 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG DS IMA
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
subroutine K_dot_product(dot,V,W,i_kind)
 !
 ! Dot product between distributed vectors 
 ! i_kind==1 -->        F dot product
 ! i_kind==0 --> standard dot product
 !
 use pars,           ONLY:SP,cZERO
 use BS_solvers,     ONLY:Haydock_vector
 use BS,             ONLY:BS_nT_grps,BS_T_grp
 use parallel_m,     ONLY:PAR_IND_T_groups,PAR_COM_Xk_ibz_INDEX,PAR_COM_eh_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use wrapper_omp,    ONLY:Vstar_dot_V_omp
 !
 implicit none
 !
 integer,              intent(in) :: i_kind
 type(Haydock_vector), intent(in) :: V(BS_nT_grps),W(BS_nT_grps)
 complex(SP),          intent(out):: dot
 !
 ! Workspace
 !
 integer       :: i_T_g,i_factor,i_ColFG,ColsFG_T
 !
 dot = cZERO
 ! 
 ! NOTE FOR DEVELOPERS:
 ! Anti-block contains the same vector components as the corresponding 
 ! res-block. Sum on all blocks will give twice the value in the case
 ! with coupling. So anti-blocks are skipped if BS_res_ares_dim=1
 !
 do i_T_g=1,BS_nT_grps
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   i_factor= ( (-1)**(BS_T_grp(i_T_g)%i_res_ares-1) )**i_kind
   ColsFG_T = size(V(i_T_g)%fragment,2)
   do i_ColFG=1,ColsFG_T
     dot=dot+ i_factor*Vstar_dot_V_omp(BS_T_grp(i_T_g)%size,V(i_T_g)%fragment(:,i_ColFG),W(i_T_g)%fragment(:,i_ColFG))
   enddo
 enddo
 !
 ! Reduce all
 !
 call PP_redux_wait( dot ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( dot ,COMM=PAR_COM_eh_INDEX%COMM )
 !
end subroutine K_dot_product
