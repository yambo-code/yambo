!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG DS
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
subroutine K_Haydock_gather(V,fullV,fullV_size,n_ColsFG)
 !
 ! Gather the Haydock vector fragments into one 
 ! large vector of full dimension  
 ! on the master for I/O 
 ! 
 use pars,          ONLY:SP,cZERO
 use BS,            ONLY:BS_T_grp,BS_nT_grps
 use BS_solvers,    ONLY:Haydock_vector
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX,PAR_IND_T_groups, &
                         PAR_COM_eh_INDEX
 !
 implicit none
 !
 type(Haydock_vector),intent(in) :: V(BS_nT_grps)
 integer,    intent(in)  :: fullV_size
 complex(SP),intent(out) :: fullV(fullV_size)
 ! ws: 
 integer                 :: istart_g,istop_g,i_g
 integer                 :: i_ColFG,n_ColsFG(BS_nT_grps)
 !
 fullV = cZERO
 istop_g = 0
 !
 ! In this loop I need T_groups indexes since I do not want duplicates 
 !
 do i_g=1,BS_nT_grps
   do i_ColFG=1,n_ColsFG(i_g)
     istart_g = istop_g + 1 
     istop_g  = istart_g + BS_T_grp(i_g)%size - 1
     if (.not.PAR_IND_T_groups%element_1D(i_g)) cycle      
     fullV(istart_g:istop_g) = V(i_g)%fragment(1:BS_T_grp(i_g)%size,i_ColFG)
   enddo
 enddo
 !
 call PP_redux_wait( fullV ,COMM=PAR_COM_Xk_ibz_INDEX%COMM )
 call PP_redux_wait( fullV ,COMM=PAR_COM_eh_INDEX%COMM )
 !
end subroutine K_Haydock_gather
