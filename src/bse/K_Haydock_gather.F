! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
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
