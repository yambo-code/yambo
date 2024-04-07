! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM MG DS
!
subroutine K_Haydock_scatter(V,fullV,fullV_size,n_ColsFG)
 !
 ! Scatter the full dimension vector on the master (read from disk/restart)
 ! into Haydock vector fragments 
 ! 
 use pars,          ONLY:SP
 use BS,            ONLY:BS_T_grp,BS_nT_grps
 use BS_solvers,    ONLY:Haydock_vector
 use parallel_int,  ONLY:PP_bcast
 use parallel_m,    ONLY:PAR_COM_Xk_ibz_INDEX,PAR_COM_eh_INDEX,PAR_IND_T_Haydock
 !
 implicit none
 !
 integer,    intent(in) :: fullV_size
 complex(SP),intent(in) :: fullV(fullV_size)
 !
 type(Haydock_vector),intent(inout) :: V(BS_nT_grps)
 ! ws: 
 integer                 :: istart_g,istop_g,i_g
 integer                 :: i_ColFG,n_ColsFG(BS_nT_grps)
 !
 ! In this loop I need T_groups Haydock since I do need duplicates 
 !
 call PP_bcast(fullV,0,COMM=PAR_COM_Xk_ibz_INDEX%COMM)  
 call PP_bcast(fullV,0,COMM=PAR_COM_eh_INDEX%COMM)  
 istop_g = 0
 !
 do i_g=1,BS_nT_grps
   do i_ColFG=1,n_ColsFG(i_g)
     istart_g = istop_g + 1 
     istop_g  = istart_g + BS_T_grp(i_g)%size - 1
     if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
     V(i_g)%fragment(1:BS_T_grp(i_g)%size,i_ColFG) = fullV(istart_g:istop_g)  
   enddo
 enddo
 !
end subroutine K_Haydock_scatter
