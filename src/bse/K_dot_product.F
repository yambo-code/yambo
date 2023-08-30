!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): MG DS IMA
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
