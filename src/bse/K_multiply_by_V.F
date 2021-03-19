!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine K_multiply_by_V(Vi,Vo,iter_par)
 !
 ! Given
 !
 !      |   (K_r)     (cI*K_c)  |  
 !  K = |                       |
 !      | -(cI*K_c)^*  (-K_r)^* |
 !
 ! distibuted on the processors, and Vi distributed on the processors
 !
 ! Multiply K by Vi
 ! 
 use pars,           ONLY:SP,cONE,cZERO,cI
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS_solvers,     ONLY:Haydock_vector,BSS_Slepc_double_grp
 use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,l_BS_ares_asymm
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 integer                ::iter_par
 type(Haydock_vector)   ::Vi(BS_nT_grps),Vo(BS_nT_grps)
 !
 ! Work Space
 !
 integer         ::i_B,i_Tk,i_Tp,i_g,i_dom_k,dom_k
 complex(SP)     ::alpha
 ! 
 if(.not.BSS_Slepc_double_grp) alpha=-real(iter_par,SP)*cI
 if(     BSS_Slepc_double_grp) alpha=-real(iter_par,SP)
 !
!aim137: ok not to specify indexes to fragment
 forall(i_g=1:BS_nT_grps,PAR_IND_T_Haydock%element_1D(i_g)) Vo(i_g)%fragment = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   dom_k = size(Vi(i_Tp)%fragment,2)
   !
   select case(BS_blk(i_B)%mode)
   case("R")
     !
     ! Resonant part 
     !
!aim137: if there is no DG, sum is trivial
!aim137: result of matrix * sum goes to column 1 of output vector
!aim137: output columns get expanded at energy stage
do i_dom_k=1,dom_k
     call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                       Vi(i_Tp)%fragment(:,i_dom_k),1,cONE,&
     &                       Vo(i_Tk)%fragment(:,i_dom_k),1)
     !
     ! Resonant part is hermitian
     !
     if (i_Tk.ne.i_Tp) &
     &  call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                       Vi(i_Tk)%fragment(:,i_dom_k),1,cONE,&
     &                       Vo(i_Tp)%fragment(:,i_dom_k),1)
     !
     ! Add the energy diffs on the diagonal
     !
!aim137: I do this only for the first col here (Coarse Gd)
     if (i_Tk.eq.i_Tp) &
     & Vo(i_Tk)%fragment(:,i_dom_k) = Vo(i_Tk)%fragment(:,i_dom_k) + BS_T_grp(i_Tk)%E(:,i_dom_k)*Vi(i_Tk)%fragment(:,i_dom_k)
enddo
     !
   case("C")
     !
     if (.not.l_BS_ares_asymm) then
       !
       ! Coupling part
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
       &                       conjg(sum(Vi(i_Tp)%fragment,dim=2)),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Coupling part can be symmetric
       !
       if ( i_Tk.ne.i_Tp ) &
       &  call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
       &                       conjg(sum(Vi(i_Tk)%fragment,dim=2)),1,cONE,&
       &                       Vo(i_Tp)%fragment(:,1),1)
       !
       !
       !
     else
       !
       ! Coupling part
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
       &                       sum(Vi(i_Tp)%fragment,dim=2),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Anti-coupling Q = -conjg(C)
       ! 
       call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-cONE,&
       &                     BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
       &                     sum(Vi(i_Tk)%fragment,dim=2),1,cONE,&
       &                     Vo(i_Tp)%fragment(:,1),1)
       !
     endif
     !
     !
   case("A")
     !
     ! Anti-Resonant part 
     !
     call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                       sum(Vi(i_Tp)%fragment,dim=2),1,cONE,&
     &                       Vo(i_Tk)%fragment(:,1),1)
     !
     ! Anti-Resonant part is hermitian
     !
     if (i_Tk.ne.i_Tp) &
     &  call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
     &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
     &                       sum(Vi(i_Tk)%fragment,dim=2),1,cONE,&
     &                       Vo(i_Tp)%fragment(:,1),1)
     !
     ! Add the energy diffs on the diagonal
     !
     if (i_Tk.eq.i_Tp) &
     & Vo(i_Tk)%fragment(:,1) = Vo(i_Tk)%fragment(:,1) + BS_T_grp(i_Tk)%E(:,1)*Vi(i_Tk)%fragment(:,1)
     !
   end select
   !
 end do
 !
#if defined _MPI
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   call PP_redux_wait(Vo(i_g)%fragment,COMM=PAR_COM_T_Haydock(i_g)%COMM)
 enddo
#endif
 !
!aim137: Expand Col 1 to all other columns (FG)
!aim137: Before copying it, I substract the Energies from Vo(col1)
!if ( dom_k > 1 ) then
!  do i_g=1,BS_nT_grps
!    do i_dom_k=2,dom_k
!      Vo(i_g)%fragment(:,i_dom_k) = Vo(i_g)%fragment(:,1) &
!      & - BS_T_grp(i_g)%E(:,1)*Vi(i_g)%fragment(:,1) &
!      & + BS_T_grp(i_g)%E(:,i_dom_k)*Vi(i_g)%fragment(:,i_dom_k)

!    enddo
!  enddo
!endif
 !
end subroutine
!
!
!aim137: critical: still need to modify this subroutine accordingly
!aim137: the extra dimension of fragment is hardcoded to 1 so it does not crash
subroutine K_multiply_by_V_transpose(Vi,Vo,iter_par)
 !
 ! Given
 !
 !      |   (K_r)     (cI*K_c)  |  
 !  K = |                       |
 !      | -(cI*K_c)^*  (-K_r)^* |
 !
 ! distibuted on the processors, and Vi distributed on the processors
 !
 ! Multiply K by Vi
 ! 
 use pars,           ONLY:SP,cONE,cZERO,cI
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS_solvers,     ONLY:Haydock_vector,BSS_Slepc_double_grp
 use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,l_BS_ares_asymm
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 integer                ::iter_par
 type(Haydock_vector)   ::Vi(BS_nT_grps),Vo(BS_nT_grps)
 !
 ! Work Space
 !
 integer         ::i_B,i_Tk,i_Tp,i_g
 complex(SP)     ::alpha
 !
 ! The factor cI is needed for the coupling when l_BS_ares_asymm=.false.
 !
!aim137: checking this is not used in my example
print*, 'in K_multiply_by_V_transpose'
 if(.not.BSS_Slepc_double_grp) alpha=-real(iter_par,SP)*cI
 if(     BSS_Slepc_double_grp) alpha=-real(iter_par,SP)
 !
 forall(i_g=1:BS_nT_grps,PAR_IND_T_Haydock%element_1D(i_g)) Vo(i_g)%fragment = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   select case(BS_blk(i_B)%mode)
   case("R")
     !
     ! Resonant part 
     !
     if (i_Tk==i_Tp) then
       !
       call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Add the energy diffs on the diagonal
!aim137: termporary ,1 so it compiles
       Vo(i_Tk)%fragment(:,1) = Vo(i_Tk)%fragment(:,1) + BS_T_grp(i_Tk)%E(:,1)*Vi(i_Tk)%fragment(:,1)
       !
     else
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       conjg(BS_blk(i_B)%mat),BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Resonant part is hermitian
       !
       call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tk)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tp)%fragment(:,1),1)
       !
     endif
     !
     !
     !
   case("C")
     !
     if (.not.l_BS_ares_asymm) then
       !
       ! Coupling part
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-alpha,&
       &                       conjg(BS_blk(i_B)%mat),BS_blk(i_B)%size(1),conjg(Vi(i_Tp)%fragment(:,1)),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Coupling part can be symmetric
       !
       if ( i_Tk.ne.i_Tp ) & 
       &  call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-alpha,&
       &                       conjg(BS_blk(i_B)%mat),BS_blk(i_B)%size(1),conjg(Vi(i_Tk)%fragment(:,1)),1,cONE,&
       &                       Vo(i_Tp)%fragment(:,1),1)
       !
     else
       !
       ! Coupling part
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-cONE,&
       &                       conjg(BS_blk(i_B)%mat),BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Anti-coupling Q = -(-(C)*)^dag=C^t
       !
       call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                     BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tk)%fragment(:,1),1,cONE,&
       &                     Vo(i_Tp)%fragment(:,1),1)
       !
     endif
     !
   case("A")
     !
     ! Anti-Resonant part 
     !
     if (i_Tk==i_Tp) then
       !
       call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Add the energy diffs on the diagonal
       !
!aim137: termporary ,1 so it compiles
       Vo(i_Tk)%fragment(:,1) = Vo(i_Tk)%fragment(:,1) + BS_T_grp(i_Tk)%E(:,1)*Vi(i_Tk)%fragment(:,1)
       !
     else
       !
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       conjg(BS_blk(i_B)%mat),BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tk)%fragment(:,1),1)
       !
       ! Anti-Resonant part is hermitian
       !
       call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tk)%fragment(:,1),1,cONE,&
       &                       Vo(i_Tp)%fragment(:,1),1)
       !
     endif
     !
   end select
   !
 end do
 !
#if defined _MPI
 do i_g=1,BS_nT_grps
   if (.not.PAR_IND_T_Haydock%element_1D(i_g)) cycle
   call PP_redux_wait(Vo(i_g)%fragment,COMM=PAR_COM_T_Haydock(i_g)%COMM)
 enddo
#endif
 !
end subroutine
