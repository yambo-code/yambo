!
!        Copyright (C) 2000-2021 the YAMBO team
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
 ! Given K distributed on the processors, and Vi distributed on the processors
 ! multiply K by Vi.
 !
 ! See mod_BS for details on the K structure
 ! 
 use pars,           ONLY:SP,cONE,cZERO,cI
 use parallel_int,   ONLY:PP_redux_wait
 use parallel_m,     ONLY:PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS_solvers,     ONLY:Haydock_vector,BSS_Slepc_double_grp
 use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp,l_BS_ares_from_res
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 integer                ::iter_par
 type(Haydock_vector)   ::Vi(BS_nT_grps),Vo(BS_nT_grps)
 !
 ! Work Space
 !
 integer         ::i_B,i_Tk,i_Tp,i_g,i_ColFG,ColsFG_Tk,ColsFG_Tp
 complex(SP)     ::alpha
 ! 
 if(.not.BSS_Slepc_double_grp) alpha=-real(iter_par,SP)*cI
 if(     BSS_Slepc_double_grp) alpha=-real(iter_par,SP)
 !
 forall(i_g=1:BS_nT_grps,PAR_IND_T_Haydock%element_1D(i_g)) Vo(i_g)%fragment = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   ColsFG_Tk = size(Vi(i_Tk)%fragment,2)
   ColsFG_Tp = size(Vi(i_Tp)%fragment,2)
   !
   select case(BS_blk(i_B)%mode)
   case("R")
     !
     ! Resonant part 
     !
     do i_ColFG=1,ColsFG_Tk
       if ( i_ColFG .gt. ColsFG_Tp ) cycle
       call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
       &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
       &                        Vi(i_Tp)%fragment(:,i_ColFG),1,cONE,&
       &                        Vo(i_Tk)%fragment(:,i_ColFG),1)
     enddo
     !
     ! Resonant part is hermitian
     !
     if (i_Tk.ne.i_Tp) then
       do i_ColFG=1,ColsFG_Tp
         if ( i_ColFG .gt. ColsFG_Tk ) cycle
         call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
         &                        BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                        Vi(i_Tk)%fragment(:,i_ColFG),1,cONE,&
         &                        Vo(i_Tp)%fragment(:,i_ColFG),1)
       enddo
     end if
     !
     ! Add the energy diffs on the diagonal
     !
     if (i_Tk.eq.i_Tp) then
       do i_ColFG=1,ColsFG_Tk
         Vo(i_Tk)%fragment(:,i_ColFG) = Vo(i_Tk)%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi(i_Tk)%fragment(:,i_ColFG)
       enddo
     end if
     !
   case("C")
     !
     if (l_BS_ares_from_res) then
       !
       ! Coupling part
       !
       do i_ColFG=1,ColsFG_Tk
         if ( i_ColFG .gt. ColsFG_Tp ) cycle
         call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
         &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                       conjg(Vi(i_Tp)%fragment(:,i_ColFG)),1,cONE,&
         &                             Vo(i_Tk)%fragment(:,i_ColFG),1)
       enddo
       !
       ! Coupling part can be symmetric
       !
       if ( i_Tk.ne.i_Tp ) then
         do i_ColFG=1,ColsFG_Tp
           if ( i_ColFG .gt. ColsFG_Tk ) cycle
           call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
           &                   BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
           &                   conjg(Vi(i_Tk)%fragment(:,i_ColFG)),1,cONE,&
           &                         Vo(i_Tp)%fragment(:,i_ColFG),1)
         enddo
       end if
       !
       !
       !
     else
       !
       ! Coupling part
       !
       do i_ColFG=1,ColsFG_Tk
         if ( i_ColFG .gt. ColsFG_Tp ) cycle
         call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
         &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                       Vi(i_Tp)%fragment(:,i_ColFG),1,cONE,&
         &                       Vo(i_Tk)%fragment(:,i_ColFG),1)
       enddo
       !
       ! Anti-coupling Q = -conjg(C)
       ! 
       do i_ColFG=1,ColsFG_Tp
         if ( i_ColFG .gt. ColsFG_Tk ) cycle
         call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),-cONE,&
         &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                       Vi(i_Tk)%fragment(:,i_ColFG),1,cONE,&
         &                       Vo(i_Tp)%fragment(:,i_ColFG),1)
       enddo
       !
     endif
     !
     !
   case("A")
     !
     ! Anti-Resonant part 
     !
     do i_ColFG=1,ColsFG_Tk
       if ( i_ColFG .gt. ColsFG_Tp ) cycle
         call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
         &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                       Vi(i_Tp)%fragment(:,i_ColFG),1,cONE,&
         &                       Vo(i_Tk)%fragment(:,i_ColFG),1)
     enddo
     !
     ! Anti-Resonant part is hermitian
     !
     if (i_Tk.ne.i_Tp) then
       do i_ColFG=1,ColsFG_Tp
         if ( i_ColFG .gt. ColsFG_Tk ) cycle
         call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
         &                       BS_blk(i_B)%mat,BS_blk(i_B)%size(1),&
         &                       Vi(i_Tk)%fragment(:,i_ColFG),1,cONE,&
         &                       Vo(i_Tp)%fragment(:,i_ColFG),1)
       enddo
     end if
     !
     ! Add the energy diffs on the diagonal
     !
     if (i_Tk.eq.i_Tp) then
       do i_ColFG=1,ColsFG_Tk
         Vo(i_Tk)%fragment(:,i_ColFG) = Vo(i_Tk)%fragment(:,i_ColFG) + BS_T_grp(i_Tk)%E(:,i_ColFG)*Vi(i_Tk)%fragment(:,i_ColFG)
       enddo
     end if
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
