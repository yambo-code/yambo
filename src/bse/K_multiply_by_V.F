!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM MG
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
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS_solvers,     ONLY:Haydock_vector
 use BS,             ONLY:BS_nT_grps,BS_blk,n_BS_blks,BS_T_grp
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
 alpha=-real(iter_par,SP)*cI
 !
 forall(i_g=1:BS_nT_grps,PAR_IND_T_Haydock%element_1D(i_g)) Vo(i_g)%fragment = cZERO
 !
 do i_B=1,n_BS_blks
   !
   i_Tk  =BS_blk(i_B)%iT_k
   i_Tp  =BS_blk(i_B)%iT_p
   !
   if (BS_blk(i_B)%mode == "R") then
     !
     ! Resonant part 
     !
     call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
          &                  BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tp)%fragment(:),1,cONE,&
          &                  Vo(i_Tk)%fragment(:),1)
     if (i_Tk.ne.i_Tp) &!off-diagonal symmetrization
          &         call M_by_V('c',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),cONE,&
          &                     BS_blk(i_B)%mat,BS_blk(i_B)%size(1),Vi(i_Tk)%fragment(:),1,cONE,&
          &                     Vo(i_Tp)%fragment(:),1)
     if (i_Tk.eq.i_Tp)& !add the energy diffs on the diagonal 
          &          Vo(i_Tk)%fragment(:) = Vo(i_Tk)%fragment(:) + BS_T_grp(i_Tk)%E(:)*Vi(i_Tk)%fragment(:)
   else
     !
     ! Coupling part
     !
     call M_by_V('n',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
          &                  BS_blk(i_B)%mat,BS_blk(i_B)%size(1),conjg(Vi(i_Tp)%fragment(:)),1,cONE,&
          &                  Vo(i_Tk)%fragment(:),1)
     if (i_Tk.ne.i_Tp) &!off-diagonal symmetrization
          &         call M_by_V('t',BS_blk(i_B)%size(1),BS_blk(i_B)%size(2),alpha,&
          &                     BS_blk(i_B)%mat,BS_blk(i_B)%size(1),conjg(Vi(i_Tk)%fragment(:)),1,cONE,&
          &                     Vo(i_Tp)%fragment(:),1)
   end if
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
