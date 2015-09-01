!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine K_multiply_by_V(iter,Vi,Vo,iter_par,call_kind)
 !
 ! Given (Coupling disabled)
 !
 !      | (K_r)     (K_c)    |  
 !  K = |                    |
 !      | (-K_c^*)  (-K_r^*) |
 !
 ! distibuted on the processors, and Vi distributed on the processors
 !
 ! Multiply K by Vi
 ! 
 !
 use pars,           ONLY:SP,cONE,cZERO
 use com,            ONLY:error
 use parallel_m,     ONLY:PP_redux_wait,PAR_IND_T_Haydock,PAR_COM_T_Haydock
 use BS,             ONLY:BS_nT_grps,Haydock_vector,BSS_uses_DbGd,BS_blk,n_BS_blks,BS_T_grp
 use wrapper,        ONLY:M_by_V
 !
 implicit none
 !
 integer                ::iter
 type(Haydock_vector)   ::Vi(:),Vo(:)
 real(SP),optional      ::iter_par
 character(3) ,optional ::call_kind
 !
 ! Work Space
 !
 logical         ::l_inv,l_Hay
 integer         ::i_B,i_Tk,i_Tp,i_g
 complex(SP)     ::alpha
 ! 
 !
 l_Hay= (trim(call_kind)=='Hay')
 l_inv= (trim(call_kind)=='Inv')
 !
 ! Messaging
 !
 if (l_inv) call error('K_multiply_by_V: Inv temporary disabled.') 
 if (BSS_uses_DbGd)  call error('K_multiply_by_V: BSS with Double Grid temporary disabled')
 if( (.not.l_inv) .and. (.not.l_Hay) ) call error('K_multiply_by_V: not a valid call.')
 !
 alpha=cONE
 if (present(iter_par)) alpha = iter_par*cONE 
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
