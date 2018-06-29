!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, AC
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
subroutine Build_DIP_bz(Xk,Xen,X)
 !
 use pars,           ONLY:cZERO
 use R_lattice,      ONLY:bz_samp,nXkbz
 use electrons,      ONLY:levels,n_sp_pol
 use X_m,            ONLY:X_t,DIP_iR
 use D_lattice,      ONLY:dl_sop,sop_inv
 use nl_optics,      ONLY:DIP_bz
 use parallel_m,     ONLY:PAR_Xk_ibz_index,PAR_IND_WF_k,PP_redux_wait,PAR_COM_Xk_ibz_INDEX
 !
 implicit none
 !
 type(bz_samp), intent(in) :: Xk
 type(levels),  intent(in) :: Xen
 type(X_t),     intent(in) :: X
 !
 ! Work Space
 !
 integer                   :: ik,ik_bz,ic,iv,is,i_sp_pol,i_star,id
 !
 DIP_bz=cZERO
 !
 ! I do not consider time-reversal symmetry because in real-time
 ! when an external field is present time-reversal is broken
 ! to the norm of the grid shift.
 !
 do i_sp_pol=1,n_sp_pol
   !
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_WF_k%element_1D(ik)) cycle
     ! 
     do iv=X%ib(1),X%ib_lim(1) 
       do ic=X%ib_lim(2),X%ib(2)
         !
         do i_star=1,Xk%nstar(ik)
           !
           is                = sop_inv(Xk%star(ik,i_star))
           ik_bz             = i_star
           !
           if ( ik>1 ) ik_bz = sum(Xk%nstar(1:ik-1))+i_star
           !
           do id=1,3
             DIP_bz(id,ic,iv,ik_bz,i_sp_pol) = sum(dl_sop(id,:,is)*DIP_iR(:,ic,iv,PAR_Xk_ibz_index(ik),i_sp_pol))
           enddo
           !
         enddo  ! i_star
         !  
       enddo  ! ic
     enddo ! iv
   enddo ! ik
   call PP_redux_wait(DIP_bz(:,:,:,:,i_sp_pol),COMM=PAR_COM_Xk_ibz_INDEX%COMM ) 
 enddo  ! i_sp_pol
 !
end subroutine Build_DIP_bz
