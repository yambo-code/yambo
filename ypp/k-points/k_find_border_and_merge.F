!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine k_find_border_and_merge(FineGd_E,FineGd_k,n_FineGd_DBs,FineGd_io_E,FineGd_io_k,Xk,l_skip_border_pts)
 !
 use pars,      ONLY:SP
 use R_lattice, ONLY:bz_samp,k_the_nearest,bz_samp_reset
 use electrons, ONLY:levels,E_reset
 use stderr,    ONLY:intc
 use com,       ONLY:msg
 !
#include<memory.h>
 !
 integer,       intent(in)     :: n_FineGd_DBs 
 type(levels),  intent(inout)  :: FineGd_io_E(n_FineGd_DBs)
 type(levels),  intent(out)    :: FineGd_E
 type(bz_samp), intent(inout)  :: FineGd_io_k(n_FineGd_DBs)
 type(bz_samp), intent(in)     :: Xk
 type(bz_samp), intent(out)    :: FineGd_k
 logical,       intent(in)     :: l_skip_border_pts
 !
 !
 ! Work Space
 !
 integer :: i_db,ik,i1,ik_near,i_fg
 logical :: l_I_am_border
 integer, allocatable :: I_am_border(:)
 integer :: n_sp_pol_input
 !
 if(l_skip_border_pts) then
   !      
   allocate(I_am_border(sum(FineGd_io_E(:n_FineGd_DBs)%nk)))
   !
   I_am_border=0
   !
   ik=0
   do i_db=1,n_FineGd_DBs
     do i1=1,FineGd_io_E(i_db)%nk
       ik_near=k_the_nearest(FineGd_io_k(i_db)%pt(i1,:),Xk%ptbz,Xk%nbz,border_point=l_I_am_border)
       ik=ik+1
       I_am_border(ik)=0
       if (l_I_am_border) I_am_border(ik)=1
     enddo
   enddo
   !
   FineGd_E%nk=count(I_am_border==0)
   FineGd_k%nibz=count(I_am_border==0)
   !
   call msg('sr','Removed '//trim(intc(count(I_am_border==1)))//' border points')
   !
 else
   !
   FineGd_E%nk=sum(FineGd_io_E(:n_FineGd_DBs)%nk)
   FineGd_k%nibz=sum(FineGd_io_E(:n_FineGd_DBs)%nk)
   !    
 endif
 !
 ! ...merge of ALL FineGd_io_E(:) and FineGd_io_k(:) into FineGd_E and FineGd_k
 !
 n_sp_pol_input=size(FineGd_io_E(1)%E(1,1,:))
 !
 FineGd_E%nb=FineGd_io_E(1)%nb
 allocate(FineGd_E%E(FineGd_E%nb,FineGd_E%nk,n_sp_pol_input))
 allocate(FineGd_k%pt(FineGd_E%nk,3))
 !
 ik=0
 i_fg=0
 do i_db=1,n_FineGd_DBs
   do i1=1,FineGd_io_E(i_db)%nk
     i_fg=i_fg+1
     if (l_skip_border_pts) then
       if (I_am_border(i_fg)==1) cycle
     endif
     ik=ik+1
     FineGd_E%E(:,ik,:)= FineGd_io_E(i_db)%E(:,i1,:)
     FineGd_k%pt(ik,:)= FineGd_io_k(i_db)%pt(i1,:)
   enddo
 enddo
 !
 ! ...clean after the i_db loop due [AM Jul 2020].
 !
 do i_db=1,n_FineGd_DBs
   call E_reset(FineGd_io_E(i_db))
   call bz_samp_reset(FineGd_io_k(i_db))
 enddo
 !
end subroutine
