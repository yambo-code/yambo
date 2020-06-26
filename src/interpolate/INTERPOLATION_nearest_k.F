!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine INTERPOLATION_nearest_k(K_in,V_in,K_out,V_out)
 !
 use pars,              ONLY:SP
 use R_lattice,         ONLY:k_the_nearest
 use interpolate,       ONLY:NN_nk_exact,NN_n_of_nearest,NN_MAX_real_nearest,INTERPOLATE_is_serial
 use interfaces,        ONLY:PARALLEL_index
 use vec_operate,       ONLY:sort
 use parallel_m,        ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,ncpu,myid
 !
#include<memory.h>
 !
 real(SP)                :: K_in(:,:)
 real(SP)                :: K_out(:,:)
 real(SP)                :: V_in(:,:,:)
 real(SP)                :: V_out(:,:,:)
 !
 ! Work Space
 ! 
 integer :: ik,Nk_in,Nk_out,i_err,Nk_to_average,ik_n,D1,D3,i1,i2,i3,NN_MAX(ncpu),NN_exact(ncpu)
 integer,  allocatable :: k_near(:)
 real(SP), allocatable :: k_dist(:)
 real(SP)              :: total_dist
 type(PP_indexes)      :: px
 !
 ! Dimensions & Allocations
 !
 Nk_in =size(K_in(:,1))
 Nk_out=size(K_out(:,1))
 D1    =size(V_out(:,1,1))
 D3    =size(V_out(1,1,:))
 allocate(k_near(Nk_in))
 allocate(k_dist(Nk_in))
 NN_exact=0
 NN_MAX=0
 V_out=0._SP
 !
 if (.not.INTERPOLATE_is_serial) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/Nk_out/))
 endif
 !
 do ik=1,Nk_out
   !
   if (.not.INTERPOLATE_is_serial) then
     if (.not.px%element_1D(ik)) cycle
   endif
   !
   Nk_to_average=1
   !
   k_near(1)=k_the_nearest(K_out(ik,:),K_in,Nk_in,k_dist=k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (i_err==0) then
     NN_exact(myid+1)=NN_exact(myid+1)+1
     total_dist=1.
     k_dist(1)=1.
   else if (NN_n_of_nearest>1) then
     call sort(k_dist,indx=k_near)
     total_dist=0._SP
     do ik_n=2,Nk_in
       if ( abs(k_dist(ik_n)-k_dist(1))>abs(k_dist(1)) .or. Nk_to_average==NN_n_of_nearest ) exit
       Nk_to_average=Nk_to_average+1
       total_dist=total_dist+k_dist(ik_n)
     enddo
     if (Nk_to_average==1) then
       total_dist=1.
       k_dist(1)=1.
     endif
   else
     total_dist=1.
     k_dist(1)=1.
   endif
   !
   NN_MAX(myid+1)=max(NN_MAX(myid+1),Nk_to_average)
   !
   do i2=1,Nk_to_average
     !
     ik_n=k_near(i2)
     !
     do i1=1,D1
       do i3=1,D3
         !
         V_out(i1,ik,i3)=V_out(i1,ik,i3)+V_in(i1,ik_n,i3)*k_dist(i2)/total_dist
         !
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 if (.not.INTERPOLATE_is_serial) then
   call PP_redux_wait(V_out)
   call PP_redux_wait(NN_MAX)
   call PP_redux_wait(NN_exact)
   NN_MAX_real_nearest=sum(NN_max)
   NN_nk_exact=NN_nk_exact+sum(NN_exact)
 else
   NN_MAX_real_nearest=NN_max(myid+1)
   NN_nk_exact=NN_nk_exact+NN_exact(myid+1)
 endif
 !
 deallocate(k_near)
 deallocate(k_dist)
 !
end subroutine INTERPOLATION_nearest_k
