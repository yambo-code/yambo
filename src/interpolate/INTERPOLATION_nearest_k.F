!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,           ONLY:SP
 use R_lattice,      ONLY:k_the_nearest
 use interpolate,    ONLY:NN_nk_exact,NN_n_of_nearest,NN_MAX_real_nearest
 use vec_operate,    ONLY:sort
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
 integer :: ik,Nk_in,Nk_out,i_err,Nk_to_average,ik_n,D1,D3,i1,i2,i3
 integer,  allocatable :: k_near(:)
 real(SP), allocatable :: k_dist(:)
 !
 ! Dimensions & Allocations
 !
 Nk_in =size(K_in(:,1))
 Nk_out=size(K_out(:,1))
 D1    =size(V_out(:,1,1))
 D3    =size(V_out(1,1,:))
 allocate(k_near(Nk_in))
 allocate(k_dist(Nk_in))
 NN_nk_exact=0
 NN_MAX_real_nearest=0
 V_out=0._SP
 !
 do ik=1,Nk_out
   !
   Nk_to_average=1
   !
   k_near(1)=k_the_nearest(K_out(ik,:),K_in,Nk_in,k_dist=k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (i_err==0) NN_nk_exact=NN_nk_exact+1
   !
   if (NN_n_of_nearest>1) then
     !
     call sort(k_dist,indx=k_near)
     !
     do ik_n=2,Nk_in
       if ( abs(k_dist(ik_n)-k_dist(1))>abs(k_dist(1)) .or. Nk_to_average==NN_n_of_nearest ) exit
       Nk_to_average=Nk_to_average+1
     enddo
   endif
   !
   NN_MAX_real_nearest=max(NN_MAX_real_nearest,Nk_to_average)
   !
   ! Transfer & interpolate
   !
   do i2=1,Nk_to_average
     !
     ik_n=k_near(i2)
     !
     do i1=1,D1
       do i3=1,D3
         !
         V_out(i1,ik,i3)=V_out(i1,ik,i3)+V_in(i1,ik_n,i3)/real(Nk_to_average)
         !
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
 deallocate(k_near)
 deallocate(k_dist)
 !
end subroutine INTERPOLATION_nearest_k
