!
!        Copyright (C) 2000-2018 the YAMBO team
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
subroutine INTERPOLATION_nearest_k(K_in,K_out,V_out,SHAPE_in,&
&                                  Kw_in,QP_in,RT_in,V_in,V1_in,SHAPE_out,Kw_out,&
&                                  SAME_SUM,N_neigh,QUIET)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:spin
 use real_time,      ONLY:RT_t
 use QP_m,           ONLY:QP_t
 use R_lattice,      ONLY:k_the_nearest
 use vec_operate,    ONLY:sort
 !
#include<memory.h>
 !
 real(SP)                :: K_in(:,:)
 real(SP)                :: K_out(:,:)
 real(SP)                :: V_out(:,:,:)
 integer                 :: SHAPE_out(2)
 !
 ! Possible IN's
 real(SP)     , optional :: V_in(:,:,:)
 real(SP)     , optional :: V1_in(:)
 type(RT_t)   , optional :: RT_in
 type(QP_t)   , optional :: QP_in
 integer      , optional :: SHAPE_in(2)
 !
 ! Weights
 real(SP)     , optional :: Kw_in(:)
 real(SP)     , optional :: Kw_out(:)
 !
 logical      , optional :: SAME_SUM,QUIET
 integer      , optional :: N_neigh
 !
 ! Work Space
 ! 
 logical :: write_message
 integer :: ik,Nk_in,Nk_out,i_err,Nk_to_average,ik_n,nk_exact,DIM,i1,i2,i3,i4
 real(SP):: VAL
 integer,  allocatable :: k_near(:)
 real(SP), allocatable :: k_dist(:)
 !
 ! Dimensions & Allocations
 !
 Nk_in =size(K_in(:,1))
 Nk_out=size(K_out(:,1))
 DIM   =size(V_out(1,1,:))
 YAMBO_ALLOC(k_near,(Nk_in))
 YAMBO_ALLOC(k_dist,(Nk_in))
 nk_exact=0
 V_out=0._SP
 !
 do ik=1,Nk_out
   !
   Nk_to_average=1
   if (present(N_neigh)) Nk_to_average=N_neigh
   !
   k_near(1)=k_the_nearest(K_out(ik,:),K_in,Nk_in,k_dist=k_dist,ROTATE_k_ref=.TRUE.,i_err=i_err)
   !
   if (i_err==0) nk_exact=nk_exact+1
   !
!   call sort(k_dist,indx=k_near)
!   !
!   do ik_n=2,Nk_in
!     if ( abs(k_dist(ik_n)-k_dist(1))>abs(k_dist(1)) ) exit
!     Nk_to_average=Nk_to_average+1
!   enddo
   Nk_to_average=1
   !
   ! Transfer & interpolate
   !
   do i2=1,Nk_to_average
     !
     ik_n=k_near(i2)
     !
     do i1=SHAPE_in(1),SHAPE_in(2)
       do i3=1,DIM
         !
         if (i1<SHAPE_out(1)) cycle
         if (i1>SHAPE_out(2)) cycle
         !
         if (present(V_in)) VAL=V_in(i1-SHAPE_in(1)+1,ik_n,i3)
         !
         if (present(RT_in)) then
           do i4=1,RT_in%nstates
             if (RT_in%table(i4,1)==i1 .and. RT_in%table(i4,3)==ik_n .and. RT_in%table(i4,4)==i3) then
               VAL=V1_in(i4)
               exit 
             endif
           enddo
         endif
         !
         if (present(QP_in)) then
           do i4=1,QP_in%n_states
             if (QP_in%table(i4,1)==i1 .and. QP_in%table(i4,3)==ik_n .and. spin(QP_in%table(i4,:))==i3) then
               if (present(V1_in)) VAL=V1_in(i4)
               exit 
             endif
           enddo
         endif
         !
         V_out(i1-SHAPE_out(1)+1,ik,i3)=V_out(i1-SHAPE_out(1)+1,ik,i3)+VAL/real(Nk_to_average)
         !
       enddo
     enddo
     !
   enddo
   !
 enddo
 !
end subroutine INTERPOLATION_nearest_k
