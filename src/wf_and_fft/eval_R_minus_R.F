!
! License-Identifier: GPL
!
! Copyright (C) 2011 The Yambo Team
!
! Authors (see AUTHORS file for details): MG
!
subroutine eval_R_minus_R()
 !
 ! Evaluate the R_i-R_j table
 ! 
 use FFT_m,     ONLY:fft_size,fft_dim,modx
 use D_lattice, ONLY:R_m_R
#include<memory.h>
 !
 ! Work Space
 ! 
 integer ii,jj,kk,ivec(3),jvec(3),kvec(3),idum
 !
 ! Allocation
 !
 if (.not.allocated(R_m_R)) then 
   YAMBO_ALLOC(R_m_R,(fft_size,fft_size))
 end if
 !
 ! Loop
 !
 do ii = 1, fft_size
   do jj = 1, fft_size
     idum = ii
     call MapVec2Indx('r',idum,ivec)
     idum = jj
     call MapVec2Indx('r',idum,jvec)
     kvec = ivec - jvec
     call MapVec2Indx('d',kk,kvec)
     R_m_R(ii,jj) = kk
   end do
 end do
 !
 contains
   !
   subroutine MapVec2Indx(direx,ii,V)
     !
     ! Map a vector V(3) of integers in a 
     ! single index ii using
     !
     ! ii = 1 + modx(V(1),fft_dim(1))+
     ! modx(V(2),fft_dim(2))*fft_dim(1)+
     ! modx(V(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
     ! 
     ! (modx as in fft_setup) and viceversa using
     !
     ! V(1) = mod(ii-1,fft_dim(1))
     ! V(2) = mod(jj,fft_dim(2))
     ! V(3) = jj/fft_dim(2)
     !
     ! where jj = (ii-1)/fft_dim(1)
     !
     implicit none
     integer, intent(inout):: ii
     integer, intent(inout):: V(3)
     character(1), intent(in)::direx
     !
     ! Work Space
     !
     integer :: jj
     !
     if (direx=='d') &     ! DIRECT MAPPING
&      ii = 1 + modx(V(1),fft_dim(1))   +&
&      modx(V(2),fft_dim(2))*fft_dim(1) +&
&      modx(V(3),fft_dim(3))*fft_dim(1)*fft_dim(2)
     if (direx=='r') then  ! REVERSE MAPPING
       V(1) = mod(ii-1,fft_dim(1))
       jj   = (ii-1)/fft_dim(1)
       V(2) = mod(jj,fft_dim(2))
       V(3) = jj/fft_dim(2)
     end if
     !
   end subroutine MapVec2Indx
   !
 end subroutine eval_R_minus_R
 
