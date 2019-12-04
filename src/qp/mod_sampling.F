!--------------------------------
!..   Fortran .f95 MODULE      ..
!--------------------------------

!---------------------------------------------------------------
! Pair sampling of a positive unidimentional (frequency) domain 
! as a function of a parameter (the scale)
! 
!                     to be used in the GW approximation
! by Dario A. Leon
!---------------------------------------------------------------

!---------------------------------------------------------------
! Notes:

!   1) Homogeneous Partition Pair Sampling (HPPS)
!   2) Quadratic Partition Pair Sampling (QPPS)
!   3) Cubic Partition Pair Sampling (CPPS)
!   4) ...
!   5) ...

! *The samplings do not depend on the sampled function 
!---------------------------------------------------------------


MODULE mod_sampling

 use pars,          ONLY:SP,DP

IMPLICIT NONE

CONTAINS

!!----------------------------------------------------------

 subroutine HPPS(n, ws, w) 

   integer,    intent(in)      :: n      ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(:)

   integer     :: a1, a2

   if(n == 1) then
     w(1) = 0.
     w(2) = ws/2.
     else
       a1 = 2**(floor(log(n+0.)/log(2.))+1) -n
       a2 = 2**(floor(log(n-1.)/log(2.)))
       w(2*n-1) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
       w(2*n)   = 1. - w(2*n-1)
      !HPPS
       w(2*n-1) = w(2*n-1)*ws
       w(2*n)   = w(2*n  )*ws
   endif

 end subroutine HPPS
!!----------------------------------------------------------

 subroutine HPPS_all(np, ws, w) 

   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)

   integer     :: i, a1, a2

  select case(np)
    case (1)
      w = (/0.*ws, 0.5*ws/)
    case (2)
      w = (/0.*ws, 0.25*ws, 0.5*ws, 0.75*ws/)
    case (3)
      w = (/0.*ws, 0.25*ws, 0.375*ws, 0.5*ws, 0.625*ws, 0.75*ws/)
    case default 
      w(1) = 0.   
      w(np+1) = 0.5  
      do i=2, np 
        a1 = 2**(floor(log(i+0.)/log(2.))+1) -i
        a2 = 2**(floor(log(i-1.)/log(2.)))
        w(i) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
        w(2*np-i+2) = 1. - w(i)
       !HPPS
        w(i)        = w(i)*ws
        w(2*np-i+2) = w(2*np-i+2)*ws
      enddo
  endselect 

 end subroutine HPPS_all
!!----------------------------------------------------------

 subroutine QPPS(n, ws, w) 

   integer,    intent(in)      :: n      ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(:)

   integer     :: a1, a2

   if(n == 1) then
     w(1) = 0.
     w(2) = ws/4.
     else
       a1 = 2**(floor(log(n+0.)/log(2.))+1) -n
       a2 = 2**(floor(log(n-1.)/log(2.)))
       w(2*n-1) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
       w(2*n)   = 1.- w(2*n-1)
      !QPPS
       w(2*n-1) = w(2*n-1)**2.*ws
       w(2*n)   = w(2*n  )**2.*ws
   endif

 end subroutine QPPS
!!----------------------------------------------------------

 subroutine QPPS_all(np, ws, w) 

   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)

   integer     :: i, a1, a2

  select case(np)
    case (1)
      w = (/0._SP, 0.25*ws/)
    case (2)
      w = (/0._SP, 0.0625*ws, 0.25*ws, 0.5625*ws/)
    case (3)
      w = (/0._SP, 0.0625*ws, 0.140625*ws, 0.25*ws, 0.390625*ws, 0.5625*ws/)
    case default 
      w(1) = 0._SP   
      w(np+1) = 0.25  
      do i=2, np 
        a1 = 2**(floor(log(i+0.)/log(2.))+1) -i
        a2 = 2**(floor(log(i-1.)/log(2.)))
        w(i) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
        w(2*np-i+2) = 1. - w(i)
       !QPPS
        w(i)        = w(i)**2.*ws
        w(2*np-i+2) = w(2*np-i+2)**2.*ws
      enddo
  endselect 

 end subroutine QPPS_all
!!----------------------------------------------------------

 subroutine CPPS(n, ws, w) 

   integer,    intent(in)      :: n      ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(:)

   integer     :: a1, a2

   if(n == 1) then
     w(1) = 0.
     w(2) = ws/8.
     else
       a1 = 2**(floor(log(n+0.)/log(2.))+1) -n
       a2 = 2**(floor(log(n-1.)/log(2.)))
       w(2*n-1) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
       w(2*n)   = 1. - w(2*n-1)
      !QPPS
       w(2*n-1) = w(2*n-1)**3.*ws
       w(2*n)   = w(2*n  )**3.*ws
   endif

 end subroutine CPPS
!!----------------------------------------------------------

 subroutine CPPS_all(np, ws, w) 

   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)

   integer     :: i, a1, a2

  select case(np)
    case (1)
      w = (/0._SP, 0.125*ws/)
    case (2)
      w = (/0._SP, 0.015625*ws, 0.125*ws, 0.421875*ws/)
    case (3)
      w = (/0._SP, 0.015625*ws, 0.0527344*ws, 0.125*ws, 0.244141*ws, 0.421875*ws/)
    case default 
      w(1) = 0.   
      w(np+1) = 0.125  
      do i=2, np 
        a1 = 2**(floor(log(i+0.)/log(2.))+1) -i
        a2 = 2**(floor(log(i-1.)/log(2.)))
        w(i) = ( 2.*mod(a1,a2) +1. )/(4.*a2)
        w(2*np-i+2) = 1. - w(i)
       !QPPS
        w(i)        = w(i)**3.*ws
        w(2*np-i+2) = w(2*np-i+2)**3.*ws
      enddo
  endselect 

 end subroutine CPPS_all
!!----------------------------------------------------------

END MODULE mod_sampling