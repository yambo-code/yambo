!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DALV, AF
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
!
!---------------------------------------------------------------
! Pair sampling of a positive unidimentional (frequency) domain 
! as a function of a parameter (the scale)
! 
!                     to be used in the GW approximation
! by Dario A. Leon
!
! Notes:
!
!   1) Homogeneous Partition Pair Sampling (HPPS)
!   2) Quadratic Partition Pair Sampling (QPPS)
!   3) Cubic Partition Pair Sampling (CPPS)
!   4) ...
!   5) ...
!
! *The samplings do not depend on the sampled function 
!---------------------------------------------------------------

 subroutine X_mpa_sampling_HPPS(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: a1, a2
   !
   if(np == 1) then
     w(1) = 0._SP
     w(2) = ws/2._SP
   else
     a1 = 2**(floor(log(np+0._SP)/log(2._SP))+1) -np
     a2 = 2**(floor(log(np-1._SP)/log(2._SP)))
     w(2*np-1) = ( 2._SP*mod(a1,a2) +1._SP )/(4._SP*a2)
     w(2*np)   = 1._SP -w(2*np-1)
     ! HPPS
     w(2*np-1) = w(2*np-1)*ws
     w(2*np)   = w(2*np  )*ws
   endif
   !
 end subroutine X_mpa_sampling_HPPS

 subroutine X_mpa_sampling_HPPS_all(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: i, a1, a2
   !
   select case(np)
   case (1)
      w = (/0.0_SP*ws, 0.5_SP*ws/)
   case (2)
      w = (/0.0_SP*ws, 0.25_SP*ws, 0.5_SP*ws, 0.75_SP*ws/)
   case (3)
      w = (/0.0_SP*ws, 0.25_SP*ws, 0.375_SP*ws, 0.5_SP*ws, 0.625_SP*ws, 0.75_SP*ws/)
   case default 
      w(1) = 0.0_SP   
      w(np+1) = 0.5_SP 
      do i=2, np 
        a1 = 2**(floor(log(i+0._SP)/log(2._SP))+1) -i
        a2 = 2**(floor(log(i-1._SP)/log(2._SP)))
        w(i) = ( 2._SP*mod(a1,a2) +1._SP)/(4._SP*a2)
        w(2*np-i+2) = 1._SP - w(i)
        !HPPS
        w(i)        = w(i)*ws
        w(2*np-i+2) = w(2*np-i+2)*ws
      enddo
   end select 
   !
 end subroutine X_mpa_sampling_HPPS_all

 subroutine X_mpa_sampling_QPPS(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: a1, a2
   !
   if(np == 1) then
     w(1) = 0._SP
     w(2) = ws/4._SP
   else
     a1 = 2**(floor(log(np+0._SP)/log(2._SP))+1) -np
     a2 = 2**(floor(log(np-1._SP)/log(2._SP)))
     w(2*np-1) = ( 2._SP*mod(a1,a2) +1._SP )/(4._SP*a2)
     w(2*np)   = 1._SP- w(2*np-1)
     !QPPS
     w(2*np-1) = w(2*np-1)**2*ws
     w(2*np)   = w(2*np  )**2*ws
   endif
   !
 end subroutine X_mpa_sampling_QPPS

 subroutine X_mpa_sampling_QPPS_all(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: i, a1, a2
   !
   select case(np)
     case (1)
       w = (/0._SP, 0.25_SP*ws/)
     case (2)
       w = (/0._SP, 0.0625_SP*ws, 0.25_SP*ws, 0.5625_SP*ws/)
     case (3)
       w = (/0._SP, 0.0625_SP*ws, 0.140625_SP*ws, 0.25_SP*ws, 0.390625_SP*ws, 0.5625_SP*ws/)
     case default 
       w(1) = 0._SP   
       w(np+1) = 0.25  
       do i=2, np 
         a1 = 2**(floor(log(i+0._SP)/log(2._SP))+1) -i
         a2 = 2**(floor(log(i-1._SP)/log(2._SP)))
         w(i) = ( 2._SP*mod(a1,a2) +1._SP )/(4._SP*a2)
         w(2*np-i+2) = 1._SP - w(i)
         !QPPS
         w(i)        = w(i)**2*ws
         w(2*np-i+2) = w(2*np-i+2)**2*ws
       enddo
   end select 
   !
 end subroutine X_mpa_sampling_QPPS_all

 subroutine X_mpa_sampling_CPPS(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! last pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: a1, a2
   !
   if(np == 1) then
     w(1) = 0._SP
     w(2) = ws/8._SP
   else
     a1 = 2**(floor(log(np+0._SP)/log(2._SP))+1) -np
     a2 = 2**(floor(log(np-1._SP)/log(2._SP)))
     w(2*np-1) = ( 2._SP*mod(a1,a2) +1._SP )/(4._SP*a2)
     w(2*np)   = 1._SP - w(2*np-1)
     !QPPS
     w(2*np-1) = w(2*np-1)**3*ws
     w(2*np)   = w(2*np  )**3*ws
   endif
   !
 end subroutine X_mpa_sampling_CPPS

 subroutine X_mpa_sampling_CPPS_all(np, ws, w) 
   use pars,     ONLY:SP
   implicit none
   !
   integer,    intent(in)      :: np     ! number of desired pairs
   real(SP),   intent(in)      :: ws     ! scale
   real(SP),   intent(inout)   :: w(2*np)
   integer     :: i, a1, a2
   !
   select case(np)
   case (1)
      w = (/0._SP, 0.125_SP*ws/)
   case (2)
      w = (/0._SP, 0.015625_SP*ws, 0.125_SP*ws, 0.421875_SP*ws/)
   case (3)
      w = (/0._SP, 0.015625_SP*ws, 0.0527344_SP*ws, 0.125_SP*ws, 0.244141_SP*ws, 0.421875_SP*ws/)
   case default 
      w(1) = 0.0_SP   
      w(np+1) = 0.125_SP
      do i=2, np 
        a1 = 2**(floor(log(i+0._SP)/log(2._SP))+1) -i
        a2 = 2**(floor(log(i-1._SP)/log(2._SP)))
        w(i) = ( 2._SP*mod(a1,a2) +1._SP )/(4._SP*a2)
        w(2*np-i+2) = 1._SP - w(i)
        !QPPS
        w(i)        = w(i)**3*ws
        w(2*np-i+2) = w(2*np-i+2)**3*ws
      enddo
   end select 
   !
 end subroutine X_mpa_sampling_CPPS_all

