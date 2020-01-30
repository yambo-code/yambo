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

 subroutine FREQUENCIES_mpa_sampling(np, w1, w2, ps, w) 
   use pars,     ONLY:SP, DP
   implicit none
   !
   integer,     intent(in)    :: np       ! number of desired pairs
   complex(SP), intent(in)    :: w1,w2    ! segment [w1,w2)
   character,   intent(in)    :: ps       ! flavour of sampling: hPPS, qPPS, cPPS, ...
   complex(SP), intent(out)   :: w(2*np)
   !
   integer     :: i, a1, a2
   complex(SP) :: ws
   real(SP)    :: w_aux1, w_aux2
   real(SP), parameter :: log2=0.693147180560_SP
   !
   ws = w2-w1
   select case(np)
     case (1)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, w1+0.5_SP*ws/)
         case ('q') !QPPS
           w = (/w1, w1+0.25_SP*ws/)
         case ('c') !CPPS
           w = (/w1, w1+0.125_SP*ws/)
         case default
           ! error 
       end select
     case (2)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, w1+0.25_SP*ws, w1+0.5_SP*ws, w1+0.75_SP*ws/)
         case ('q') !QPPS
           w = (/w1, w1+0.0625_SP*ws, w1+0.25_SP*ws, w1+0.5625_SP*ws/)
         case ('c') !CPPS
           w = (/w1, w1+0.015625_SP*ws, w1+0.125_SP*ws, w1+0.421875_SP*ws/)
         case default
           ! error 
       end select
     case (3)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, w1+0.25_SP*ws, w1+0.375_SP*ws, w1+0.5_SP*ws, w1+0.625_SP*ws, w1+0.75_SP*ws/)
         case ('q') !QPPS
           w = (/w1, w1+0.0625_SP*ws, w1+0.140625_SP*ws, w1+0.25_SP*ws, w1+0.390625_SP*ws, w1+0.5625_SP*ws/)
         case ('c') !CPPS
           w = (/w1, w1+0.015625_SP*ws, w1+0.0527344_SP*ws, w1+0.125_SP*ws, w1+0.244141_SP*ws, w1+0.421875_SP*ws/)
         case default
           ! error 
       end select
     case default 
      w(1) = w1 
      select case(ps)
        case ('h') !HPPS
          w(np+1) = w1+0.5_SP*ws 
        case ('q') !QPPS
          w(np+1) = w1+0.25_SP*ws 
        case ('c') !CPPS
          w(np+1) = w1+0.15_SP*ws 
        case default
          ! error 
      end select  
      do i=2, 2*np 
        w(i)=w1+ws*(i-1)/(2*np) 
      enddo
   end select 
   !
end subroutine FREQUENCIES_mpa_sampling

