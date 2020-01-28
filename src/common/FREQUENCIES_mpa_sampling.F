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
   complex(DP), intent(in)    :: w1,w2    ! segment [w1,w2)
   character,   intent(in)    :: ps       ! flavour of sampling: hPPS, qPPS, cPPS, ...
   complex(DP), intent(out)   :: w(2*np)
   !
   integer     :: i, a1, a2
   complex(DP) :: ws
   real(DP)    :: w_aux1, w_aux2
   real(DP), parameter :: log2=0.693147180560_DP
   !
   ws = w1+w2
   select case(np)
     case (1)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, 0.5_DP*ws/)
         case ('q') !QPPS
           w = (/w1, 0.25_DP*ws/)
         case ('c') !CPPS
           w = (/w1, 0.125_DP*ws/)
         case default
           ! error 
       end select
     case (2)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, 0.25_DP*ws, 0.5_DP*ws, 0.75_DP*ws/)
         case ('q') !QPPS
           w = (/w1, 0.0625_DP*ws, 0.25_DP*ws, 0.5625_DP*ws/)
         case ('c') !CPPS
           w = (/w1, 0.015625_DP*ws, 0.125_DP*ws, 0.421875_DP*ws/)
         case default
           ! error 
       end select
     case (3)
       select case(ps)
         case ('h') !HPPS
           w = (/w1, 0.25_DP*ws, 0.375_DP*ws, 0.5_DP*ws, 0.625_DP*ws, 0.75_DP*ws/)
         case ('q') !QPPS
           w = (/w1, 0.0625_DP*ws, 0.140625_DP*ws, 0.25_DP*ws, 0.390625_DP*ws, 0.5625_DP*ws/)
         case ('c') !CPPS
           w = (/w1, 0.015625_DP*ws, 0.0527344_DP*ws, 0.125_DP*ws, 0.244141_DP*ws, 0.421875_DP*ws/)
         case default
           ! error 
       end select
     case default 
      w(1) = w1 
      select case(ps)
        case ('h') !HPPS
          w(np+1) = 0.5_DP*ws 
        case ('q') !QPPS
          w(np+1) = 0.25_DP*ws 
        case ('c') !CPPS
          w(np+1) = 0.15_DP*ws 
        case default
          ! error 
      end select  
      do i=2, np 
        a1 = 2**(floor(log(i+0._DP)/log2)+1) -i
        a2 = 2**(floor(log(i-1._DP)/log2))
        w_aux1 = ( 2._DP*mod(a1,a2) +1._DP)/(4._DP*a2)
        w_aux2 = 1._DP - w_aux1        
        select case(ps)
          case ('h') !HPPS
            w(i)        = w_aux1*ws
            w(2*np-i+2) = w_aux2*ws
          case ('q') !QPPS
            w(i)        = w_aux1**2*ws
            w(2*np-i+2) = w_aux2**2*ws
          case ('c') !CPPS
            w(i)        = w_aux1**3*ws
            w(2*np-i+2) = w_aux2**3*ws
          case default
            ! error 
        end select
      enddo
   end select 
   !
end subroutine FREQUENCIES_mpa_sampling

