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
!                     to be used in the MPA approximation
! by Dario A. Leon
!
! Notes:
!
!   1) Homogeneous (homo)
!   1) Linear Partition Pair Sampling (LPPS)
!   2) Quadratic Partition Pair Sampling (QPPS)
!   3) Cubic Partition Pair Sampling (CPPS)
!   5) ...
!
! *The samplings do not depend on the sampled function 
!---------------------------------------------------------------

 subroutine FREQUENCIES_mpa_sampling(np, w1, w2, ps, gs, w) 
   use pars,     ONLY:SP,DP,cZERO,cI
   implicit none
   !
   integer,     intent(in)    :: np       ! number of desired frequency pairs/poles
   complex(SP), intent(in)    :: w1,w2    ! segment [w1,w2]
   character(2),   intent(in)    :: ps    ! flavour of sampling: 1line, 2lines, ...
   character(2),   intent(in)    :: gs    ! type of grid: homo, lPPS, qPPS, cPPS, ...
   complex(SP), intent(out)   :: w(2*np)
   !
   integer     :: i
   integer(SP) :: lp,r,c,p
   real(SP)    :: aux
   complex(SP) :: ws, uni=0.1_SP*cI
   real(SP), parameter :: log2=0.693147180560_SP
   !
   ws = w2-w1
   select case(np)
     case(1)
       w = (/w1, w2/)
     case(2)
       select case(ps)
         case('2l') !2lines
           w = (/real(w1,SP)+cZERO, real(w2,SP)+uni, w1, w2/)
         case('1l') !1line
           select case(gs)
             case ('ho') !homo
               w = (/w1, w1+0.33_SP*ws, w1+0.66_SP*ws, w2/)
             case ('lP') !lhPPS
               w = (/w1, w1+0.25_SP*ws, w1+0.5_SP*ws, w2/)
             case ('qP') !QPPS
               w = (/w1, w1+0.0625_SP*ws, w1+0.25_SP*ws, w2/)
             case ('cP') !CPPS
               w = (/w1, w1+0.015625_SP*ws, w1+0.125_SP*ws, w2/)
             case default
               ! error 
           end select
         case default
         ! error 
       end select
     case (3)
       select case(ps)
         case('2l') !2lines
           select case(gs)
             case ('ho') !homo
               w=(/real(w1,SP)+cZERO,real(w1+0.5_SP*ws)+uni,real(w2,SP)+uni,w1,w1+0.5_SP*ws,w2/)
             case ('lP') !lPPS
               w=(/real(w1,SP)+cZERO,real(w1+0.5_SP*ws)+uni,real(w2,SP)+uni,w1,w1+0.5_SP*ws,w2/)
             case ('qP') !qPPS
               w=(/real(w1,SP)+cZERO,real(w1+0.25_SP*ws)+uni,real(w2,SP)+uni,w1,w1+0.25_SP*ws,w2/)
             case ('cP') !cPPS
               w=(/real(w1,SP)+cZERO,real(w1+0.125_SP*ws)+uni,real(w2,SP)+uni,w1,w1+0.125_SP*ws,w2/)
             case default
               ! error 
           end select
         case('1l') !1line
           select case(gs)
             case ('ho') !homo
               w=(/w1,w1+0.2_SP*ws,w1+0.4_SP*ws,w1+0.6_SP*ws,w1+0.8_SP*ws,w2/)
             case ('hP') !lPPS
               w=(/w1,w1+0.0625_SP*ws,w1+0.25_SP*ws,w1+0.5_SP*ws,w1+0.75_SP*ws,w2/)
             case ('qP') !qPPS
               w=(/w1,w1+0.00390625_SP*ws,w1+0.0625_SP*ws,w1+0.25_SP*ws,w1+0.5625_SP*ws,w2/)
             case ('cP') !cPPS
               w=(/w1,w1+0.000244141_SP*ws,w1+0.015625_SP*ws,w1+0.125_SP*ws,w1+0.421875_SP*ws,w2/)
             case default
               ! error 
           end select
       end select
     case default 
       select case(ps)
         case('2l') !2lines
           select case(gs)
             case('ho') !homo
               w(np+1) = w1
               w(1)=real(w1,SP)+cZERO!+uni
               do i=2, np 
                 w(np+i)=w1+ws*(i-1)/(np-1) 
                 w(i)=real(w(np+i),SP)+uni
               enddo
             case('lP') !hPPS
               w(np+1) = w1
               w(1)=real(w1,SP)+cZERO!+uni
               w(2*np) = w2
               w(np)=real(w2,SP)+cZERO+uni
               lp=log(-1._SP+np)/log(2._SP)
               r=mod(np-1,2**lp)
               do i=2,2*r
                 w(np+i)=w1+ws*(i-1)/2._SP**(lp+1)
                 w(i)=real(w(np+i),SP)+uni 
               enddo
               !
               do i=2*r+1,np-1
                 w(np+i)=w1+ws*(i-1-r)/2._SP**(lp)
                 w(i)=real(w(np+i),SP)+uni 
               enddo
               !
           end select
         case('1l') !1line
           select case(gs)
             case('ho') !homo
               w(1) = w1
               do i=2, 2*np 
                 w(i)=w1+ws*(i-1)/(2*np-1) 
               enddo
             case('lP') !hPPS
               w(1) = w1
               w(2*np) = w2
               lp=log(-1._SP+2._SP*np)/log(2._SP)
               r=mod(2*np-1,2**lp)
               do i=2,2*r
                 w(i)=w1+ws*(i-1)/2._SP**(lp+1)
               enddo
               !
               do i=2*r+1,2*np-1
                 w(i)=w1+ws*(i-1-r)/2._SP**(lp)
               enddo
               !    
           end select
       end select
   end select 
   !
end subroutine FREQUENCIES_mpa_sampling

