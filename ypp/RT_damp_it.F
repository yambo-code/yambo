!
! Copyright (C) 2000-2011 C. Attaccalite, A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_damp_it(damp_type,damp_factor,ft,nt,dt,lda)
 !
 use pars,           ONLY:SP,DP
 use com,            ONLY:warning
 implicit none
 !
 integer,      intent(in)      :: nt,lda
 real(SP),     intent(in)      :: dt,damp_factor
 complex(DP),  intent(inout)   :: ft(nt,lda)
 character(*), intent(in)      :: damp_type
 !                          
 ! Work Space 
 !
 integer  :: i_t
 real(SP) :: damp_function(nt)
 !
 select case ( trim(damp_type) )
   case("LORENTZIAN")  
     forall(i_t=1:nt) damp_function(i_t) = exp(-abs((i_t-1)*dt)*damp_factor)
   case("GAUSSIAN") 
     forall(i_t=1:nt) damp_function(i_t) = exp(-((i_t-1)*dt)**2*damp_factor**2)
   case("NONE") 
     forall(i_t=1:nt) damp_function(i_t) = 1._SP
   case default
     call warning(" Unknown damping! ")
     return
 end select
 !
 forall(i_t=1:nt)  ft(i_t,:)=ft(i_t,:)*damp_function(i_t)
 !
end subroutine RT_damp_it

