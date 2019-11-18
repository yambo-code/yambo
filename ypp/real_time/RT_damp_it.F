!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM CA
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
subroutine RT_damp_it(damp_type,damp_factor,ft,lda)
 !
 use YPP_real_time,  ONLY:RT_time,RT_conf
 use pars,           ONLY:SP,DP
 !
 implicit none
 !
 integer,      intent(in)      :: lda
 real(SP),     intent(in)      :: damp_factor
 complex(DP),  intent(inout)   :: ft(RT_conf%T_n_steps,lda)
 character(*), intent(in)      :: damp_type
 !                          
 ! Work Space 
 !
 integer  :: i_t
 real(SP) :: damp_function(RT_conf%T_n_steps)
 !
 select case ( trim(damp_type) )
   case("LORENTZIAN")  
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-abs(RT_time(i_t)*damp_factor))
   case("GAUSSIAN") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-RT_time(i_t)**2*damp_factor**2)
   case("NONE") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = 1._SP
   case default
     call warning(" Unknown damping! ")
     return
 end select
 !
 forall(i_t=1:RT_conf%T_n_steps)  ft(i_t,:)=ft(i_t,:)*damp_function(i_t)
 !
end subroutine RT_damp_it

