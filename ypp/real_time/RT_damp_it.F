!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use stderr,         ONLY:STRING_split
 use pars,           ONLY:SP,DP,schlen,pi
 use fields,         ONLY:Efield,field_frequency
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
 integer  :: i_t,i_Probe
 real(SP) :: damp_function(RT_conf%T_n_steps),T_ref,T_0,sigma,W_0
 character(schlen)        :: field_defs(3)
 !
 i_Probe=1
 call STRING_split(Efield(i_Probe)%ef_name,field_defs)
 !
 sigma=Efield(i_Probe)%width
 !
 select case( trim(field_defs(1)) )
   case('STATIC','SIN','ANTIRES','RES','SOFTSIN','THETA')
     ! Fields which do not need T_0
     T_0=0._SP
   case('DELTA')
     ! Special case DELTA T_0
     T_0=2*(RT_time(2)-RT_time(1))
   case('GAUSS','QSSIN','QSFIELD','PULSE','SPULSE')
     ! Fields which need T_0
     W_0=field_frequency(Efield(i_Probe),1)
     T_0=pi/W_0*(real(nint(W_0/pi*3._SP*sigma),SP))
     if(trim(field_defs(2))=="2SIGMA" .or. trim(field_defs(3))=="2SIGMA") T_0=2._SP*sigma
     if(trim(field_defs(2))=="3SIGMA" .or. trim(field_defs(3))=="3SIGMA") T_0=3._SP*sigma
     if(trim(field_defs(2))=="4SIGMA" .or. trim(field_defs(3))=="4SIGMA") T_0=4._SP*sigma
     if(trim(field_defs(2))=="5SIGMA" .or. trim(field_defs(3))=="5SIGMA") T_0=5._SP*sigma
 end select
 !
 T_ref=Efield(i_Probe)%t_initial+T_0/2._SP
 !
 select case ( trim(damp_type) )
   case("LORENTZIAN")  
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-abs((RT_time(i_t)-T_ref)*damp_factor))
   case("GAUSSIAN") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = exp(-(RT_time(i_t)-T_ref)**2*damp_factor**2)
   case("NONE") 
     forall(i_t=1:RT_conf%T_n_steps) damp_function(i_t) = 1._SP
   case default
     call error(" Unknown damping! ")
     return
 end select
 !
 forall(i_t=1:RT_conf%T_n_steps)  ft(i_t,:)=ft(i_t,:)*damp_function(i_t)
 !
end subroutine RT_damp_it

