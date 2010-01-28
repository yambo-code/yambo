!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Ext_Field(time)
 !
 ! This subroutine add the extenal vector potential 
 !
 use pars,           ONLY:SP,pi
 use units,          ONLY:SPEED_OF_LIGHT
 use D_lattice,      ONLY:DL_vol
 use com,            ONLY:error
 use real_time,      ONLY:ext_field,n_ext_fields,Efield, &
&                         j_current_ext,A_vecpot
 !
 implicit none
 real(SP),    intent(in)     :: time
 ! 
 ! Work Space
 !
 integer     :: i1
 complex(SP) :: j_norm
 !
 j_current_ext=(0._SP,0._SP)
 !
 do i1=1,n_ext_fields
   ! 
   if(Efield(i1)%ef_name=="none".or.Efield(i1)%t_initial>time) cycle
   !
   select case(trim(Efield(i1)%ef_name))
     case('SINUSOIDAL')
       j_norm=-Efield(i1)%amplitude*cos(Efield(i1)%frequency*time)*Efield(i1)%frequency 
       j_current_ext=j_current_ext+j_norm*Efield(i1)%versor(:)/(4._SP*pi)*DL_vol
     case('RESONANT')
       j_norm=-Efield(i1)%amplitude*exp((0._SP,1._SP)*Efield(i1)%frequency*time)*Efield(i1)%frequency*(0._SP,1._SP)
       j_current_ext=j_current_ext+j_norm*Efield(i1)%versor(:)/(4._SP*pi)*DL_vol
     case('ANTIRESONANT')
       j_norm=-Efield(i1)%amplitude*exp(-(0._SP,1._SP)*Efield(i1)%frequency*time)*Efield(i1)%frequency*(0._SP,-1._SP)
       j_current_ext=j_current_ext+j_norm*Efield(i1)%versor(:)/(4._SP*pi)*DL_vol
     case('DELTA')
       j_norm=(0._SP,0._SP)
     case('STEP')
       j_norm=(0._SP,0._SP)
     case('EXPSTEP')
       j_norm       =Efield(i1)%frequency
       j_current_ext=j_current_ext+j_norm*A_vecpot%vecpot_vel(:)/(4._SP*pi)*DL_vol/SPEED_OF_LIGHT
     case default
     call error(" External Field "//trim(Efield(i1)%ef_name)//" not implemented ") 
   end select
   !
 enddo
 !
end subroutine RT_Ext_Field
