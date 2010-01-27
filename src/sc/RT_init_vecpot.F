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
subroutine RT_init_vecpot()
 !
 ! This subroutine add the extenal vector potential 
 !
 use pars,           ONLY:SP,DP
 use com,            ONLY:error
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:ext_field,A_vecpot,n_ext_fields,Efield
 !
 implicit none
 ! 
 ! Work Space
 !
 integer     :: i1
 complex(SP) :: A_norm
 !
 A_vecpot%vecpot    =(0._SP,0._SP)
 A_vecpot%vecpot_vel=(0._SP,0._SP)
 !
 do i1=1,n_ext_fields
   ! 
   if(Efield(i1)%ef_name=="none") cycle
   !
   select case(trim(Efield(i1)%ef_name))
     case('SINUSOIDAL')
     case('RESONANT')
       A_norm=-Efield(i1)%amplitude*SPEED_OF_LIGHT
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)-A_norm*Efield(i1)%versor(:)
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)-A_norm*Efield(i1)%versor(:)/(Efield(i1)%frequency*(0._SP,1._SP))
     case('ANTIRESONANT')
       A_norm=-Efield(i1)%amplitude*SPEED_OF_LIGHT
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)-A_norm*Efield(i1)%versor(:)
       A_vecpot%vecpot(:)=A_vecpot%vecpot(:)-A_norm*Efield(i1)%versor(:)/(Efield(i1)%frequency*(0._SP,-1._SP))
     case('DELTA')
       A_norm=-Efield(i1)%amplitude*SPEED_OF_LIGHT
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)+Efield(i1)%versor(:)*A_norm
     case('STEP')
       A_norm=-Efield(i1)%amplitude*SPEED_OF_LIGHT
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)+Efield(i1)%versor(:)*A_norm
     case('EXPSTEP')
       A_norm=-Efield(i1)%amplitude*SPEED_OF_LIGHT
       A_vecpot%vecpot_vel(:)=A_vecpot%vecpot_vel(:)+Efield(i1)%versor(:)*A_norm
       A_vecpot%vecpot(:)    =A_vecpot%vecpot(:)-Efield(i1)%versor(:)*A_norm/Efield(i1)%frequency
     case default
       call error(" External Field "//trim(Efield(i1)%ef_name)//" not implemented ") 
   end select
   !
 enddo
 !
end subroutine RT_init_vecpot
