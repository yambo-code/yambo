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
subroutine RT_Ext_Current(time)
 !
 ! This subroutine defines the external current that is then used
 ! to evolve the vector potential 
 !
 use pars,           ONLY:SP,pi
 use com,            ONLY:error
 use real_time,      ONLY:ext_field,n_ext_fields,Efield,j_current_ext,RT_step,i_time
 !
 implicit none
 real(SP),    intent(in)     :: time
 ! 
 ! Work Space
 !
 integer     :: i1
 complex(SP) :: j_norm
 real(SP)    :: sigma,t0,j_current_versor(3)
 !
 j_norm          =(0._SP,0._SP)
 j_current_ext   =(0._SP,0._SP)
 !
 do i1=1,n_ext_fields
   ! 
   if(Efield(i1)%ef_name=="none".or.Efield(i1)%t_initial>time) cycle
   !
   j_current_versor=Efield(i1)%versor(:)/(4._SP*pi)
   !
   sigma=Efield(i1)%damping         ! the width of the Gaussian is     
   t0   =10._SP*sigma               ! the center of the Gaussian is at     t0 = 10*sigma
   !
   select case(trim(Efield(i1)%ef_name))
     case('SINL')
       j_norm=-Efield(i1)%amplitude*cos(Efield(i1)%frequency*time)*Efield(i1)%frequency 
     case('ANTIRES')
       j_norm=-Efield(i1)%amplitude*exp((0._SP,1._SP)*Efield(i1)%frequency*time)*Efield(i1)%frequency*(0._SP,1._SP)
     case('RES')
       j_norm=-Efield(i1)%amplitude*exp(-(0._SP,1._SP)*Efield(i1)%frequency*time)*Efield(i1)%frequency*(0._SP,-1._SP)
     case('GAUSS')
       j_norm=Efield(i1)%amplitude*exp(-(time-t0)**2/(2._SP*sigma**2) )*(time-t0)/sigma**2
     case('QSSIN')
       j_norm=-Efield(i1)%amplitude*(cos(Efield(i1)%frequency*(time-t0))*Efield(i1)%frequency &
              &-(time-t0)/sigma**2*sin(Efield(i1)%frequency*(time-t0)))*exp(-(time-t0)**2/(2._SP*sigma**2))   
     case('DELTA')
       if(i_time==Efield(i1)%int_t_initial)     j_norm=-Efield(i1)%amplitude/RT_step**2
       if(i_time==Efield(i1)%int_t_initial+1)   j_norm= Efield(i1)%amplitude/RT_step**2
     case default
     call error(" External Field "//trim(Efield(i1)%ef_name)//" not implemented ") 
   end select
   !
   j_current_ext=j_current_ext+j_norm*j_current_versor
   !
 enddo
 !
end subroutine RT_Ext_Current
