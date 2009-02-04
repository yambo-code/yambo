!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine Ext_Field(ik,Sigma_s)
 !
 use pars,           ONLY:SP,pi
 use com,            ONLY:error
 use SC,             ONLY:SC_bands,SC_potential
 use real_time,      ONLY:Ext_Field_freq,Ext_Field_Int,Ext_Field_name,RTime,&
&                         RT_step,T_initial,Ef_rhoq0
 implicit none
 integer, intent(in)     :: ik
 complex(SP), intent(inout) :: Sigma_s(SC_bands,SC_bands) 
 ! 
 ! Work Space
 !
 complex(SP) :: Et
 real(SP)    :: sigma
 !
 select case(trim(Ext_Field_name))
   case('SINUSOIDAL')
     Et=Ext_Field_Int*cos(Ext_Field_freq*(RTime-T_initial))
   case('DELTA')
     sigma=RT_step
     Et=Ext_Field_Int*1._SP/(sigma*sqrt(pi))*exp(-((RTime-T_initial)/sigma)**2)
   case('STEP')
     Et=Ext_Field_Int
   case default
     call error("External Field "//trim(Ext_Field_name)//" not implemented ") 
 end select
 !
 ! Add the External Field to the singular part of the Self-Energy
 !
#if defined _DOUBLE
 call zaxpy( SC_bands*SC_bands, Et, Ef_rhoq0(:,:,ik), 1, Sigma_s, 1 )
#else
 call caxpy( SC_bands*SC_bands, Et, Ef_rhoq0(:,:,ik), 1, Sigma_s, 1 )
#endif 
 !
end subroutine Ext_Field
