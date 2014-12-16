! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
integer function RT_carriers_IO_setup(E,k)
 !
 use pars,           ONLY:SP
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use R_lattice,      ONLY:bz_samp
 use units,          ONLY:FS2AUT
 use electrons,      ONLY:levels
 use rt_ctl,         ONLY:RT_carriers_IO,RT_IO_JPO_steps,OCC_deltaT,OCC_T_range
 use real_time,      ONLY:RT_step,l_RT_CCA_Kernel,&
&                         E_P_Hole_lifetime,E_P_Electron_lifetime,& 
&                         E_P_Ho_abs_lifetime,E_P_El_abs_lifetime, &
&                         E_E_Hole_lifetime,E_E_Electron_lifetime,RT_delta0_occupations,&
&                         G_lesser_reference,RT_E_occupations_reference,RT_H_occupations_reference,&
&                         NE_time,RT_nk,RT_PP_time
 use SC,             ONLY:SC_bands
 use IO_m,           ONLY:LOG,NONE
 use com,            ONLY:error,msg
 !
 implicit none
 !
 type(bz_samp)      :: k
 type(levels)       :: E
 !
 real(SP)           :: loading_time
 !
! call RT_carriers_IO(E,'G',k=k,Time=OCC_T_range(1),imposed_COM=LOG)
 !
! if (.not.allocated(RT_delta0_occupations)) call error('Occupations database not found')
 !
 if ((l_elph_scatt.and.l_RT_CCA_Kernel).and..not.allocated(E_P_Hole_lifetime)) then
   allocate(E_P_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   allocate(E_P_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   allocate(E_P_Ho_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   allocate(E_P_El_abs_lifetime(SC_bands(1):SC_bands(2),RT_nk))
 endif
 !
 if (l_elel_scatt.and..not.allocated(E_E_Hole_lifetime)) then
   allocate(E_E_Hole_lifetime(SC_bands(1):SC_bands(2),RT_nk))
   allocate(E_E_Electron_lifetime(SC_bands(1):SC_bands(2),RT_nk))
 endif
 !
 !call RT_carriers_IO(E,'G',k=k,Time=OCC_T_range(2))
 !
 if (OCC_deltaT<RT_IO_JPO_steps*RT_step) then
   ! 
   OCC_deltaT=RT_IO_JPO_steps*RT_step
   ! 
 else
   !
   OCC_deltaT=nint(OCC_deltaT/RT_IO_JPO_steps/RT_step)*RT_IO_JPO_steps*RT_step
   !
 endif
 !
 RT_carriers_IO_setup=nint( (OCC_T_range(2)-OCC_T_range(1))/OCC_deltaT )+1
 !
 if (RT_carriers_IO_setup<0) return
 !
 if (NE_time<OCC_T_range(2)) then
   OCC_T_range(2)=NE_time
   if(OCC_deltaT>NE_time) OCC_deltaT=NE_time
   RT_carriers_IO_setup=nint(OCC_T_range(2)/OCC_deltaT)+1
 endif
 !
 call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
 call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
 call msg('s','Time steps     :',RT_carriers_IO_setup)
 !
end function RT_carriers_IO_setup
