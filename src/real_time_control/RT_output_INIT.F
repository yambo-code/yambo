!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine RT_output_INIT( )
 !
 ! RT File Names & HEADERS
 ! 
 use pars,           ONLY:schlen,SP
 use RT_output_m,    ONLY:RT_FILE_add,YPP_append,N_MAX_RT_X_order
 use electrons,      ONLY:n_spin
 use com,            ONLY:msg
 use drivers,        ONLY:l_use_Hxc_collisions,l_elel_scatt,l_elph_scatt
 use stderr,         ONLY:intc
 use real_time,      ONLY:l_lftm_fit_temperatures
 use LIVE_t,         ONLY:what_is_running
 use timing_m,       ONLY:timing
 use D_lattice,      ONLY:Bose_Temp
 !
 implicit none
 !
 ! Work space
 !
 integer                  :: i_f,i_fp,i_c,i_order,il
 character(schlen)        :: dumb_ch,dumb_ch2
 character(2)             :: chart_string(3)
 !
 call msg('s','Output files definition ...')
 !
 call timing('RT output (INIT)',OPR='start')
 !
 if (index(what_is_running,'YPP')>0) YPP_append='YPP-'
 !
 ! Time Resolved...
 !=================
 !
 call RT_FILE_add(name='current',TITLES=(/"Time[fs]","j_x     ","j_y     ","j_z     "/))
 !
 call RT_FILE_add(name='polarization',TITLES=(/"Time[fs]","Pol_x   ","Pol_y   ","Pol_z   "/))
 !
 call RT_FILE_add('total_field',TITLES=(/"Time[fs]"/))
 call RT_FILE_add(TITLES=(/"E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]","E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/))
 !
 do i_fp=1,2
   if (i_fp==1) dumb_ch="external_field"
   if (i_fp==2) dumb_ch="induced_field"
   call RT_FILE_add(trim(dumb_ch),&
&                 TITLES=(/"Time[fs]   ","E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]","E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/))
   if (i_fp==1) call RT_FILE_add(TITLES=(/"Int. [kW/cm^2]  ","Fluence[nJ/cm^2]"/))
 enddo
 !
 !
 do i_order=0,N_MAX_RT_X_order,1
   call RT_FILE_add('dP_order_'//trim(intc(i_order)),TITLES=(/"Time[fs]","dPol_x  ","dPol_y  ","dPol_z  "/))
 enddo
 !
 call RT_FILE_add('P_minus_P_pump',TITLES=(/"Time[fs]","dPol_x  ","dPol_y  ","dPol_z  "/))
 !
 !
 ! ... energy resolved ...
 !=========================
 !
 chart_string=(/"_x","_y","_z"/)
 !
 call RT_FILE_add('eps_along_E',TITLES=(/"E [eV]   ",'Im/eps_d1','Im/eps_d2',&
&                                        'Im/eps_d3','Re/eps_d1','Re/eps_d2','Re/eps_d3'/))
 !
 do i_c=1,3
   call RT_FILE_add('eps'//chart_string(i_c),TITLES=(/'E [eV]','eps/Im','eps/Re'/))
 enddo
 !
 call RT_FILE_add('eels_along_E',TITLES=(/'E [eV]   ','Im/eel_d1','Im/eel_d2',&
&                                         'Im/eel_d3','Re/eel_d1','Re/eel_d2','Re/eel_d3'/))
 !
 call RT_FILE_add('Polarization_W',TITLES=(/'E [eV]','X/Im  ','X/Re  '/))
 !
 call RT_FILE_add('E_frequency',TITLES=(/"E [eV]"/))
 call RT_FILE_add(TITLES=(/'E_x(w)/Re','E_y(w)/Re','E_z(w)/Re','E_x(w)/Im','E_y(w)/Im','E_z(w)/Im'/))
 !
 do i_order=0,N_MAX_RT_X_order,1
   do i_c=1,3
     call RT_FILE_add('X_probe_order_'//trim(intc(i_order))//chart_string(i_c),&
&                     TITLES=(/'E [eV]','X/Im  ','X/Re  '/))
     if (i_order.ne.1) then
        call RT_FILE_add(TITLES=(/'X/Im[cm/statV]','X/Re[cm/statV]','err(X/Im)     ','err(X/Re)     '/))
     endif
   enddo
   !
   call RT_FILE_add('dP_W_order_'//trim(intc(i_order)),TITLES=(/'E [eV]','P/Im  ','P/Re  '/))
   !
 enddo
 !
 ! ... and time/energy resolved ...
 !==================================
 !
 call RT_FILE_add('X_effective',TITLES=(/'T [fs]','E [eV]','X/Im  ','X/Re  '/))
 !
 ! 
 call msg('l','done')
 !
 call timing('RT output (INIT)',OPR='stop')
 !
end subroutine RT_output_INIT
