!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_output_INIT( )
 !
 ! RT File Names & HEADERS
 ! 
 use pars,           ONLY:schlen
 use RT_output_m,    ONLY:RT_FILE_add,YPP_append,N_MAX_RT_X_order
 use electrons,      ONLY:n_spin
 use com,            ONLY:msg
 use drivers,        ONLY:l_use_Hxc_collisions,l_elel_scatt,l_elph_scatt
 use stderr,         ONLY:intc
 use RT_lifetimes,   ONLY:l_RT_lifetimes_Tfit,RT_EP_abs_life
 use RT_control,     ONLY:l_project_spin_dipoles
 use C_driver,       ONLY:code_bin
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 ! Work space
 !
 integer                  :: i_f,i_fp,i_c,i_order,il
 character(schlen)        :: dumb_ch
 character(2)             :: chart_string(3)
 !
 call msg('s','Output files definition ...')
 !
 call timing('RT output (INIT)',OPR='start')
 !
 if (index(code_bin,'ypp')>0) YPP_append='YPP-'
 !
 ! Time Resolved...
 !=================
 !
 call RT_FILE_add(name='current',TITLES=(/"Time[fs]","j_x     ","j_y     ","j_z     "/))
 call RT_FILE_add(name='polarization',TITLES=(/"Time[fs]","Pol_x   ","Pol_y   ","Pol_z   "/))
 call RT_FILE_add(name='probe-current',TITLES=(/"Time[fs]","j_x     ","j_y     ","j_z     "/))
 call RT_FILE_add(name='probe-polarization',TITLES=(/"Time[fs]","Pol_x   ","Pol_y   ","Pol_z   "/))
 call RT_FILE_add(name='pump-current',TITLES=(/"Time[fs]","j_x     ","j_y     ","j_z     "/))
 call RT_FILE_add(name='pump-polarization',TITLES=(/"Time[fs]","Pol_x   ","Pol_y   ","Pol_z   "/))
 !
 do i_fp=1,3
   if (i_fp==1) dumb_ch="external_field"
   if (i_fp==2) dumb_ch="induced_field"
   if (i_fp==3) dumb_ch="total_field"
   call RT_FILE_add(trim(dumb_ch),&
&                 TITLES=(/"Time[fs]   ","E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]","E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/))
   if (i_fp==1) call RT_FILE_add(TITLES=(/"Profile  [V/m]  ","Int. [kW/cm^2]  ","Fluence[nJ/cm^2]"/))
 enddo
 !
 call RT_FILE_add('energy',TITLES=(/"Time[fs]  ","dE_kin[eV]","dE_ion[eV]"/))
 if(     l_use_Hxc_collisions) call RT_FILE_add(TITLES=(/"dE_Hxc[eV]","dE_tot[eV]"/))
 if(.not.l_use_Hxc_collisions) call RT_FILE_add(TITLES=(/"dE_Har[eV]","dE_xc [eV]","dE_tot[eV]"/))
! !
 call RT_FILE_add('carriers',TITLES=(/"Time[fs]"/))
 call RT_FILE_add(TITLES=(/"d(N_hole-N_elec)"/))
 call RT_FILE_add(TITLES=(/"dN_hole","dN_elec"/))
 if (l_elel_scatt.or.l_elph_scatt) call RT_FILE_add(TITLES=(/"T_hole [K]","T_elec [K]","Ef(h) [eV]","Ef(e) [eV]"/))
 !
 call RT_FILE_add('thermodynamics',&
&  TITLES=(/"Time[fs]   ","dE   [eV]  ","dE(h)[eV]  ","dE(e)[eV]  ","dS   [eV/K]",&
&           "dS(h)[eV/K]","dS(e)[eV/K]","T    [K]   ","T(h) [K]   ","T(e) [K]   "/))
 !
 do i_order=0,N_MAX_RT_X_order,1
   call RT_FILE_add('dP_order_'//trim(intc(i_order)),TITLES=(/"Time[fs]","dPol_x  ","dPol_y  ","dPol_z  "/))
 enddo
 !
 call RT_FILE_add('P_minus_P_pump',TITLES=(/"Time[fs]","dPol_x  ","dPol_y  ","dPol_z  "/))
 !
 call RT_FILE_add('mean_EPlifetimes',TITLES=(/"Time [fs]       ","Hole  Abs  [meV]","Elec  Emit [meV]"/))
 if(RT_EP_abs_life%active) call RT_FILE_add(TITLES=(/"Hole Emit [meV]","Elec Abs  [meV]"/))
 if(l_RT_lifetimes_Tfit) call RT_FILE_add(TITLES=(/" T Hole  [K] "," T Elec  [K] "/))
 !
 call RT_FILE_add('mean_EElifetimes',TITLES=(/"Time [fs]       ","Hole  Abs  [meV]","Elec  Emit [meV]"/))
 if(l_RT_lifetimes_Tfit) call RT_FILE_add(TITLES=(/" T Hole  [K] "," T Elec  [K] "/))
 !
 call RT_FILE_add('mean_RADlifetimes',(/"Time[fs] ","Hole [ps]","Elec [ps]"/))
 !
 call RT_FILE_add('N_dN_E_conservation_factors',(/"Time[fs] "/))
 if(l_elph_scatt) call RT_FILE_add(TITLES=(/"e-p Hole Abs ","e-p Elec Emit"/))
 if (RT_EP_abs_life%active) call RT_FILE_add(TITLES=(/"e-p Elec Abs ","e-p Hole Emit"/))
 if(l_elel_scatt) call RT_FILE_add(TITLES=(/"e-e Hole","e-e Elec"/))
 !
 call RT_FILE_add('BGR',(/"Time[fs]  "/))
 call RT_FILE_add(TITLES=(/"DirGap [meV]","IndGap [meV]"/))
 call RT_FILE_add(TITLES=(/"DeltaDirGap [meV]","DeltaIndGap [meV]"/))
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
 call RT_FILE_add('single_time_dos',TITLES=(/'E [eV]','DOS   '/))
 if (n_spin==2) call RT_FILE_add(TITLES=(/'up','dn'/))
 !
 call RT_FILE_add('single_time_INTERPOLATED_dos',TITLES=(/'E [eV]','DOS   '/))
 if (n_spin==2) call RT_FILE_add(TITLES=(/'up','dn'/))
 !
 call RT_FILE_add('TD_dos',TITLES=(/'T [fs]','E [eV]','DOS   '/))
 if (n_spin==2) call RT_FILE_add(TITLES=(/'up','dn'/))
 !
 call RT_FILE_add('TD_INTERPOLATED_dos',TITLES=(/'T [fs]','E [eV]','DOS   '/))
 if (n_spin==2) call RT_FILE_add(TITLES=(/'up','dn'/))
 !
 call RT_FILE_add('TD_P_decomposition',TITLES=(/'T        [fs]','deltaE   [eV]',&
                                                'deltaF*Dip(x)','deltaF*Dip(y)','deltaF*Dip(z)'/))
 !
 if (n_spin>1) then
   if (l_project_spin_dipoles) then
     call RT_FILE_add('spin_magnetization',&
     &                TITLES=(/'T [fs]',"Ms_x  ","Ms_y  ","Ms_z  ","Mv_x  ","Mv_y  ","Mv_z  ","Mc_x  ","Mc_y  ","Mc_z  "/))
   else 
     call RT_FILE_add('spin_magnetization',TITLES=(/'T [fs]',"Ms_x  ","Ms_y  ","Ms_z  "/))
   endif
   call RT_FILE_add('orbt_magnetization',TITLES=(/'T [fs]',"Ml_x  ","Ml_y  ","Ml_z  ","Mi_x  ","Mi_y  ","Mi_z  "/))
 endif
 !
 call msg('l','done')
 !
 call timing('RT output (INIT)',OPR='stop')
 !
end subroutine RT_output_INIT
