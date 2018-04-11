!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA AM
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
subroutine RT_output(what,VALUES,TIME,E)
 ! 
 use units,          ONLY:HA2EV,HA2KEL,SPEED_OF_LIGHT,AU2KWCMm2,PS2AUT,AU2VMm1
 use pars,           ONLY:lchlen,SP,schlen
 use com,            ONLY:msg,of_open_close
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt,l_use_Hxc_collisions
 use QP_m,           ONLY:Time_of
 use fields,         ONLY:A_tot,A_ind,A_ext
 use electrons,      ONLY:n_spin,Spin_magn,Orbital_magn,Itinerant_magn
 use D_lattice,      ONLY:Bose_Temp
 use stderr,         ONLY:set_real_printed_length,string_split,intc,real2ch
 use RT_control,     ONLY:RT_do_it,RT_description,n_RT_descriptions
 use real_time,      ONLY:RT_ind_J,RT_P,RT_mean_lifetimes,RT_magn,l_lftm_fit_temperatures, &
&                         EE_prefactor,EP_prefactor,EP_abs_prefactor,RAD_prefactor,    &
&                         RT_Tfit_lifetimes
 use LIVE_t,         ONLY:what_is_running
 use timing_m,       ONLY:timing
#if defined _ELPH
 use real_time,      ONLY:N_RT_PH_displaced,l_RT_PH_oscillations,RT_PH_displaced,RT_D
 use ELPH,           ONLY:elph_branches
#endif
 !
 implicit none
 !
 character(*),       intent(in) :: what
 real(SP), optional, intent(in) :: VALUES(:) 
 real(SP), optional, intent(in) :: TIME
 real(SP), optional, intent(in) :: E
 !
 ! Work space
 !
 integer, parameter       :: n_output_files=100,max_X_order=9
 character(lchlen), save  :: file_name(n_output_files)
 character(schlen)        :: orders(n_output_files),short_file_name(n_output_files)
 integer                  :: i_f,i_fp,n_headings(n_output_files),next_file,i_order,i_c,n1,il
 logical                  :: file_is_todo(n_output_files),open_files,add_empty_line,initialize,close_files,add_data
 logical,           save  :: RT_descriptions_dumped(n_output_files),file_already_opened(n_output_files)
 character(schlen)        :: headings(n_output_files,max(11,N_RT_PH_displaced)),append_ch,extension_ch,tmp_ch
 character(2)             :: chart_string(3)
 real(SP)                 :: data_to_dump(max(11,N_RT_PH_displaced))
 !
 open_files    =index(what,'open')>0
 close_files   =index(what,'close')>0
 add_empty_line=index(what,'space')>0
 initialize    =index(what,'initialize')>0
 add_data      =present(VALUES)
 !
 if (.not.RT_do_it('OUT').and..not.open_files.and..not.initialize) return      
 !
 call timing('RT output',OPR='start')
 !
 call string_split(what,orders)
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 ! File Names & HEADERS
 !----------------------
 !
 ! Time Resolved...
 !
 headings=" "
 if (initialize) file_name=" "
 if (initialize) file_already_opened=.FALSE.
 short_file_name=" "
 next_file=1
 !
 append_ch=' '
 if (index(what_is_running,'YPP')>0) append_ch='YPP-'
 !
 if (open_files.and..not.file_already_opened(next_file)) file_name(next_file)=trim(append_ch)//'current'
 short_file_name(next_file)=trim(append_ch)//'current'
 n_headings(next_file)=4
 headings(next_file,2:4)=(/"j_x","j_y","j_z"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'polarization'
 short_file_name(next_file)=trim(append_ch)//'polarization'
 n_headings(next_file)=4
 headings(next_file,2:4)=(/"Pol_x","Pol_y","Pol_z"/)
 next_file=next_file+1
 !
#if defined _ELPH
 if (l_RT_PH_oscillations) then
   if (open_files.and..not.file_already_opened(next_file)) &
&     file_name(next_file)=trim(append_ch)//'displace_field'
   short_file_name(next_file)=trim(append_ch)//'displace_field'
   n_headings(next_file)=N_RT_PH_displaced+1
   i_f=1
   do il=elph_branches(1),elph_branches(2)
     if (RT_PH_displaced(il)==0) cycle
     i_f=i_f+1
     headings(next_file,i_f)="<D>_l="//trim(intc(il))//" Field"
   enddo
   next_file=next_file+1
 endif
#endif
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'total_field'
 short_file_name(next_file)=trim(append_ch)//'total_field'
 n_headings(next_file)=7
 headings(next_file,2:4)=(/"E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]"/)
 headings(next_file,5:7)=(/"E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'external_field'
 short_file_name(next_file)=trim(append_ch)//'external_field'
 n_headings(next_file)=9
 headings(next_file,2:4)=(/"E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]"/)
 headings(next_file,5:7)=(/"E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/)
 headings(next_file,8)="Int. [kW/cm^2]"
 headings(next_file,9)="Fluence[nJ/cm^2]"
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'induced_field'
 short_file_name(next_file)=trim(append_ch)//'induced_field'
 n_headings(next_file)=7
 headings(next_file,2:4)=(/"E_x/Re[V/m]","E_y/Re[V/m]","E_z/Re[V/m]"/)
 headings(next_file,5:7)=(/"E_x/Im[V/m]","E_y/Im[V/m]","E_z/Im[V/m]"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'energy'
 short_file_name(next_file)=trim(append_ch)//'energy'
 n_headings(next_file)=5
 if(.not.l_use_Hxc_collisions) n_headings(next_file)=5+1
 headings(next_file,2:3)=(/"dE_kin[eV]","dE_ion[eV]"/)
 if(     l_use_Hxc_collisions) headings(next_file,4:5)=(/"dE_Hxc[eV]","dE_tot[eV]"/)
 if(.not.l_use_Hxc_collisions) headings(next_file,4:6)=(/"dE_Har[eV]","dE_xc [eV]","dE_tot[eV]"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'carriers'
 short_file_name(next_file)=trim(append_ch)//'carriers'
 n_headings(next_file)=4
 headings(next_file,2)="d(N_hole-N_elec)"
 headings(next_file,3)="dN_hole"
 headings(next_file,4)="dN_elec"
 if (l_elel_scatt.or.l_elph_scatt) then 
   n_headings(next_file)=n_headings(next_file)+2
   headings(next_file,5)="T_hole [K]"
   headings(next_file,6)="T_elec [K]"
 endif
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'thermodynamics'
 short_file_name(next_file)=trim(append_ch)//'thermodynamics'
 n_headings(next_file)=10
 headings(next_file, 2)="dE   [eV]"
 headings(next_file, 3)="dE(h)[eV]"
 headings(next_file, 4)="dE(e)[eV]"
 headings(next_file, 5)="dS   [eV/K]"
 headings(next_file, 6)="dS(h)[eV/K]"
 headings(next_file, 7)="dS(e)[eV/K]"
 headings(next_file, 8)="T    [K]"
 headings(next_file, 9)="T(h) [K]"
 headings(next_file,10)="T(e) [K]"
 next_file=next_file+1
 !
 do i_order=0,max_X_order,1
   if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'dP_order_'//trim(intc(i_order))
   short_file_name(next_file)=trim(append_ch)//'dP_order_'//trim(intc(i_order))
   n_headings(next_file)=4
   headings(next_file,2:4)=(/"dPol_x","dPol_y","dPol_z"/)
   next_file=next_file+1
 enddo
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'P_minus_P_pump'
 short_file_name(next_file)=trim(append_ch)//'P_minus_P_pump'
 n_headings(next_file)=4
 headings(next_file,2:4)=(/"dPol_x","dPol_y","dPol_z"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'P_minus_P_pump'
 short_file_name(next_file)=trim(append_ch)//'P_minus_P_pump'
 n_headings(next_file)=4
 headings(next_file,2:4)=(/"dPol_x","dPol_y","dPol_z"/)
 next_file=next_file+1
 !
 if (open_files.and.l_elph_scatt.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'mean_EPlifetimes'
 short_file_name(next_file)=trim(append_ch)//'mean_EPlifetimes'
 n1=2
 headings(next_file,2:3)=(/"Ho.(em) [meV]","El.(em) [meV]"/)
 if(Bose_Temp>0._SP ) then
   n1=n1+2
   headings(next_file,n1:n1+1)=(/"Ho.(abs)[meV]","El.(abs)[meV]"/)
 endif
 if(l_lftm_fit_temperatures) then
   n1=n1+2
   headings(next_file,n1:n1+1)=(/" T Hole  [K] "," T Elec  [K] "/)
 endif
 n_headings(next_file)=n1+1
 next_file=next_file+1
 !
 if (open_files.and.l_elel_scatt.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'mean_EElifetimes'
 short_file_name(next_file)=trim(append_ch)//'mean_EElifetimes'
 n1=2
 headings(next_file,2:3)=(/"Hole [meV]","Elec [meV]"/)
 if(l_lftm_fit_temperatures) then
   n1=n1+2
   headings(next_file,n1:n1+1)=(/" T Hole  [K] "," T Elec  [K] "/)
 endif
 n_headings(next_file)=n1+1
 next_file=next_file+1
 !
 if (open_files.and.l_elphoton_scatt.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'mean_RADlifetimes'
 short_file_name(next_file)=trim(append_ch)//'mean_RADlifetimes'
 n_headings(next_file)=3
 headings(next_file,2:3)=(/"Hole [ps]","Elec [ps]"/)
 next_file=next_file+1
 !
 if (open_files.and.(l_elph_scatt.or.l_elel_scatt).and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'N_dN_E_conservation_factors'
 short_file_name(next_file)=trim(append_ch)//'N_dN_E_conservation_factors'
 n1=1
 if(l_elph_scatt) then
   headings(next_file,2:3)=(/"e-p val","e-p con"/)
   n1=3
   if(Bose_Temp>0._SP) then
     headings(next_file,4:5)=(/"e-p (abs) val","e-p (abs) con"/) 
     n1=5
   endif
 endif
 if(l_elel_scatt) then
   headings(next_file,n1+1:n1+2)=(/"e-e val","e-e con"/)
   n1=n1+2
 endif
 n_headings(next_file)=n1
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'BGR'
 short_file_name(next_file)=trim(append_ch)//'BGR'
 n_headings(next_file)=3
 headings(next_file,2:3)=(/"DGap [meV]","IGap [meV]"/)
 next_file=next_file+1
 !
 headings(1:next_file-1,1)="Time[fs]  "
 !
 ! ... energy resolved ...
 !
 chart_string=(/"_x","_y","_z"/)
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'eps_along_E'
 short_file_name(next_file)=trim(append_ch)//'eps_along_E'
 n_headings(next_file)=7
 headings(next_file,2:7)=(/'Im/eps_d1','Im/eps_d2','Im/eps_d3','Re/eps_d1','Re/eps_d2','Re/eps_d3'/)
 headings(next_file,1)="E [eV]"
 next_file=next_file+1
 !
 do i_c=1,3
   if (open_files.and..not.file_already_opened(next_file)) &
&     file_name(next_file)=trim(append_ch)//'eps'//chart_string(i_c)
   short_file_name(next_file)=trim(append_ch)//'eps'//chart_string(i_c)
   n_headings(next_file)=3
   headings(next_file,2:3)=(/'eps/Im','eps/Re'/)
   headings(next_file,1)="E [eV]"
   next_file=next_file+1
 enddo
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'eels_along_E'
 short_file_name(next_file)=trim(append_ch)//'eels_along_E'
 n_headings(next_file)=7
 headings(next_file,2:7)=(/'Im/eel_d1','Im/eel_d2','Im/eel_d3','Re/eel_d1','Re/eel_d2','Re/eel_d3'/)
 headings(next_file,1)="E [eV]"
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'Polarization_W'
 short_file_name(next_file)=trim(append_ch)//'Polarization_W'
 n_headings(next_file)=3
 headings(next_file,1)="E [eV]"
 headings(next_file,2:3)=(/'X/Im','X/Re'/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'E_frequency'
 short_file_name(next_file)=trim(append_ch)//'E_frequency'
 n_headings(next_file)=7
 headings(next_file,2:4)=(/'E_x(w)/Re','E_y(w)/Re','E_z(w)/Re'/)
 headings(next_file,5:7)=(/'E_x(w)/Im','E_y(w)/Im','E_z(w)/Im'/)
 headings(next_file,1)="E [eV]"
 next_file=next_file+1
 !
 do i_order=0,max_X_order,1
   !
   do i_c=1,3
     if (open_files.and..not.file_already_opened(next_file)) &
&       file_name(next_file)=trim(append_ch)//'X_probe_order_'//trim(intc(i_order))//chart_string(i_c)
     short_file_name(next_file)=trim(append_ch)//'X_probe_order_'//trim(intc(i_order))//chart_string(i_c)
     n_headings(next_file)=3
     headings(next_file,1)="E [eV]"
     headings(next_file,2:3)=(/'X/Im','X/Re'/)
     if (i_order.ne.1) then
        n_headings(next_file)=5
        headings(next_file,2:5)=(/'X/Im[cm/statV]','X/Re[cm/statV]','err(X/Im)     ','err(X/Re)     '/)
     endif
     next_file=next_file+1
   enddo
   !
   if (open_files.and..not.file_already_opened(next_file)) &
&     file_name(next_file)=trim(append_ch)//'dP_W_order_'//trim(intc(i_order))
   short_file_name(next_file)=trim(append_ch)//'dP_W_order_'//trim(intc(i_order))
   n_headings(next_file)=3
   headings(next_file,1)="E [eV]"
   headings(next_file,2:3)=(/'P/Im','P/Re'/)
   next_file=next_file+1
   !
 enddo
 !
#if defined _ELPH
 if (l_RT_PH_oscillations) then
   if (open_files.and..not.file_already_opened(next_file)) &
&     file_name(next_file)=trim(append_ch)//'FT_displace_field'
   short_file_name(next_file)=trim(append_ch)//'FT_displace_field'
   n_headings(next_file)=2*N_RT_PH_displaced+1
   i_f=0
   headings(next_file,1)="E [meV]"
   do il=elph_branches(1),elph_branches(2)
     if (RT_PH_displaced(il)==0) cycle
     i_f=i_f+2
     headings(next_file,i_f )="Re[<D>_l="//trim(intc(il))//"(w)]"
     headings(next_file,i_f+1)="Im[<D>_l="//trim(intc(il))//"(w)]"
   enddo
   next_file=next_file+1
 endif
#endif
 !
 ! ... and time/energy resolved ...
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'X_effective'
 short_file_name(next_file)=trim(append_ch)//'X_effective'
 n_headings(next_file)=4
 headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
 headings(next_file,3:4)=(/'X/Im','X/Re'/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'2D_occ_dos'
 short_file_name(next_file)=trim(append_ch)//'2D_occ_dos'
 !
 if(n_spin==1) then
   n_headings(next_file)=3
   headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
   headings(next_file,3  )='DOS'
 else
   n_headings(next_file)=5
   headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
   headings(next_file,3:5)=(/'DOS','up ',' dw'/)
 endif
 next_file=next_file+1
 !
 if(n_spin>1) then
   if (open_files.and..not.file_already_opened(next_file)) &
&     file_name(next_file)=trim(append_ch)//'magnetization'
   short_file_name(next_file)=trim(append_ch)//'magnetization'
   n_headings(next_file)=4
   !n_headings(next_file)=10
   headings(next_file,1)='T [fs]'
   headings(next_file,2:4)=(/"Ms_x","Ms_y","Ms_z"/)
   !headings(next_file,2:10)=(/"Ms_x","Ms_y","Ms_z","Ml_x","Ml_y","Ml_z","Mi_x","Mi_y","Mi_z"/)
   next_file=next_file+1
 endif
 !
 do i_f=1,3
   if (i_f==1) extension_ch='trans_eps_real'
   if (i_f==2) extension_ch='trans_eps_imag'
   if (i_f==3) extension_ch='eps_eq'
   if (open_files.and..not.file_already_opened(next_file)) then
     file_name(next_file)=trim(append_ch)//trim(extension_ch)
   endif
   short_file_name(next_file)=trim(append_ch)//trim(extension_ch)
   n_headings(next_file)=3
   if(i_f<3) then
     headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
     headings(next_file,3  )='Eps'
   else
     headings(next_file,1)='E [eV]'
     headings(next_file,2:3)=(/'Im [e]','Re [e]'/)
   endif
   next_file=next_file+1
 enddo
 !
 extension_ch='trans_residual'
 if (open_files.and..not.file_already_opened(next_file)) then
   file_name(next_file)=trim(append_ch)//trim(extension_ch)
 endif
 short_file_name(next_file)=trim(append_ch)//trim(extension_ch)
 n_headings(next_file)=3
 headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
 headings(next_file,3  )='Residual'
 next_file=next_file+1
 !
 do i_f=1,3
   if (i_f==1) extension_ch='trans_refl_rel'
   if (i_f==2) extension_ch='trans_refl_abs'
   if (i_f==3) extension_ch='refl_eq'
   if (open_files.and..not.file_already_opened(next_file)) then
     file_name(next_file)=trim(append_ch)//trim(extension_ch)
   endif
   short_file_name(next_file)=trim(append_ch)//trim(extension_ch)
   if(i_f<3) then
     n_headings(next_file)=3
     headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
     headings(next_file,3  )='Reflectivity'
   else
     n_headings(next_file)=2
     headings(next_file,1)='E [eV]'
     headings(next_file,2)='Reflectivity'
   endif
   next_file=next_file+1
 enddo
 !
 do i_f=1,3
   if (i_f==1) extension_ch='trans_transm_rel'
   if (i_f==2) extension_ch='trans_transm_abs'
   if (i_f==3) extension_ch='transm_eq'
   if (open_files.and..not.file_already_opened(next_file)) then
     file_name(next_file)=trim(append_ch)//trim(extension_ch)
   endif
   short_file_name(next_file)=trim(append_ch)//trim(extension_ch)
   if(i_f<3) then
     n_headings(next_file)=3
     headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
     headings(next_file,3  )='Transmission'
   else
     n_headings(next_file)=2
     headings(next_file,1)='E [eV]'
     headings(next_file,2)='Transmission'
   endif
  next_file=next_file+1
 enddo
 !
 ! Check which files to operate on
 !---------------------------------
 !
 file_is_todo=.FALSE.
 do i_f=1,n_output_files
   do i_fp=1,n_output_files
     if ( len_trim(orders(i_fp))==0 ) cycle
     if (.not.file_is_todo(i_f)) &
       file_is_todo(i_f) = index(      file_name(i_f),trim(orders(i_fp)))>0 &
&                    .and. index(short_file_name(i_f),trim(orders(i_fp)))>0
   enddo
 enddo
 !
 ! OPEN 
 !-------
 !
 if (open_files) then
   !
   RT_descriptions_dumped=.FALSE.
   !
   do i_f=1,n_output_files
     if (.not.file_is_todo(i_f)) cycle
     call of_open_close(file_name(i_f),'ot')
     call of_open_close(trim(short_file_name(i_f)))
     file_already_opened(i_f)=.TRUE.
   enddo
   !
   call set_real_printed_length( )
   !
   goto 1
   !
 endif
 !
 ! CLOSE 
 !-------
 !
 if (close_files) then
   !
   do i_f=1,n_output_files
     if (.not.file_is_todo(i_f)) cycle
     call of_open_close(trim(short_file_name(i_f)))
   enddo
   !
   goto 1
   !
 endif
 !
 do i_f=1,n_output_files
   !
   if (.not.file_is_todo(i_f)) cycle
   !
   if (add_empty_line) then
     if(.not.file_already_opened(i_f)) cycle
     call of_open_close(file_name(i_f),'oa')
     call msg('o '//trim(short_file_name(i_f)),' ')
     call of_open_close(file_name(i_f))
     cycle
   endif
   !
   call of_open_close(file_name(i_f),'oa')
   !
   ! Headers and Descriptions
   !---------------------------
   !
   if (.not.RT_descriptions_dumped(i_f)) then
     do i_fp=1,n_RT_descriptions
       call msg('o '//trim(short_file_name(i_f)),'#',trim(RT_description(i_fp)),INDENT=0)
     enddo
     !
     if (n_spin>1.and.trim(short_file_name(i_f))=="magnetization") then
       call msg('o '//trim(short_file_name(i_f)),'#')
       tmp_ch="Equilibrium spin      magnetization is "//&
&             trim(real2ch(Spin_magn(1)))//", "//trim(real2ch(Spin_magn(2)))//", "//trim(real2ch(Spin_magn(3)))//". "
!       call msg('o '//trim(short_file_name(i_f)),'#',tmp_ch,INDENT=0,USE_TABS=.TRUE.)
!       tmp_ch="Equilibrium orbital   magnetization is "//&
!&             trim(real2ch(Orbital_magn(1)))//", "//trim(real2ch(Orbital_magn(2)))//", "//trim(real2ch(Orbital_magn(3)))//". "
!       call msg('o '//trim(short_file_name(i_f)),'#',tmp_ch,INDENT=0,USE_TABS=.TRUE.)
!       tmp_ch="Equilibrium itinerant magnetization is "//&
!&             trim(real2ch(Itinerant_magn(1)))//", "//trim(real2ch(Itinerant_magn(2)))//", "//trim(real2ch(Itinerant_magn(3)))//". "
       call msg('o '//trim(short_file_name(i_f)),'#',tmp_ch,INDENT=0,USE_TABS=.TRUE.)
     endif
     !
     call msg('o '//trim(short_file_name(i_f)),'#')
     call msg('o '//trim(short_file_name(i_f)),'#',headings(i_f,1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(short_file_name(i_f)),'#')
     RT_descriptions_dumped(i_f)=.TRUE.
     !
   endif
   !
   ! Values
   !--------
   if (.not.present(TIME).and..not.present(E)) then
     call msg('o '//trim(short_file_name(i_f)),'')
     call of_open_close(file_name(i_f))
     cycle
   endif
   !
   ! Values
   !--------
   if (present(TIME).and..not.present(E)) data_to_dump(1)=TIME
   if (present(E).and..not.present(TIME)) data_to_dump(1)=E
   if (present(TIME)  .and.   present(E)) data_to_dump(1:2)=(/TIME,E/)
   !
   select case (trim(short_file_name(i_f)))
     case('current','YPP-current')
       data_to_dump(2:4) =real(RT_ind_J)
     case('polarization','YPP-polarization','YPP-dP_order_0',&
&         'YPP-dP_order_1','YPP-dP_order_2','YPP-dP_order_3','YPP-dP_order_4',&
&         'YPP-dP_order_5','YPP-dP_order_6','YPP-dP_order_7','YPP-dP_order_8','YPP-dP_order_9',&
&         'YPP-P_minus_P_pump')
       data_to_dump(2:4) =real(RT_P)
#if defined _ELPH
     case('YPP-FT_displace_field')
       data_to_dump(1)=data_to_dump(1)*1000.
       data_to_dump(2:2*N_RT_PH_displaced+1) = VALUES
     case('displace_field','YPP-displace_field')
       if (l_RT_PH_oscillations) then
         i_c=1
         do il=elph_branches(1),elph_branches(2)
           if (RT_PH_displaced(il)==0) cycle
           i_c=i_c+1
           data_to_dump(i_c) =real(RT_D(il))
         enddo
       endif
#endif
     case('magnetization')
       data_to_dump(2: 4) = real(RT_magn(:,1))
       !data_to_dump(5: 7) = real(RT_magn(:,2))
       !data_to_dump(8:10) = real(RT_magn(:,3))
     case('total_field')
       data_to_dump(2:4) = -real(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(5:7) =-aimag(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
     case('external_field')
       data_to_dump(2:4) = -real(A_ext%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(5:7) =-aimag(A_ext%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(8)=A_ext%intensity*AU2KWCMm2
       data_to_dump(9)=A_ext%fluence*AU2KWCMm2/real(PS2AUT,SP)
     case('induced_field')
       data_to_dump(2:4) = -real(A_ind%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(5:7) =-aimag(A_ind%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
     case('BGR')
       data_to_dump(2:3) = VALUES(1:2)*HA2EV*1000._SP
     case('energy')
       if(     l_use_Hxc_collisions) data_to_dump(2:5) = VALUES(1:4)
       if(.not.l_use_Hxc_collisions) data_to_dump(2:6) = VALUES(1:5)
     case('carriers')
       data_to_dump(2:4) = VALUES(1:3)
       if(l_elel_scatt.or.l_elph_scatt) data_to_dump(5:6) = VALUES(4:5)
     case('thermodynamics')
       data_to_dump(2: 4) = VALUES(1:3)*HA2EV
       data_to_dump(5: 7) = VALUES(4:6)*HA2EV/HA2KEL
       data_to_dump(8:10) = VALUES(7:9)*HA2KEL
     case('YPP-eps_along_E','YPP-eels_along_E','YPP-E_frequency', &
&         'YPP-SHG_qssin','YPP-eps_x','YPP-eps_y','YPP-eps_z',&
&         'YPP-X_probe_order_0_x','YPP-X_probe_order_1_x','YPP-X_probe_order_2_x','YPP-X_probe_order_3_x',&
&         'YPP-X_probe_order_4_x','YPP-X_probe_order_5_x','YPP-X_probe_order_6_x','YPP-X_probe_order_7_x',&
&         'YPP-X_probe_order_8_x','YPP-X_probe_order_9_x',&
&         'YPP-X_probe_order_0_y','YPP-X_probe_order_1_y','YPP-X_probe_order_2_y','YPP-X_probe_order_3_y',&
&         'YPP-X_probe_order_4_y','YPP-X_probe_order_5_y','YPP-X_probe_order_6_y','YPP-X_probe_order_7_y',&
&         'YPP-X_probe_order_8_y','YPP-X_probe_order_9_y',&
&         'YPP-X_probe_order_0_z','YPP-X_probe_order_1_z','YPP-X_probe_order_2_z','YPP-X_probe_order_3_z',&
&         'YPP-X_probe_order_4_z','YPP-X_probe_order_5_z','YPP-X_probe_order_6_z','YPP-X_probe_order_7_z',&
&         'YPP-X_probe_order_8_z','YPP-X_probe_order_9_z',&
&         'YPP-dP_W_order_0','YPP-dP_W_order_1','YPP-dP_W_order_2','YPP-dP_W_order_3','YPP-dP_W_order_4',&
&         'YPP-dP_W_order_5','YPP-dP_W_order_6','YPP-dP_W_order_7','YPP-dP_W_order_8','YPP-dP_W_order_9',&
&         'YPP-Polarization_W')
       if(.not.present(VALUES)) cycle
       data_to_dump(2:n_headings(i_f)) = VALUES(1:n_headings(i_f)-1)
     case('YPP-X_effective','YPP-2D_occ_dos')
       data_to_dump(3:n_headings(i_f)) = VALUES(1:n_headings(i_f)-2)
     case('YPP-eps_eq','YPP-refl_eq','YPP-transm_eq')
       data_to_dump(2:n_headings(i_f)) = VALUES(1:n_headings(i_f)-1)
     case('YPP-trans_eps_real' ,'YPP-trans_eps_imag',&
&         'YPP-trans_refl_rel','YPP-trans_refl_abs',&
&         'YPP-trans_transm_rel','YPP-trans_transm_abs',&
&         'YPP-trans_residual')
       data_to_dump(3:n_headings(i_f)) = VALUES(1:n_headings(i_f)-2)
     case('mean_EPlifetimes')
       n1=2
       data_to_dump(2:3) = RT_mean_lifetimes(3:4)*HA2EV*1000._SP
       if(Bose_Temp>0._SP) then
         n1=n1+2
         data_to_dump(n1:n1+1)=RT_mean_lifetimes(5:6)*HA2EV*1000._SP
       endif
       if(l_lftm_fit_temperatures) then
         n1=n1+2
         data_to_dump(n1:n1+1) = RT_Tfit_lifetimes(1:2)*HA2KEL
       endif
     case('mean_EElifetimes')
       n1=2
       data_to_dump(2:3) = RT_mean_lifetimes(1:2)*HA2EV*1000._SP
       if(l_lftm_fit_temperatures) then
         n1=n1+2
         data_to_dump(n1:n1+1) = RT_Tfit_lifetimes(3:4)*HA2KEL
       endif
     case('mean_RADlifetimes')
       data_to_dump(2) = Time_of(RT_mean_lifetimes(7),"ps")
       data_to_dump(3) = Time_of(RT_mean_lifetimes(8),"ps")
     case('N_dN_E_conservation_factors')
       n1=1
       if(l_elph_scatt) then
         data_to_dump(2:3)  =EP_prefactor
         n1=3
         if(Bose_Temp>0._SP) then
           data_to_dump(4:5)=EP_abs_prefactor
           n1=5
         endif
       endif
       if(l_elel_scatt) then
         data_to_dump(n1+1:n1+2)=EE_prefactor
         n1=n1+2
       endif
       if(l_elphoton_scatt) then
         data_to_dump(n1+1:n1+2)=RAD_prefactor
         n1=n1+2
       endif
       !
   end select
   !
   call msg('o '//trim(short_file_name(i_f)),'',data_to_dump(1:n_headings(i_f)),USE_TABS=.TRUE.)
   !
   call of_open_close(file_name(i_f))
   !
 enddo
 !
 call set_real_printed_length( )
 !
1 continue
 !
 call timing('RT output',OPR='stop')
 !
end subroutine RT_output
