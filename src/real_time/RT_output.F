!
! Copyright (C) 2000-2013 C. Attaccalite and the YAMBO team
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
subroutine RT_output(what,VALUEs,TIME,E)
 ! 
 use units,          ONLY:HA2EV,HA2KEL
 use pars,           ONLY:lchlen,SP,schlen
 use com,            ONLY:msg,of_open_close
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt
 use SC,             ONLY:SC_description,n_SC_descriptions,it_now
 use fields,         ONLY:A_vecpot
 use D_lattice,      ONLY:Bose_Temp
 use stderr,         ONLY:set_real_printed_length,string_split,intc
 use real_time,      ONLY:RT_ind_J,RT_P,RT_mean_lifetimes,l_Debye_TBose_update, &
&                         l_NE_simulate_IP,RT_IO_output_steps,NE_i_time,RT_Tfit_lifetimes!,Fluence
 use LIVE_t,         ONLY:what_is_running
#if defined _TIMING
 use timing_m,       ONLY:timing
#endif
 implicit none
 !
 character(*),       intent(in) :: what
 real(SP), optional, intent(in) :: VALUEs(:) 
 real(SP), optional, intent(in) :: TIME
 real(SP), optional, intent(in) :: E
 !
 ! Work space
 !
 integer, parameter       :: n_output_files=100,max_X_order=9
 character(lchlen), save  :: file_name(n_output_files)
 character(schlen)        :: orders(n_output_files),short_file_name(n_output_files)
 integer                  :: n1,i_f,i_fp,n_headings(n_output_files),next_file,i_order,i_c
 logical                  :: file_is_todo(n_output_files),open_files,initialize
 logical,           save  :: SC_descriptions_dumped(n_output_files),file_already_opened(n_output_files)
 character(schlen)        :: headings(n_output_files,11),append_ch
 character(2)             :: chart_string(3)
 real(SP)                 :: data_to_dump(11)
 !
 if ( mod(NE_i_time-1,RT_IO_output_steps)/=0 ) return
 !
#if defined _TIMING
 call timing('RT output',OPR='start')
#endif
 !
 call string_split(what,orders)
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 open_files=index(what,'open')>0
 initialize=index(what,'initialize')>0
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
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'vector_potential'
 short_file_name(next_file)=trim(append_ch)//'vector_potential'
 n_headings(next_file)=10
 headings(next_file,2:4 )=(/"A_x","A_y","A_z"/)
 headings(next_file,5:7 )=(/"A'_x","A'_y","A'_z"/)
 headings(next_file,8:10)=(/"A''_x","A''_y","A''_z"/)
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'energy'
 short_file_name(next_file)=trim(append_ch)//'energy'
 n_headings(next_file)=6
 headings(next_file,2:3)=(/"E_kin[eV]","E_ion[eV]"/)
 headings(next_file,4:5)=(/"Hartree [eV]","Exc     [eV]"/)
 headings(next_file,6)="E_tot[eV]"
 next_file=next_file+1
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'dynamics'
 short_file_name(next_file)=trim(append_ch)//'dynamics'
 n_headings(next_file)=4
 if (l_elel_scatt.or.l_elph_scatt) n_headings(next_file)=n_headings(next_file)+2
 if (l_Debye_TBose_update) n_headings(next_file)=n_headings(next_file)+1
 headings(next_file,2)="Total E[eV]"
 headings(next_file,3)="N" 
 headings(next_file,4)="dG"
 if (l_elel_scatt.or.l_elph_scatt) then 
   headings(next_file,5)="T(hol) [K]" 
   headings(next_file,6)="T(el)  [K]" 
   if (l_Debye_TBose_update) headings(next_file,7)="T(bose) [K]"
 endif
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
 n_headings(next_file)=7
 headings(next_file,2:3)=(/"Hole  [meV]","Elec  [meV]"/)
 headings(next_file,4:5)=(/"H abs [meV]","E abs [meV]"/)
 headings(next_file,6:7)=(/"T Hole  [K]","T Elec  [K]"/)
 next_file=next_file+1
 !
 if (open_files.and.l_elel_scatt.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'mean_EElifetimes'
 short_file_name(next_file)=trim(append_ch)//'mean_EElifetimes'
 n_headings(next_file)=5
 headings(next_file,2:3)=(/"Hole [meV]","Elec [meV]"/)
 headings(next_file,4:5)=(/"T Hole [K]","T Elec [K]"/)
 next_file=next_file+1
 !
 headings(1:next_file-1,1)="Time[fs]"
 !
 ! ... energy resolved ...
 !
 chart_string=(/"_x","_y","_z"/)
 !
 if (open_files.and..not.file_already_opened(next_file)) &
&   file_name(next_file)=trim(append_ch)//'eps_along_E'
 short_file_name(next_file)=trim(append_ch)//'eps_along_E'
 n_headings(next_file)=3
 headings(next_file,2:3)=(/'eps/Im','eps/Re'/)
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
 n_headings(next_file)=3
 headings(next_file,2:3)=(/'eel/Im','eel/Re'/)
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
&   file_name(next_file)=trim(append_ch)//'el_dos'
 short_file_name(next_file)=trim(append_ch)//'el_dos'
 n_headings(next_file)=3
 headings(next_file,1:2)=(/'T [fs]','E [eV]'/)
 headings(next_file,3  )='DOS'
 next_file=next_file+1
 !
 ! Check which files to write
 !----------------------------
 !
 file_is_todo=.FALSE.
 do i_f=1,n_output_files
   do i_fp=1,n_output_files
     if (len_trim(orders(i_fp))==0) cycle
     if (.not.file_is_todo(i_f)) file_is_todo(i_f) = index(file_name(i_f),trim(orders(i_fp)))>0
   enddo
 enddo
 !
 ! OPEN 
 !-------
 !
 if (open_files) then
   !
   SC_descriptions_dumped=.FALSE.
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
 do i_f=1,n_output_files
   !
   if (.not.file_is_todo(i_f)) cycle
   !
   call of_open_close(file_name(i_f),'oa')
   !
   ! Headers and Descriptions
   !---------------------------
   !
   if (.not.SC_descriptions_dumped(i_f)) then
     do i_fp=1,n_SC_descriptions
       call msg('o '//trim(short_file_name(i_f)),'#',trim(SC_description(i_fp)),INDENT=0)
     enddo
     call msg('o '//trim(short_file_name(i_f)),'#')
     call msg('o '//trim(short_file_name(i_f)),'#',headings(i_f,1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(short_file_name(i_f)),'#')
     SC_descriptions_dumped(i_f)=.TRUE.
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
   if (present(TIME)) data_to_dump(1)=TIME
   if (present(E))    data_to_dump(1)=E
   !
   select case (trim(short_file_name(i_f)))
     case('current','YPP-current')
       data_to_dump(2:4) =real(RT_ind_J)
     case('polarization','YPP-polarization','YPP-dP_order_0',&
&         'YPP-dP_order_1','YPP-dP_order_2','YPP-dP_order_3','YPP-dP_order_4',&
&         'YPP-dP_order_5','YPP-dP_order_6','YPP-dP_order_7','YPP-dP_order_8','YPP-dP_order_9',&
&         'YPP-P_minus_P_pump')
       data_to_dump(2:4) =real(RT_P)
     case('vector_potential')
       data_to_dump(2:4) =real(A_vecpot%vecpot(:))
       data_to_dump(5:7) =real(A_vecpot%vecpot_vel(:))
       data_to_dump(8:10)=real(A_vecpot%vecpot_acc(:))
       !data_to_dump(11)  =Fluence(it_now)
     case('energy')
       data_to_dump(2:6) = VALUES(1:5)
     case('dynamics')
       data_to_dump(2:4) = VALUES(1:3)
       if(l_elel_scatt.or.l_elph_scatt) data_to_dump(5:6) = VALUES(4:5)
       if(l_Debye_TBose_update) data_to_dump(7) = Bose_Temp*HA2KEL
     case('YPP-eps_along_E','YPP-eps_x','YPP-eps_y','YPP-eps_z','YPP-eels_along_E','YPP-SHG_qssin',&
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
     case('YPP-X_effective','YPP-el_dos')
       data_to_dump(1:2)=(/Time,E/)
       data_to_dump(3:n_headings(i_f)) = VALUES(1:n_headings(i_f)-2)
     case('mean_EPlifetimes')
       data_to_dump(2:5) = RT_mean_lifetimes(1:4)*HA2EV*1000.
       data_to_dump(6:7) = RT_Tfit_lifetimes(1:2)*HA2KEL
     case('mean_EElifetimes')
       data_to_dump(2:3) = RT_mean_lifetimes(5:6)*HA2EV*1000.
       data_to_dump(4:5) = RT_Tfit_lifetimes(3:4)*HA2KEL
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
#if defined _TIMING
 call timing('RT output',OPR='stop')
#endif
 !
end subroutine RT_output
