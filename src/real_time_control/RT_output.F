!
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,           ONLY:SP,schlen
 use com,            ONLY:msg,of_open_close
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elphoton_scatt,l_use_Hxc_collisions
 use QP_m,           ONLY:Time_of
 use fields,         ONLY:A_tot,A_ind,A_ext
 use electrons,      ONLY:n_spin,Spin_magn
 use D_lattice,      ONLY:Bose_Temp
 use stderr,         ONLY:set_real_printed_length,STRING_split,real2ch
 use RT_output_m,    ONLY:N_MAX_RT_o_files,RT_descriptions_dumped,RT_o_file_is_open,&
&                         N_RT_o_files,RT_o_file_is_open,RT_o_file_short,RT_o_file_long,&
&                         n_RT_headings,RT_headings,N_MAX_RT_headings
 use RT_control,     ONLY:RT_do_it,RT_description,n_RT_descriptions
 use real_time,      ONLY:RT_ind_J,RT_P,RT_mean_lifetimes,RT_magn,l_lftm_fit_temperatures, &
&                         EE_prefactor,EP_prefactor,EP_abs_prefactor,RAD_prefactor,    &
&                         RT_Tfit_lifetimes
 use timing_m,       ONLY:timing
 use C_driver,       ONLY:code_bin
 use functions,      ONLY:NAN
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
 character(schlen)        :: orders(20),dumb_ch
 integer                  :: i_f,n_orders,i_o,i_desc,il,N,i_v
 logical                  :: file_is_todo(N_MAX_RT_o_files)
 real(SP)                 :: data_to_dump(N_MAX_RT_headings)
 logical, external        :: STOP_now
 !
 call STRING_split(what,orders,n_non_empty_strings=n_orders)
 !
 ! Check which files to operate on
 !---------------------------------
 !
 file_is_todo=.FALSE.
 do i_f=1,N_RT_o_files
   if (file_is_todo(i_f)) cycle
   do i_o=1,n_orders
     if (file_is_todo(i_f)) cycle
     if (index(code_bin,'ypp')>0) then
       file_is_todo(i_f) = index(RT_o_file_short(i_f),"YPP-"//trim(orders(i_o)))==1
     else
       file_is_todo(i_f) = index(RT_o_file_short(i_f),trim(orders(i_o)))==1
     endif
   enddo
 enddo
 !
 ! OPEN 
 !-------
 !
 if (index(what,'open')>0) then
   !
   RT_descriptions_dumped=.FALSE.
   !
   do i_f=1,N_RT_o_files
     if (.not.file_is_todo(i_f).or.RT_o_file_is_open(i_f)) cycle
     call of_open_close(RT_o_file_long(i_f),'ot')
     RT_o_file_is_open(i_f)=.TRUE.
   enddo
   !
   return
   !
 endif
 !
 ! CLOSE 
 !-------
 !
 if (index(what,'close')>0) then
   !
   do i_f=1,N_RT_o_files
     if (.not.file_is_todo(i_f).or..not.RT_o_file_is_open(i_f)) cycle
     call of_open_close(RT_o_file_long(i_f))
     RT_o_file_is_open(i_f)=.FALSE.
   enddo
   !
   return
   !
 endif
 !
 if (.not.RT_do_it('OUT')) return
 !
 ! Check for NAN's in the VALUES and eventually stop
 !
 if (present(VALUES)) then
   N=size(VALUES)
   do i_v=1,N
     if (NAN(VALUES(i_v))) then
       call warning("Found NaN in "//trim(what)//". Dynamics stopped")
       if (STOP_now(.TRUE.)) stop
     endif
   enddo
 endif
 !
 call timing('RT output',OPR='start')
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 do i_f=1,N_RT_o_files
   !
   if (.not.file_is_todo(i_f)) cycle
   if (.not.RT_o_file_is_open(i_f)) cycle
   !
   if (index(what,'space')>0) then
     call msg('o '//trim(RT_o_file_short(i_f)),' ')
     cycle
   endif
   !
   ! Headers and Descriptions
   !---------------------------
   !
   if (.not.RT_descriptions_dumped(i_f)) then
     do i_desc=1,n_RT_descriptions
       call msg('o '//trim(RT_o_file_short(i_f)),'#',trim(RT_description(i_desc)),INDENT=0)
     enddo
     if (n_spin>1.and.trim(RT_o_file_short(i_f))=="magnetization") then
       call msg('o '//trim(RT_o_file_short(i_f)),'#')
       dumb_ch="Equilibrium spin      magnetization is "//&
&             trim(real2ch(Spin_magn(1)))//", "//trim(real2ch(Spin_magn(2)))//", "//trim(real2ch(Spin_magn(3)))//". "
       call msg('o '//trim(RT_o_file_short(i_f)),'#',dumb_ch,INDENT=0,USE_TABS=.TRUE.)
     endif
     !
     call msg('o '//trim(RT_o_file_short(i_f)),'#')
     call msg('o '//trim(RT_o_file_short(i_f)),'#',RT_headings(i_f,1:n_RT_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(RT_o_file_short(i_f)),'#')
     RT_descriptions_dumped(i_f)=.TRUE.
     !
   endif
   !
   ! Values
   !--------
   if (.not.present(TIME).and..not.present(E)) then
     call msg('o '//trim(RT_o_file_short(i_f)),'')
     cycle
   endif
   !
   ! Values
   !--------
   if (present(TIME).and..not.present(E)) data_to_dump(1)=TIME
   if (present(E).and..not.present(TIME)) data_to_dump(1)=E
   if (present(TIME)  .and.   present(E)) data_to_dump(1:2)=(/TIME,E/)
   !
   select case (trim(RT_o_file_short(i_f)))
     !
     case('current','YPP-current')
       data_to_dump(2:4) =real(RT_ind_J)
     !
     case('polarization','YPP-polarization','YPP-dP_order_0',&
&         'YPP-dP_order_1','YPP-dP_order_2','YPP-dP_order_3','YPP-dP_order_4',&
&         'YPP-dP_order_5','YPP-dP_order_6','YPP-dP_order_7','YPP-dP_order_8','YPP-dP_order_9',&
&         'YPP-P_minus_P_pump')
       data_to_dump(2:4) =real(RT_P)
    !
     case('magnetization')
       data_to_dump(2: 4) = real(RT_magn(:,1))
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
       data_to_dump(2:5) = VALUES(1:4)*HA2EV*1000._SP
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
       data_to_dump(2:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-1)
     case('YPP-X_effective','YPP-TD_dos','YPP-TD_INTERPOLATED_dos')
       data_to_dump(3:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-2)
     case('YPP-single_time_dos','YPP-single_time_INTERPOLATED_dos')
       data_to_dump(2:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-1)
     case('YPP-eps_eq','YPP-refl_eq','YPP-transm_eq')
       data_to_dump(2:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-1)
     case('YPP-trans_eps_real' ,'YPP-trans_eps_imag',&
&         'YPP-trans_refl_rel','YPP-trans_refl_abs',&
&         'YPP-trans_transm_rel','YPP-trans_transm_abs',&
&         'YPP-trans_residual')
       data_to_dump(3:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-2)
     case('mean_EPlifetimes')
       N=2
       data_to_dump(2:3) = RT_mean_lifetimes(3:4)*HA2EV*1000._SP
       if(Bose_Temp>0._SP) then
         N=N+2
         data_to_dump(N:N+1)=RT_mean_lifetimes(5:6)*HA2EV*1000._SP
       endif
       if(l_lftm_fit_temperatures) then
         N=N+2
         data_to_dump(N:N+1) = RT_Tfit_lifetimes(1:2)*HA2KEL
       endif
     case('mean_EElifetimes')
       N=2
       data_to_dump(2:3) = RT_mean_lifetimes(1:2)*HA2EV*1000._SP
       if(l_lftm_fit_temperatures) then
         N=N+2
         data_to_dump(N:N+1) = RT_Tfit_lifetimes(3:4)*HA2KEL
       endif
     case('mean_RADlifetimes')
       data_to_dump(2) = Time_of(RT_mean_lifetimes(7),"ps")
       data_to_dump(3) = Time_of(RT_mean_lifetimes(8),"ps")
     case('N_dN_E_conservation_factors')
       N=1
       if(l_elph_scatt) then
         data_to_dump(2:3)  =EP_prefactor
         N=3
         if(Bose_Temp>0._SP) then
           data_to_dump(4:5)=EP_abs_prefactor
           N=5
         endif
       endif
       if(l_elel_scatt) then
         data_to_dump(N+1:N+2)=EE_prefactor
         N=N+2
       endif
       if(l_elphoton_scatt) then
         data_to_dump(N+1:N+2)=RAD_prefactor
         N=N+2
       endif
       !
   end select
   !
   call msg('o '//trim(RT_o_file_short(i_f)),'',data_to_dump(1:n_RT_headings(i_f)),USE_TABS=.TRUE.)
   !
 enddo
 !
 call set_real_printed_length( )
 !
 call timing('RT output',OPR='stop')
 !
end subroutine RT_output
