!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): CA AM
!
subroutine RT_output(what,VALUES,TIME,E)
 ! 
 use units,          ONLY:HA2EV,HA2KEL,SPEED_OF_LIGHT,AU2KWCMm2,PS2AUT,AU2VMm1
 use pars,           ONLY:SP,schlen
 use com,            ONLY:msg,of_open_close,com_compose_msg
 use drivers,        ONLY:l_use_Hxc_collisions
 use QP_m,           ONLY:Time_of
 use fields,         ONLY:A_tot,A_ind,A_ext
 use electrons,      ONLY:n_spin,Spin_magn
 use R_lattice,      ONLY:FineGd_desc
 use stderr,         ONLY:STRING_split
 use RT_output_m,    ONLY:N_MAX_RT_o_files,RT_descriptors_dumped,RT_o_file_is_open,&
&                         N_RT_o_files,RT_o_file_is_open,RT_o_file_short,RT_o_file_long,&
&                         n_RT_headings,RT_headings,N_MAX_RT_headings,RT_desc,Gr_desc,TIME_STEP_desc
 use RT_control,     ONLY:RT_NAN_found,OUTPUT_RT_IO_t,l_project_spin_dipoles
 use RT_lifetimes,   ONLY:l_RT_lifetimes_Tfit,&
&                         RT_EP_emit_life,RT_EE_life,RT_EH_life,RT_EP_abs_life
 use real_time,      ONLY:RT_ind_J,RT_P,RT_spin_magn,RT_orbt_magn,NE_i_time
 use timing_m,       ONLY:timing
 use C_driver,       ONLY:code_bin
 use functions,      ONLY:NAN
 use interfaces,     ONLY:DESC_write
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
 integer                  :: i_f,n_orders,i_o,il,N,i_v
 logical                  :: file_is_todo(N_MAX_RT_o_files)
 real(SP)                 :: data_to_dump(N_MAX_RT_headings)
 logical, external        :: RT_do_it
 !
 call STRING_split(what,orders,n_non_empty_strings=n_orders)
 !
 ! Check which files to operate on
 !---------------------------------
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
 if (index(what,'open')>0) then
   !
   RT_descriptors_dumped=.FALSE.
   !
   do i_f=1,N_RT_o_files
     if ((.not.file_is_todo(i_f)).or.RT_o_file_is_open(i_f)) cycle
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
 if (index(what,'close')>0) then
   !
   do i_f=1,N_RT_o_files
     if ((.not.file_is_todo(i_f)).or.(.not.RT_o_file_is_open(i_f))) cycle
     call of_open_close(RT_o_file_long(i_f))
     RT_o_file_is_open(i_f)=.FALSE.
   enddo
   !
   return
   !
 endif
 !
 if (.not.(RT_do_it('OUT').or.index(code_bin,'ypp')>0)) return
 !
 ! Check for NAN's in the VALUES and eventually stop
 !
 if (present(VALUES)) then
   N=size(VALUES)
   do i_v=1,N
     if (NAN(VALUES(i_v))) then
       call warning("Found NaN in "//trim(what)//". Dynamics stopped")
       RT_NAN_found=.TRUE.
     endif
   enddo
 endif
 !
 call timing('RT output',OPR='start')
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
   if (.not.RT_descriptors_dumped(i_f)) then
     !
     call DESC_write('o '//trim(RT_o_file_short(i_f)),'#',RT_desc)
     call DESC_write('o '//trim(RT_o_file_short(i_f)),'#',Gr_desc)
     call DESC_write('o '//trim(RT_o_file_short(i_f)),'#',FineGd_desc)
     call DESC_write('o '//trim(RT_o_file_short(i_f)),'#',TIME_STEP_desc)
     !
     if (n_spin>1.and.trim(RT_o_file_short(i_f))=="spin_magnetization") then
       call msg('o '//trim(RT_o_file_short(i_f)),'#')
       dumb_ch="Equilibrium spin magnetization"
       call msg('o '//trim(RT_o_file_short(i_f)),com_compose_msg(pre='# ',body=dumb_ch),&
&               Spin_magn,INDENT=0)
     endif
     !
     call msg('o '//trim(RT_o_file_short(i_f)),'#')
     call msg('o '//trim(RT_o_file_short(i_f)),'#',RT_headings(i_f,1:n_RT_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(RT_o_file_short(i_f)),'#')
     RT_descriptors_dumped(i_f)=.TRUE.
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
     case('YPP-probe-current','YPP-pump-current','current','YPP-current')
       data_to_dump(2:4) =real(RT_ind_J)
     !
     case('YPP-probe-polarization','YPP-pump-polarization','polarization','YPP-polarization','YPP-dP_order_0',&
&         'YPP-dP_order_1','YPP-dP_order_2','YPP-dP_order_3','YPP-dP_order_4',&
&         'YPP-dP_order_5','YPP-dP_order_6','YPP-dP_order_7','YPP-dP_order_8','YPP-dP_order_9',&
&         'YPP-P_minus_P_pump')
       data_to_dump(2:4) =real(RT_P)
     !
     case('spin_magnetization')
       data_to_dump(2: 4) = real(RT_spin_magn(:,1))
       if (l_project_spin_dipoles) then
         data_to_dump(5: 7) = real(RT_spin_magn(:,2))
         data_to_dump(8:10) = real(RT_spin_magn(:,3))
       endif
     case('orbt_magnetization')
       data_to_dump(2: 4) = real(RT_orbt_magn(:,1))
       data_to_dump(5: 7) = real(RT_orbt_magn(:,2))
     case('total_field')
       data_to_dump(2:4) = -real(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(5:7) =-aimag(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
     case('external_field')
       data_to_dump(2:4) = -real(A_ext%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(5:7) =-aimag(A_ext%vecpot_vel(:))/SPEED_OF_LIGHT*AU2VMm1
       data_to_dump(8)   =A_ext%envelop  *AU2VMm1
       data_to_dump(9)   =A_ext%intensity*AU2KWCMm2
       data_to_dump(10)  =A_ext%fluence  *AU2KWCMm2/real(PS2AUT,SP)
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
       if(RT_EE_life%active.or.RT_EP_emit_life%active) data_to_dump(5:8) = VALUES(4:7)
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
     case('YPP-X_effective','YPP-TD_dos','YPP-TD_INTERPOLATED_dos','YPP-TD_P_decomposition')
       data_to_dump(3:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-2)
     case('YPP-single_time_dos','YPP-single_time_INTERPOLATED_dos')
       data_to_dump(2:n_RT_headings(i_f)) = VALUES(1:n_RT_headings(i_f)-1)
     case('mean_EPlifetimes')
       N=2
       data_to_dump(2:3) = RT_EP_emit_life%mean*HA2EV*1000._SP
       if(RT_EP_abs_life%active) then
         N=N+2
         data_to_dump(N)  =RT_EP_abs_life%mean(2)*HA2EV*1000._SP
         data_to_dump(N+1)=RT_EP_abs_life%mean(1)*HA2EV*1000._SP
       endif
       if(l_RT_lifetimes_Tfit) then
         N=N+2
         data_to_dump(N:N+1) = RT_EP_emit_life%Tfit*HA2KEL
       endif
     case('mean_EElifetimes')
       N=2
       data_to_dump(2:3) = RT_EE_life%mean*HA2EV*1000._SP
       if(l_RT_lifetimes_Tfit) then
         N=N+2
         data_to_dump(N:N+1) = RT_EE_life%Tfit*HA2KEL
       endif
     case('mean_RADlifetimes')
       data_to_dump(2) = Time_of(RT_EH_life%mean(1),"ps")
       data_to_dump(3) = Time_of(RT_EH_life%mean(2),"ps")
     case('N_dN_E_conservation_factors')
       N=1
       if(RT_EP_emit_life%active) then
         data_to_dump(2:3)=RT_EP_emit_life%prefactor
         N=3
       endif
       if(RT_EP_abs_life%active) then
         data_to_dump(4:5)=RT_EP_abs_life%prefactor
         N=5
       endif
       if(RT_EE_life%active) then
         data_to_dump(N+1:N+2)=RT_EE_life%prefactor
         N=N+2
       endif
       if(RT_EH_life%active) then
         data_to_dump(N+1:N+2)=RT_EH_life%prefactor
         N=N+2
       endif
       !
   end select
   !
   call msg('o '//trim(RT_o_file_short(i_f)),'',data_to_dump(1:n_RT_headings(i_f)),USE_TABS=.TRUE.)
   call of_open_close(trim(RT_o_file_short(i_f)),'f')
   !
 enddo
 !
 call timing('RT output',OPR='stop')
 !
 OUTPUT_RT_IO_t%last_point=NE_i_time
 !
end subroutine RT_output
