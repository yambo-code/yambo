!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): MG CA
!
subroutine NL_output(i_time,i_run)
 !
 use pars,         ONLY:SP,schlen,pi
 use units,        ONLY:FS2AUT,HA2EV,SPEED_OF_LIGHT
 use fields,       ONLY:A_tot,A_ext
 use com,          ONLY:msg,of_open_close
 use stderr,       ONLY:intc
 use real_time,    ONLY:NE_steps,RT_step
 use nl_optics,    ONLY:NL_er,NL_estep,VERB_LOW,NL_verbosity,NL_J,l_eval_CURRENT,NL_P,NL_desc, &
&                       l_pump_and_probe,loop_on_angles,loop_on_frequencies,n_angles 
 use interfaces,   ONLY:DESC_write
 !
 implicit none
 !
 integer, intent(in) :: i_time,i_run
 !
 ! Working space
 !
 character(schlen)  :: file_name(4),short_file_name(4)
 character(schlen)  :: headings(4,10)
 real(SP)           :: data_to_dump(4,11),angle
 integer            :: n_files,n_headings(4),i_f
 !
 if(NL_verbosity==VERB_LOW) return
 !
 file_name(1)='external_potential_F'//intc(i_run)
 file_name(2)='polarization_F'//intc(i_run)
 file_name(3)='e_total_F'//intc(i_run)
 file_name(4)='current_F'//intc(i_run)
 !
 n_files=1
 short_file_name(n_files)='external_potential'
 n_headings(n_files)=10
 !
 n_files=n_files+1
 short_file_name(n_files)='polarization'
 n_headings(n_files)=7
 !
 n_files=n_files+1
 short_file_name(n_files)='e_total'
 n_headings(n_files)=4
 !
 if(l_eval_CURRENT) then
   n_files=n_files+1
   short_file_name(n_files)='current'
   n_headings(n_files)=7
 endif
 !
 !
 ! Open files if i_time==1
 !
 if(i_time==1) then 
   !
   headings(:,1)="Time[fs]"
   !
   headings(1, 2: 4)=(/"A_x","A_y","A_z"/)
   headings(1, 5:7 )=(/"A'_x","A'_y","A'_z"/)
   headings(1, 8:10)=(/"A''_x","A''_y","A''_z"/)
   !
   headings(2, 2:4)=(/"Pol_x","Pol_y","Pol_z"/)
   headings(2, 5:7)=(/"<Dip_x>","<Dip_y>","<Dip_z>"/)
   !
   headings(3, 2:4)=(/"Etot_x","Etot_y","Etot_z"/)
   !
   headings(4, 2:4)=(/"Curr_x","Curr_y","Curr_z"/)
   headings(4, 5:7)=(/"<v_x>","<v_y>","<v_z>"/)
   !
   do i_f=1,n_files
     call of_open_close(trim(file_name(i_f)),'ot')
     call DESC_write('o '//trim(short_file_name(i_f)),'#',NL_desc)
     call msg('o '//trim(short_file_name(i_f)),'#')
     if(l_pump_and_probe) then
       call msg('o '//trim(short_file_name(i_f)),'# Pump and probe mode')
     else if(loop_on_frequencies) then
       call msg('o '//trim(short_file_name(i_f)),'# Frequency number    ',i_run,INDENT=0)
       call msg('o '//trim(short_file_name(i_f)),'# Frequency value [ev]',&
&            (NL_er(1)+real((i_run-1),SP)*NL_estep)*HA2EV,INDENT=0)
     else if(loop_on_angles) then
       call msg('o '//trim(short_file_name(i_f)),'# Angle number        ',i_run,INDENT=0)
       angle=2.0*pi/real(n_angles,SP)*(i_f-1.0)
       call msg('o '//trim(short_file_name(i_f)),'# Angle value [grad]  ',angle,INDENT=0)
     endif
     call msg('o '//trim(short_file_name(i_f)),'#')
     call msg('o '//trim(short_file_name(i_f)),'#',headings(i_f,1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(short_file_name(i_f)),'#')
   enddo
   !
 endif
 !
 ! Write data on files
 !
 data_to_dump(:,1)=RT_step*(i_time-1._SP)/FS2AUT
 !
 data_to_dump(1,2:4) =real(A_ext%vecpot(:))
 data_to_dump(1,5:7) =real(A_ext%vecpot_vel(:))
 data_to_dump(1,8:10)=real(A_ext%vecpot_acc(:))
 !
 data_to_dump(2,2:4) =real(NL_P(1:3))
 data_to_dump(2,5:7) =real(NL_P(4:6))
 !
 data_to_dump(3,2:4) =-real(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT
 !
 if(l_eval_CURRENT) then
   data_to_dump(4,2:4) =real(NL_J(1:3))
   data_to_dump(4,5:7) =real(NL_J(4:6))
 endif
 !
 do i_f=1,n_files
   call msg('o '//trim(file_name(i_f)),'',data_to_dump(i_f,1:n_headings(i_f)),USE_TABS=.TRUE.)
 enddo
 !
 ! Close files if i_time==NE_steps
 !
 if(i_time==NE_steps) then
   do i_f=1,n_files
     call of_open_close(trim(short_file_name(i_f)))
   enddo
 endif
 !
end subroutine NL_output
