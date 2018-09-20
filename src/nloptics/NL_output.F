!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG CA
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
! Software Foundation, Inc., 59 Temple Place - Suite 330,Boston,       _
! MA 02111-1307, USA or visit http://www.gnu.org/copyleft/gpl.txt.
!
subroutine NL_output(i_time,i_freq)
 !
 use pars,         ONLY:SP,schlen
 use units,        ONLY:FS2AUT,HA2EV,SPEED_OF_LIGHT
 use fields,       ONLY:A_tot,A_ext
 use com,          ONLY:msg,of_open_close
 use stderr,       ONLY:intc
 use real_time,    ONLY:NE_steps,RT_step,RT_P
 use nl_optics,    ONLY:NL_er,NL_estep,VERB_LOW,NL_verbosity,NL_J,NL_Po
 use stderr,       ONLY:set_real_printed_length
 !
 implicit none
 !
 integer, intent(in) :: i_time,i_freq
 !
 ! Working space
 !
 character(schlen)  :: file_name(4),short_file_name(4)
 character(schlen)  :: headings(4,10)
 real(SP)           :: data_to_dump(4,11)
 integer            :: n_files,n_headings(4),i_f
 !
 if(NL_verbosity==VERB_LOW) return
 !
 call set_real_printed_length(f_length=18,g_length=18)
 !
 file_name(1)='external_potential_F'//intc(i_freq)
 file_name(2)='polarization_F'//intc(i_freq)
 file_name(3)='e_total_F'//intc(i_freq)
 file_name(4)='current_F'//intc(i_freq)
 !
 short_file_name(1)='external_potential'
 short_file_name(2)='polarization'
 short_file_name(3)='e_total'
 short_file_name(4)='current'
 !
 n_files=4
 !
 n_headings(1)=10
 n_headings(2)=7
 n_headings(3)=4
 n_headings(4)=7
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
   headings(2, 5:7)=(/"<r_x>","<r_y>","<r_z>"/)
   !
   headings(3, 2:4)=(/"Etot_x","Etot_y","Etot_z"/)
   headings(4, 2:4)=(/"Curr_x","Curr_y","Curr_z"/)
   headings(4, 5:7)=(/"<v_x>","<v_y>","<v_z>"/)
   !
   do i_f=1,n_files
     call of_open_close(trim(file_name(i_f)),'ot')
     call msg('o '//trim(short_file_name(i_f)),'#')
     call msg('o '//trim(short_file_name(i_f)),'# Frequency number '//intc(i_freq))
     call msg('o '//trim(short_file_name(i_f)),'# Frequency value  ',(NL_er(1)+real((i_freq-1),SP)*NL_estep)*HA2EV)
     call msg('o '//trim(short_file_name(i_f)),'#')
     call msg('o '//trim(short_file_name(i_f)),'#',headings(i_f,1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(short_file_name(i_f)),'#')
   enddo
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
 data_to_dump(2,2:4) =real(RT_P(1:3))
 data_to_dump(2,5:7) =real(NL_Po(1:3))
 !
 data_to_dump(3,2:4) =-real(A_tot%vecpot_vel(:))/SPEED_OF_LIGHT
 !
 data_to_dump(4,2:4) =real(NL_J(1:3))
 data_to_dump(4,5:7) =real(NL_J(4:6))
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
 call set_real_printed_length()
 !
end subroutine NL_output
