!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team
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
subroutine RT_output(what,VALUEs,TIME_or_E)
 ! 
 use pars,           ONLY:lchlen,SP,schlen
 use com,            ONLY:msg,of_open_close,opened_of
 use SC,             ONLY:SC_description,n_SC_descriptions
 use fields,         ONLY:A_vecpot
 use stderr,         ONLY:set_real_printed_length,string_split
 use real_time,      ONLY:RT_ind_J,RT_P
 use timing,         ONLY:what_is_running
 implicit none
 !
 character(*),       intent(in) :: what
 real(SP), optional, intent(in) :: VALUEs(:) 
 real(SP), optional, intent(in) :: TIME_or_E
 !
 ! Work space
 !
 integer, parameter       :: n_output_files=7
 character(lchlen), save  :: file_name(n_output_files)
 character(schlen)        :: orders(n_output_files),short_file_name(n_output_files)
 integer                  :: i_f,i_fp,n_headings(n_output_files)
 logical                  :: file_is_todo(n_output_files),initialize,finalize
 logical,           save  :: SC_descriptions_dumped(n_output_files)
 character(schlen)        :: headings(n_output_files,10),append_ch
 real(SP)                 :: data_to_dump(20)
 !
 call string_split(what,orders)
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 initialize=index(what,'open')>0
 finalize  =index(what,'close')>0
 !
 ! File Names & HEADERS
 !----------------------
 !
 ! Time Resolved...
 !
 headings=" "
 headings(1:4,1)="Time[fs]"
 !
 append_ch=' '
 if (index(what_is_running,'YPP')>0) append_ch='YPP-'
 !
 if (initialize) file_name(1)=trim(append_ch)//'current'
 short_file_name(1)=trim(append_ch)//'current'
 n_headings(1)=4
 headings(1,2:4)=(/"j_x","j_y","j_z"/)
 !
 if (initialize) file_name(2)=trim(append_ch)//'polarization'
 short_file_name(2)=trim(append_ch)//'polarization'
 n_headings(2)=4
 headings(2,2:4)=(/"Pol_x","Pol_y","Pol_z"/)
 !
 if (initialize) file_name(3)=trim(append_ch)//'vector_potential'
 short_file_name(3)=trim(append_ch)//'vector_potential'
 n_headings(3)=10
 headings(3,2:4 )=(/"A_x","A_y","A_z"/)
 headings(3,5:7 )=(/"A'_x","A'_y","A'_z"/)
 headings(3,8:10)=(/"A''_x","A''_y","A''_z"/)
 !
 if (initialize) file_name(4)=trim(append_ch)//'dynamics'
 short_file_name(4)=trim(append_ch)//'dynamics'
 n_headings(4)=8
 headings(4,2:3)=(/"E_kin[eV]","E_ion[eV]"/)
 headings(4,4:6)=(/"Hartree [eV]","Exchange[eV]","Correlat[eV]"/)
 headings(4,7)="E_mg[eV]"
 headings(4,8)="N" 
 !
 ! ... and energy resolved
 !
 headings(5:6,1)="E [eV]"
 !
 if (initialize) file_name(5)=trim(append_ch)//'eps'
 short_file_name(5)=trim(append_ch)//'eps'
 n_headings(5)=3
 headings(5,2:3)=(/'eps/Im','eps/Re'/)
 !
 if (initialize) file_name(6)=trim(append_ch)//'eels'
 short_file_name(6)=trim(append_ch)//'eels'
 n_headings(6)=3
 headings(6,2:3)=(/'eel/Im','eel/Re'/)
 !
 if (initialize) file_name(7)=trim(append_ch)//'nonlinear'
 short_file_name(7)=trim(append_ch)//'nonlinear'
 n_headings(7)=3
 headings(7,2:3)=(/'X/Im','X/Re'/)
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
 ! OPEN & CLOSE
 !--------------
 !
 if (initialize.or.finalize) then
   !
   SC_descriptions_dumped=.FALSE.
   !
   do i_f=1,n_output_files
     if (.not.file_is_todo(i_f)) cycle
     if (initialize) call of_open_close(file_name(i_f),'ot')
     if (finalize)   call of_open_close(trim(short_file_name(i_f)))
   enddo
   !
   return
   !
 endif
 !
 do i_f=1,n_output_files
   !
   if (.not.file_is_todo(i_f)) cycle
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
   data_to_dump(1)=TIME_or_E
   select case (i_f)
     case(1)
       data_to_dump(2:4) =real(RT_ind_J)
     case(2)
       data_to_dump(2:4) =real(RT_P)
     case(3)
       data_to_dump(2:4) =real(A_vecpot%vecpot(:))
       data_to_dump(5:7) =real(A_vecpot%vecpot_vel(:))
       data_to_dump(8:10)=real(A_vecpot%vecpot_acc(:))
     case(4:)
       data_to_dump(2:n_headings(i_f)) = VALUES
   end select
   !
   call msg('o '//trim(short_file_name(i_f)),'',data_to_dump(1:n_headings(i_f)),USE_TABS=.TRUE.)
   !
 enddo
 !
 call set_real_printed_length( )
 !
end subroutine RT_output
