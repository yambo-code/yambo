! 
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine RT_occupations_analyze(k,E)
 !
 use units,          ONLY:FS2AUT,SP
 use pars,           ONLY:schlen
 Use stderr,         ONLY:intc
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT
 use com,            ONLY:msg,of_open_close
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate
 use SC,             ONLY:RT_occupations,SC_description,n_SC_descriptions
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_T,n_T_steps,i_fp
 real(SP)              :: io_Time
 character(schlen)     :: titles(2)
 integer,     external :: RT_occupations_check
 !
 call section('*','Real-Time occupations')
 !
 call QP_state_table_setup(E)
 !
 n_T_steps=RT_occupations_check(E,k)
 !
 if (n_T_steps<0) return
 !
 call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
 call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
 call msg('s','Steps          :',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   io_Time=0.
   if (i_T >1) then
     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
     call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=io_Time)
   endif
   !
   do i_s=1,QP_n_states
     !
     ! n   =QP_table(i_s,1)
     ! k   =QP_table(i_s,3)
     ! sp  =QP_table(i_s,4)
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
     !
     if (i_T==1) then
       !
       call of_open_close(trim(output_fname),'ot')
       !
       do i_fp=1,n_SC_descriptions
         call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
       enddo
       !
       titles(1)='T[fs]'
       titles(2)='Occupation'
       call msg('o '//trim(output_fname),'#')
       call msg('o '//trim(output_fname),'#',titles(:2),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#')
       !
     endif
     !
     call msg('o '//trim(output_fname),'',(/io_TIME/FS2AUT,RT_occupations(ib,ik)/2./),INDENT=-2,USE_TABS=.true.)
     !
     if (i_T==n_T_steps) call of_open_close(trim(output_fname))
     !
   enddo
   !
 enddo
 !
end subroutine RT_occupations_analyze
