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
 use drivers,        ONLY:l_carrier_dynamics
 use units,          ONLY:FS2AUT,SP,HA2EV
 use pars,           ONLY:schlen
 use stderr,         ONLY:intc,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,spin_occ
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT
 use com,            ONLY:msg,of_open_close
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate,E_P_Hole_lifetime,E_P_Electron_lifetime,&
&                         E_P_Phonon_lifetime,l_ELPH_CCA_Kernel,E_E_Hole_lifetime,E_E_Electron_lifetime
 use SC,             ONLY:RT_occupations,SC_description,n_SC_descriptions
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_T,n_T_steps,i_fp
 real(SP)              :: io_Time,data_to_plot(7)
 real(SP), allocatable :: RT_occupations_sum(:)
 character(schlen)     :: titles(7)
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
 call msg('s','Time steps     :',n_T_steps)
 !
 allocate(RT_occupations_sum(n_T_steps)) 
 RT_occupations_sum=0._SP
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 do i_T=1,n_T_steps
   !
   io_Time=0.
   if (i_T >1) io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=io_Time)
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
       call msg('o '//trim(output_fname),'#')
       !
       ! Titles
       !========
       if (l_carrier_dynamics.and.l_ELPH_CCA_Kernel) then
         titles(:)=(/'         ','         ','|--------','--El-Ph--','--------|','|-El-El--','--------|'/)
         call msg('o '//trim(output_fname),'#',titles(:),INDENT=0,USE_TABS=.TRUE.)
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:7)=(/'Hole[meV]','El. [meV]','Ph. [meV]','Hole[meV]','El. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:7),INDENT=0,USE_TABS=.TRUE.)
       else if (l_carrier_dynamics.and..not.l_ELPH_CCA_Kernel) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:4)=(/'Hole[meV]','El. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
       else if (.not.l_carrier_dynamics.and.l_ELPH_CCA_Kernel) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:5)=(/'Hole[meV]','El. [meV]','Ph. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:5),INDENT=0,USE_TABS=.TRUE.)
       else if (.not.l_carrier_dynamics.and..not.l_ELPH_CCA_Kernel) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         call msg('o '//trim(output_fname),'#',titles(:2),INDENT=0,USE_TABS=.TRUE.)
       endif
       call msg('o '//trim(output_fname),'#')
       !
     endif
     !
     data_to_plot(:2)=(/io_TIME/FS2AUT,RT_occupations(ib,ik)/spin_occ/)
     RT_occupations_sum(i_T)=RT_occupations_sum(i_T)+RT_occupations(ib,ik)/spin_occ
     !
     if (l_ELPH_CCA_Kernel) then
       data_to_plot(3:5)=(/E_P_Hole_lifetime(ib,ik)*HA2EV,E_P_Electron_lifetime(ib,ik)*HA2EV,&
&                                              E_P_Phonon_lifetime(ib,ik)*HA2EV/)*1000.
       if (l_carrier_dynamics) then
         data_to_plot(6:7)=(/E_E_Hole_lifetime(ib,ik)*HA2EV,E_E_Electron_lifetime(ib,ik)*HA2EV/)*1000.
         call msg('o '//trim(output_fname),'',data_to_plot(:),INDENT=-2,USE_TABS=.true.)
       else
         call msg('o '//trim(output_fname),'',data_to_plot(:5),INDENT=-2,USE_TABS=.true.) 
       endif
     else
       call msg('o '//trim(output_fname),'',data_to_plot(:2),INDENT=-2,USE_TABS=.true.)
     endif
     !
     if (i_T==n_T_steps) call of_open_close(trim(output_fname))
     !
   enddo
   !
 enddo
 !
 output_fname='YPP-RT_occupations_ALL'
 call of_open_close(trim(output_fname),'ot')
 do i_fp=1,n_SC_descriptions
   call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
 enddo
 titles(1)='T[fs]'
 titles(2)='Tot. Occ.'
 call msg('o '//trim(output_fname),'#',titles(:2),INDENT=0,USE_TABS=.TRUE.)
 call msg('o '//trim(output_fname),'#')
 !
 do i_T=1,n_T_steps
   io_Time=0.
   if (i_T>1) io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   data_to_plot(:2)=(/io_TIME/FS2AUT,RT_occupations_sum(i_T)/float(QP_n_states)/)
   call msg('o '//trim(output_fname),'',data_to_plot(:2),INDENT=-2,USE_TABS=.true.)
 enddo
 call of_open_close(trim(output_fname))
 !
 call set_real_printed_length()
 !
end subroutine RT_occupations_analyze
