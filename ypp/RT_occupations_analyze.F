! 
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
 use drivers,        ONLY:l_carrier_dynamics,l_ph_corr
 use units,          ONLY:FS2AUT,SP,HA2EV,BO2ANG
 use pars,           ONLY:schlen
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,spin,spin_occ,Nel
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state,QP_nb
 use YPP,            ONLY:output_fname,OCC_T_range,OCC_deltaT,RT_n_FIT
 use com,            ONLY:msg,of_open_close,warning,error
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate,E_P_Hole_lifetime,E_P_Electron_lifetime,E_P_Phonon_lifetime,&
&                         l_RT_CCA_Kernel,E_E_Hole_lifetime,E_E_Electron_lifetime
 use SC,             ONLY:RT_occupations,SC_description,n_SC_descriptions,SC_bands
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_T,n_T_steps,i_fp
 integer               :: i1,n1,n_skip_time
 real(SP)              :: io_Time,data_to_plot(10)
 real(SP), allocatable :: RT_occupations_sum(:,:)
 real(SP)              :: RT_occ_DATA((SC_bands(2)-SC_bands(1)+1)*k%nibz,RT_n_FIT)
 real(SP)              :: RT_occ_Fit_time(RT_n_FIT)
 character(schlen)     :: titles(10)
 logical               :: l_ELPH_lifetimes,l_RT_Occ_FIT,l_RT_occ_Plot
 logical, allocatable  :: l_RT_Occ_FIT_vec(:)
 integer,     external :: RT_occupations_check
 !
 call section('*','Real-Time occupations')
 !
 call parser('OccPlot',l_RT_occ_Plot)
 call parser('OccFit' ,l_RT_occ_FIT )
 !
 if(.not.l_RT_occ_Plot .and. .not.l_RT_occ_FIT) then
   call warning(' Occupations: choose Plot or Fit or both')
   return
 endif
 !
 if(l_RT_occ_FIT.and.RT_n_FIT<1) call error(' Occupations: RT_n_FIT<1')
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
 allocate(RT_occupations_sum(3,n_T_steps)) 
 RT_occupations_sum=0._SP
 !
 allocate(l_RT_Occ_FIT_vec(n_T_steps))
 l_RT_Occ_FIT_vec=.FALSE.
 RT_occ_DATA=0._SP
 if(l_RT_Occ_FIT) then
   i1=0
   if(RT_n_FIT==1) then
     i_T=n_T_steps
     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
     l_RT_Occ_FIT_vec(i_T)=.TRUE.
     RT_occ_Fit_time(1)=io_Time
   else
     n_skip_time=n_T_steps/(RT_n_FIT-1)
     do i_T=n_T_steps,1,-n_skip_time
       i1=i1+1
       if(i1>RT_n_FIT) cycle
       io_Time=0.
       if (i_T >1) io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
       l_RT_Occ_FIT_vec(i_T)=.TRUE.
       RT_occ_Fit_time(RT_n_FIT-i1+1)=io_Time
     enddo
   endif
   i1=0
 endif
 !
 l_ELPH_lifetimes=l_ph_corr.and.l_RT_CCA_Kernel
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 ! F(it) for each (ik,ib)
 do i_T=1,n_T_steps
   !
   if(.not.(l_RT_occ_plot.or.l_RT_Occ_FIT_vec(i_T))) cycle
   !
   io_Time=0.
   if (i_T >1) io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   if (l_RT_Occ_FIT_vec(i_T)) i1=i1+1
   !
   call OCCUPATIONS_IO_and_interpolate(E,'G',k=k,Time=io_Time)
   !
   call RT_occupations_repair(E,k)
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
     if (ib<SC_bands(1)) cycle
     if (ib>SC_bands(2)) cycle
     if (ik>k%nibz) cycle
     !
     ! Prepare f(eps_{ik,ib}) for i_T=it_report
     !
     if (l_RT_Occ_FIT_vec(i_T)) RT_occ_DATA(i_s,i1)=RT_occupations(ib,ik)     
     !
     if (.not.l_RT_occ_Plot) cycle
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
       if (l_carrier_dynamics.and.l_ELPH_lifetimes) then
         titles(1:2)=(/'         ','         '/)
         titles(3:5)=(/'|--------','--El-Ph--','--------|'/)
         titles(6:7)=(/'|-----El-','El------|'/)
         call msg('o '//trim(output_fname),'#',titles(:),INDENT=0,USE_TABS=.TRUE.)
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:5)=(/'Hole[meV]','El. [meV]','Ph. [meV]'/)
         titles(6:7)=(/'Hole[meV]','El. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:7),INDENT=0,USE_TABS=.TRUE.)
       else if (l_carrier_dynamics.and..not.l_ELPH_lifetimes) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:4)=(/'Hole[meV]','El. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
       else if (.not.l_carrier_dynamics.and.l_ELPH_lifetimes) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         titles(3:5)=(/'Hole[meV]','El. [meV]','Ph. [meV]'/)
         call msg('o '//trim(output_fname),'#',titles(:5),INDENT=0,USE_TABS=.TRUE.)
       else if (.not.l_carrier_dynamics.and..not.l_ELPH_lifetimes) then
         titles(1)='T[fs]'
         titles(2)='Occ.'
         call msg('o '//trim(output_fname),'#',titles(:2),INDENT=0,USE_TABS=.TRUE.)
       endif
       call msg('o '//trim(output_fname),'#')
       !
     endif
     !
     if (ib<SC_bands(1)) then
       RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+spin_occ*k%weights(ik)
       cycle
     endif
     !
     if (ib<=E%nbf) data_to_plot(:2)=(/io_TIME/FS2AUT,(RT_occupations(ib,ik)-spin_occ)*k%weights(ik)/)
     if (ib> E%nbf) data_to_plot(:2)=(/io_TIME/FS2AUT, RT_occupations(ib,ik)          *k%weights(ik)/)
     if (ib<=E%nbf) RT_occupations_sum(1,i_T)=RT_occupations_sum(1,i_T)+data_to_plot(2)
     if (ib> E%nbf) RT_occupations_sum(2,i_T)=RT_occupations_sum(2,i_T)+data_to_plot(2)
     RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+RT_occupations(ib,ik)*k%weights(ik)
     !
     n1=1
     if (l_ELPH_lifetimes) then
       n1=4
       data_to_plot(3:4)=(/E_P_Hole_lifetime(ib,ik),E_P_Electron_lifetime(ib,ik)/)
       data_to_plot(5)  =E_P_Phonon_lifetime(ib,ik)
       data_to_plot(3:)=data_to_plot(3:)*HA2EV*1000._SP
     endif
     if (l_carrier_dynamics) then
       n1=3
       if(l_ELPH_lifetimes) n1=6
       data_to_plot(n1:n1+1)=(/E_E_Hole_lifetime(ib,ik),E_E_Electron_lifetime(ib,ik)/)*HA2EV*1000.
     endif
     call msg('o '//trim(output_fname),'',data_to_plot(:n1+1),INDENT=-2,USE_TABS=.true.)
     !
     if (i_T==n_T_steps) call of_open_close(trim(output_fname))
     !
   enddo
   !
 enddo
 !
 !
 if(l_RT_occ_plot) then
   !
   ! Sum_{ib,ik} f_{ik,ib}(it)
   !
   output_fname='YPP-RT_occupations_ALL'
   call of_open_close(trim(output_fname),'ot')
   do i_fp=1,n_SC_descriptions
     call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
   enddo
   titles(1)='T[fs]'
   titles(2)='Elec.'
   titles(3)='Holes'
   titles(4)='Tot. Occ. [OMEGA^-1]'
   call msg('o '//trim(output_fname),'#')
   call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
   call msg('o '//trim(output_fname),'#')
   !
   do i_T=1,n_T_steps
     io_Time=0.
     if (i_T>1) io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
     data_to_plot(:4)=(/io_TIME/FS2AUT,RT_occupations_sum(:,i_T)/)  !/(DL_vol*BO2ANG**3*1.E-24)/)
     call msg('o '//trim(output_fname),'',data_to_plot(:4),INDENT=-2,USE_TABS=.true.)
   enddo
   call of_open_close(trim(output_fname))
 endif
 !
 ! f(eps_{ik,ib})
 !
 if(l_RT_occ_Fit) call RT_occupations_FIT(k,E,RT_occ_DATA(:QP_n_states,:),RT_occ_Fit_time)
 !
 call set_real_printed_length()
 !
end subroutine RT_occupations_analyze
