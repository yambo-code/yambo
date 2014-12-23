! 
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT,SP,HA2EV
 use pars,           ONLY:schlen
 use stderr,         ONLY:intc,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states
 use YPP,            ONLY:output_fname,l_RT_occ_Plot,l_RT_occ_Fit,RT_IO_time_range_setup
 use com,            ONLY:msg,of_open_close
 use IO_m,           ONLY:LOG
 use QP_CTL_m,       ONLY:QP_apply
 use rt_ctl,         ONLY:OCC_T_range,OCC_deltaT,RT_apply
 use real_time,      ONLY:l_RT_CCA_Kernel,RT_carriers
 use SC,             ONLY:SC_bands
 !
 implicit none
 !
 type(bz_samp) :: k
 type(levels)  :: E
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_RT,i_T,n_T_steps,n_gamma,n_output
 real(SP)              :: io_Time,data_to_plot(9)
 character(schlen)     :: titles(9),specs(9,2)
 logical               :: l_ELPH_lifetimes
 real(SP), allocatable :: RT_E_occ(:,:,:),RT_gamma(:,:,:,:),RT_df(:,:,:),RT_f_bare(:,:,:),&
&                         RT_NEQ_Lifetime(:,:,:),RT_gamma_bare(:,:,:)
 !
 call section('+','Real-Time occupations')
 !
 ! Pre-setup of carrier components and Time evolution parameters
 !
 call RT_apply(SC_bands,E,k,IO_COM=LOG,keep_RT_carriers=.TRUE.)
 !
 call  RT_IO_time_range_setup(n_T_steps)
 !
 ! Additional QP corrections
 !=================================
 !
 call QP_apply(SC_bands,E,k,'G',msg_fmt='s')
 !
 l_ELPH_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 ! SET titles
 ! 
 n_output=2
 titles(1)='T[fs]'
 titles(2)='Occ.'
 !
 if (l_elel_scatt.and.l_ELPH_lifetimes) then
   n_output=9
   titles(3:6) =(/'Hole[meV]','El. [meV]','Hole[meV]','El. [meV]'/)
   titles(7:9) =(/'Hole[meV]','El. [meV]','  [meV]  '/)
   specs(1:2,1)=(/'         ','         '/)
   specs(3:6,1)=(/'|--------','------El-','Ph-------','--------|'/)
   specs(7:9,1)=(/'|-----El-','El------|','|--NEQ--|'/)
   specs(1:2,2)=(/'         ','         '/)
   specs(3:6,2)=(/'   (emis ','sion)    ','   (absor','ption)   '/)
   specs(7:9,2)=(/'         ','         ','         '/)
 else if (l_elel_scatt.and..not.l_ELPH_lifetimes) then
   n_output=5
   titles(3:5) =(/'Hole[meV]','El. [meV]','  [meV]  '/)
   specs(1:2,1)=(/'         ','         '/)
   specs(3:5,1)=(/'|-----El-','El------|','|--NEQ--|'/)
   specs(1:2,2)=(/'         ','         '/)
   specs(3:5,2)=(/'         ','         ','         '/)
 else if (.not.l_elel_scatt.and.l_ELPH_lifetimes) then
   n_output=7
   titles(3:7) =(/'Hole[meV]','El. [meV]','Hole[meV]','El. [meV]','  [meV]  '/)
   specs(1:2,1)=(/'         ','         '/)
   specs(3:7,1)=(/'|--------','------El-','Ph-------','--------|','|--NEQ--|'/)
   specs(1:2,2)=(/'         ','         '/)
   specs(3:7,2)=(/'   (emis ','sion)    ','   (absor','ption)   ','         '/)
 endif
 !
 ! Shadow variables:
 !
 if (l_RT_occ_Fit) then
   allocate(RT_E_occ(RT_carriers%nstates,n_T_steps,1),RT_gamma(RT_carriers%nstates,n_T_steps,6,1))
   RT_E_occ  =0._SP
 endif
 if (l_RT_occ_Plot) then
   allocate(RT_gamma(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol,6))
   allocate(RT_df(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol))
   allocate(RT_f_bare(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol))
   allocate(RT_NEQ_Lifetime(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol)) 
   allocate(RT_gamma_bare(RT_carriers%nb(1):RT_carriers%nb(2),K%nibz,n_sp_pol)) 
   RT_gamma=0._SP
   RT_gamma_bare=0._SP
   RT_df=0._SP
   RT_f_bare=0._SP
   RT_NEQ_Lifetime=0._SP
 endif
 RT_gamma=0._SP
 !
 do i_T=1,n_T_steps
   !
   io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
   !
   call RT_apply(SC_bands,E,k,Time=io_Time,keep_RT_carriers=.TRUE.)
   !
   if (l_RT_occ_Fit) then
     !
     ! Fill the large arrays to be used for the fitting procedure.
     !
     do i_RT=1,RT_carriers%nstates
       !
       ! Occupations
       !============
       RT_E_occ(i_RT,i_T,1)=         RT_carriers%f_bare(i_RT)+RT_carriers%delta_f(i_RT)
       !
       ! Lifetimes
       !===========
       n_gamma=0
       if (l_ELPH_lifetimes) then
         n_gamma=4
         RT_gamma(i_RT,i_T,1,1)=RT_carriers%dGAMMA_E_P_Hole(i_RT)+RT_carriers%GAMMA_bare(i_RT,3)
         RT_gamma(i_RT,i_T,2,1)=RT_carriers%dGAMMA_E_P_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,4)
         RT_gamma(i_RT,i_T,3,1)=RT_carriers%dGAMMA_E_P_Ho_abs(i_RT)+RT_carriers%GAMMA_bare(i_RT,5)
         RT_gamma(i_RT,i_T,4,1)=RT_carriers%dGAMMA_E_P_El_abs(i_RT)+RT_carriers%GAMMA_bare(i_RT,6)
       endif
       !
       if (l_elel_scatt) then
         n_gamma=n_gamma+2
         RT_gamma(i_RT,i_T,n_gamma-1,1)=RT_carriers%dGAMMA_E_E_Hole(i_RT)+RT_carriers%GAMMA_bare(i_RT,1)
         RT_gamma(i_RT,i_T,n_gamma  ,1)=RT_carriers%dGAMMA_E_E_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,2)
       endif
       !
     enddo
     !
   endif
   !
   ! Interpolate and fill the shadow variables defined on the small k-grid
   !
   if (l_RT_occ_Plot) then
     !
     !real(SP)   ,pointer:: f_bare(:)       => null() ! equilibrium occupations
     !real(SP)   ,pointer:: delta_f(:)      => null() ! change in the occupations
     !real(SP)   ,pointer:: GAMMA_bare(:)   => null() ! "initial equilibrium" lifetimes. In the code the "initial equilibrium"
     !real(SP)   ,pointer:: NEQ_Lifetime(:) => null() ! time dependent lifetime defined as for Silicon
     !real(SP)   ,pointer:: delta_GAMMA(:)  => null() ! change with respect to the "initial equilibrium" lifetimes
     !real(SP)   ,pointer:: dGAMMA_E_P_Hole(:)     => null() 
     !real(SP)   ,pointer:: dGAMMA_E_P_Electron(:) => null() 
     !real(SP)   ,pointer:: dGAMMA_E_P_Ho_abs(:)   => null() 
     !real(SP)   ,pointer:: dGAMMA_E_P_El_abs(:)   => null() 
     !real(SP)   ,pointer:: dGAMMA_E_E_Hole(:)     => null() 
     !real(SP)   ,pointer:: dGAMMA_E_E_Electron(:) => null() 
     !
     call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                   RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,RT_carriers%delta_f,RT_df,&
&                                   ' ',.TRUE.)
     call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                   RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,RT_carriers%f_bare,RT_f_bare,&
&                                   ' ',.TRUE.)
     n_gamma=0
     if (l_ELPH_lifetimes) then
       n_gamma=4
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_P_Hole,RT_gamma(:,:,:,3),' ',.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_P_Electron,RT_gamma(:,:,:,4),' ',.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_P_Ho_abs,RT_gamma(:,:,:,5),' ',.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_P_El_abs,RT_gamma(:,:,:,6),' ',.TRUE.)
     endif
     if (l_elel_scatt) then
       n_gamma=n_gamma+2
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_E_Hole,RT_gamma(:,:,:,n_gamma-1),' ',.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%dGAMMA_E_E_Electron,RT_gamma(:,:,:,n_gamma),' ',.TRUE.)
     endif
     if (l_ELPH_lifetimes.or.l_elel_scatt) then
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%NEQ_Lifetime,RT_NEQ_Lifetime(:,:,:),' ',.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,k%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,k%pt,RT_carriers%k_weight,k%weights,&
&                                     RT_carriers%GAMMA_bare,RT_gamma_bare(:,:,:),' ',.TRUE.)
     endif
     !
     do i_s=1,QP_n_states
       !
       ib    =QP_table(i_s,1)
       ik    =QP_table(i_s,3)
       i_spin=spin(QP_table(i_s,:))
       !
       if (ib<SC_bands(1)) cycle
       if (ib>SC_bands(2)) cycle
       !
       if (l_RT_occ_Plot) then
         output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
         if (i_T==1) call open_file()
       endif
       !
       !  RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+RT_delta0_occ(i_RT,i_T)*k%weights(ik)/real(RT_Nk_ibz(ik),SP)
       !  !
       !  if (ib==SC_bands(1).and.SC_bands(1)>1) RT_occupations_sum(3,i_T)=RT_occupations_sum(3,i_T)+ &
       !                                         spin_occ*k%weights(ik)*real(SC_bands(1)-1)/real(RT_Nk_ibz(ik),SP)
       !
       data_to_plot(1)=io_TIME/FS2AUT
       data_to_plot(2)=RT_df(ib,ik,i_spin)
       !
       if (n_gamma>0) then
         data_to_plot(2+1:2+n_gamma)=(RT_gamma_bare(ib,ik,i_spin)+RT_gamma(ib,ik,i_spin,:n_gamma))*HA2EV*1000._SP
         data_to_plot(2+n_gamma+1)  =RT_NEQ_lifetime(ib,ik,i_spin)*HA2EV*1000._SP
       endif
       !
       !if(.not.l_group_occ .or. (ik_rim==RT_ibz_coo(ik,2).and.l_group_occ) ) then
       !  data_to_plot(2)=data_to_plot(2)/real(RT_Nk_ibz(ik),SP)
       !  if (ib<=E%nbf) RT_occupations_sum(1,i_T)=RT_occupations_sum(1,i_T)+data_to_plot(2)
       !  if (ib> E%nbf) RT_occupations_sum(2,i_T)=RT_occupations_sum(2,i_T)+data_to_plot(2)
       !endif
       !
       call msg('o '//trim(output_fname),'',data_to_plot(:n_gamma+3),INDENT=-2,USE_TABS=.true.)
       if (i_T==n_T_steps) call of_open_close(trim(output_fname))
       !
     enddo
   endif
   !
 enddo
 !
! if (l_RT_occ_plot) then
!   !
!   ! Sum_{ib,ik} f_{ik,ib}(it)
!   !
!   output_fname='YPP-RT_occupations_ALL'
!   call of_open_close(trim(output_fname),'ot')
!   titles(1)='T[fs]'
!   titles(2)='Elec.'
!   titles(3)='Holes'
!   titles(4)='Tot. Occ. [OMEGA^-1]'
!   call msg('o '//trim(output_fname),'#')
!   call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
!   call msg('o '//trim(output_fname),'#')
!   !
!   do i_T=1,n_T_steps
!     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
!     data_to_plot(:4)=(/io_TIME/FS2AUT,RT_occupations_sum(:,i_T)/)  !/(DL_vol*BO2ANG**3*1.E-24)/)
!     call msg('o '//trim(output_fname),'',data_to_plot(:4),INDENT=-2,USE_TABS=.true.)
!   enddo
!   call of_open_close(trim(output_fname))
! endif
 !
 ! Occupations Fit 
 !=================
 !
 if (l_RT_occ_Fit) call RT_occupations_FIT_and_plot(RT_carriers%E_bare,RT_E_occ,RT_gamma,n_gamma,n_T_steps)
 !
 ! CLEAN-UP
 !==========
 !
 deallocate(RT_gamma)
 if (l_RT_occ_Fit)  deallocate(RT_E_occ)
 if (l_RT_occ_Plot) deallocate(RT_df,RT_f_bare,RT_gamma_bare,RT_NEQ_Lifetime)
 !
 call set_real_printed_length()
 !
 contains
   !
   subroutine open_file()
     !
     call of_open_close(trim(output_fname),'ot')
     !
     call msg('o '//trim(output_fname),'#')
     !
     ! Titles
     !========
     if (l_elel_scatt.or.l_ELPH_lifetimes) then
       call msg('o '//trim(output_fname),'#',specs(:n_output,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),'#',specs(:n_output,2),INDENT=0,USE_TABS=.TRUE.)
     endif
     !
     call msg('o '//trim(output_fname),'#',titles(:n_output),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(output_fname),'#')
     !
   end subroutine open_file
   !
end subroutine RT_occupations_analyze
