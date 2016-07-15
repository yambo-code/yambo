! 
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine RT_occ_time_plot(En,kpt,n_T_steps)
 !
 use drivers,        ONLY:l_elel_scatt,l_elph_scatt
 use units,          ONLY:FS2AUT,SP,HA2EV
 use pars,           ONLY:schlen
 use stderr,         ONLY:intc,real2ch,set_real_printed_length
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,n_sp_pol,n_spinor
 use QP_m,           ONLY:QP_table,QP_n_states
 use YPP,            ONLY:output_fname
 use YPP_real_time,  ONLY:RT_time
 use com,            ONLY:msg,of_open_close
 use vec_operate,    ONLY:c2a
 use RT_control,     ONLY:RT_apply
 use real_time,      ONLY:l_RT_CCA_Kernel,RT_carriers,RT_bands
 !
 implicit none
 !
 type(bz_samp) :: kpt
 type(levels)  :: En
 integer       :: n_T_steps
 !
 ! Work Space
 !
 integer               :: ik,ib,i_spin,i_s,i_T,n_gamma,n_output,k_range(2),bands_range(2)
 real(SP)              :: io_Time,data_to_plot(9),k_al(3),k_cc(3)
 character(schlen)     :: titles(9),specs(9,2),dumb_ch(2)
 character(3)          :: ch3_dummy
 logical               :: l_ELPH_lifetimes
 real(SP)              :: RT_gamma(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol,6),&
&                         RT_df(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol),&
&                         RT_f_bare(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol),&
&                         RT_NEQ_Lifetime(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol),&
&                         RT_gamma_bare(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol,6)
 !
 real(SP),allocatable  :: spin_factors(:,:,:),magn_factors(:,:,:)
 !
 if(all(QP_table==0)) return
 !
 if (n_spinor==2) then
   k_range    =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
   bands_range=(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   allocate(spin_factors(bands_range(1):bands_range(2),k_range(1):k_range(2),2))
   call electrons_spin_factors(kpt,k_range,bands_range,spin_factors)
   allocate(magn_factors(bands_range(1):bands_range(2),k_range(1):k_range(2),3))
   call electrons_magn_factors(kpt,k_range,bands_range,magn_factors)
 endif
 !
 l_ELPH_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
 !
 call set_real_printed_length(f_length=10,g_length=10)
 !
 ch3_dummy=" "
 !
 ! SET titles
 ! 
 n_output=2
 titles(1)='T[fs]'
 titles(2)='Delta Occ.'
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
 RT_gamma=0._SP
 RT_gamma_bare=0._SP
 RT_df=0._SP
 RT_f_bare=0._SP
 RT_NEQ_Lifetime=0._SP
 RT_gamma=0._SP
 !
 ch3_dummy="YPP"
 !
 do i_T=1,n_T_steps
   !
   io_Time=RT_time(i_T)
   !
   call RT_apply(RT_bands,En,kpt,Time=io_Time,keep_RT_carriers=.TRUE.)
   !
   ! Interpolate and fill the shadow variables defined on the small k-grid
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
   call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                   RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,RT_carriers%delta_f,RT_df,&
&                                   ch3_dummy,.TRUE.)
   !
   call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                   RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,RT_carriers%f_bare,RT_f_bare,&
&                                   ch3_dummy,.TRUE.)
   !
   n_gamma=0
   !
   if (l_ELPH_lifetimes) then
     n_gamma=4
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_P_Hole,RT_gamma(:,:,:,1),ch3_dummy,.TRUE.)
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_P_Electron,RT_gamma(:,:,:,2),ch3_dummy,.TRUE.)
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_P_Ho_abs,RT_gamma(:,:,:,3),ch3_dummy,.TRUE.)
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_P_El_abs,RT_gamma(:,:,:,4),ch3_dummy,.TRUE.)
     if(i_T==1) then
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,3),RT_gamma_bare(:,:,:,1),ch3_dummy,.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,4),RT_gamma_bare(:,:,:,2),ch3_dummy,.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,5),RT_gamma_bare(:,:,:,3),ch3_dummy,.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,6),RT_gamma_bare(:,:,:,4),ch3_dummy,.TRUE.)
     endif
   endif
   !
   if (l_elel_scatt) then
     n_gamma=n_gamma+2
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_E_Hole,RT_gamma(:,:,:,n_gamma-1),ch3_dummy,.TRUE.)
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%dGAMMA_E_E_Electron,RT_gamma(:,:,:,n_gamma),ch3_dummy,.TRUE.)
     if(i_T==1) then
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,1),RT_gamma_bare(:,:,:,n_gamma-1),ch3_dummy,.TRUE.)
       call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                       RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                       RT_carriers%GAMMA_bare(:,2),RT_gamma_bare(:,:,:,n_gamma-1),ch3_dummy,.TRUE.)
     endif
   endif
   !
   if (l_ELPH_lifetimes.or.l_elel_scatt) then
     call Nearest_kpt_interpolation(RT_carriers%nk,kpt%nibz,RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table, &
&                                     RT_carriers%kpt,kpt%pt,RT_carriers%k_weight,kpt%weights,&
&                                     RT_carriers%NEQ_Lifetime,RT_NEQ_Lifetime(:,:,:),ch3_dummy,.TRUE.)
   endif
   !
   do i_s=1,QP_n_states
     !
     ib    =QP_table(i_s,1)
     ik    =QP_table(i_s,3)
     i_spin=spin(QP_table(i_s,:))
     !
     if (ib<RT_bands(1)) cycle
     if (ib>RT_bands(2)) cycle
     !
     output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
     if (i_T==1) call open_file()
     !
     data_to_plot(1)=io_TIME/FS2AUT
     data_to_plot(2)=RT_df(ib,ik,i_spin)
     !
     if (n_gamma>0) then
       data_to_plot(3:n_gamma+2)=(RT_gamma_bare(ib,ik,i_spin,:n_gamma)+RT_gamma(ib,ik,i_spin,:n_gamma))*HA2EV*1000._SP
       data_to_plot(  n_gamma+3)= RT_NEQ_lifetime(ib,ik,i_spin)                                        *HA2EV*1000._SP
     endif
     !
     call msg('o '//trim(output_fname),'',data_to_plot(:n_gamma+3),INDENT=-2,USE_TABS=.true.)
     !
     if (i_T==n_T_steps) call of_open_close(trim(output_fname))
     !
   enddo
   !
 enddo
 !
 if (n_spinor==2) deallocate(spin_factors,magn_factors)
 !
 !
 ! Sum_{ib,ik} f_{ik,ib}(it)
 !
 !output_fname='YPP-RT_occupations_ALL'
 !call of_open_close(trim(output_fname),'ot')
 !titles(1)='T[fs]'
 !titles(2)='Elec.'
 !titles(3)='Holes'
 !titles(4)='Tot. Occ. [OMEGA^-1]'
 !call msg('o '//trim(output_fname),'#')
 !call msg('o '//trim(output_fname),'#',titles(:4),INDENT=0,USE_TABS=.TRUE.)
 !call msg('o '//trim(output_fname),'#')
 !do i_T=1,n_T_steps
 !  io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
 !  data_to_plot(:4)=(/io_TIME/FS2AUT,RT_occupations_sum(:,i_T)/)  !/(DL_vol*BO2ANG**3*1.E-24)/)
 !  call msg('o '//trim(output_fname),'',data_to_plot(:4),INDENT=-2,USE_TABS=.true.)
 !enddo
 !call of_open_close(trim(output_fname))
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
     call c2a(v_in=kpt%pt(ik,:),v_out=k_al,mode='ki2a')
     call c2a(v_in=kpt%pt(ik,:),v_out=k_cc,mode='ki2c')
     ! 
     dumb_ch(1)='K ['//trim(intc(ik))//'] : '//trim(real2ch(k_cc(1)))//' '//&
&              trim(real2ch(k_cc(2)))//' '//trim(real2ch(k_cc(3)))//' ( cc) '
     dumb_ch(2)='        '//trim(real2ch(kpt%pt(ik,1)))//' '//&
&              trim(real2ch(kpt%pt(ik,2)))//' '//trim(real2ch(kpt%pt(ik,3)))//' (rlu) '
     !
     if(ik>  9) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik> 99) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik>999) dumb_ch(2)=' '//trim(dumb_ch(2))
     !
     call msg('o '//trim(output_fname),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(output_fname),"#",trim(dumb_ch(2)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(output_fname),"#")
     !
     if(n_spinor==2) then
       dumb_ch(1)=" Spin factors  [up,dw]   :     "//trim(real2ch(spin_factors(ib,ik,1)))//&
&                                           "     "//trim(real2ch(spin_factors(ib,ik,2)))
       call msg('o '//trim(output_fname),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),"#")
       dumb_ch(1)=" Magn factor [Mx, My, Mz]: "//trim(real2ch(magn_factors(ib,ik,1)))//&
&                                           " "//trim(real2ch(magn_factors(ib,ik,2)))//&
&                                           " "//trim(real2ch(magn_factors(ib,ik,3)))
       call msg('o '//trim(output_fname),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(output_fname),"#")
     endif
     !
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
end subroutine RT_occ_time_plot
