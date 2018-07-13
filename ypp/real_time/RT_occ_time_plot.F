! 
!        Copyright (C) 2000-2018 the YAMBO team
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
#include<memory.h>
 !
 type(bz_samp) :: kpt
 type(levels)  :: En
 integer       :: n_T_steps
 !
 ! Work Space
 !
 integer               :: i_RT,i_qp,ik_RT,ik,ib,i_spin,i_T,n_gamma,n_output,k_range(2),bands_range(2)
 real(SP)              :: io_Time,tmp_lifetimes(6),data_to_plot(9),k_al(3),k_cc(3)
 character(schlen)     :: titles(9),specs(9,2),dumb_ch(2)
 character(3)          :: ch3_dummy
 logical               :: l_elph_lifetimes,do_state(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol)
 !
 real(SP),allocatable  :: spin_factors(:,:,:),magn_factors(:,:,:)
 !
 if(all(QP_table==0)) return
 !
 if (n_spinor==2) then
   k_range    =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
   bands_range=(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   YAMBO_ALLOC(spin_factors,(bands_range(1):bands_range(2),k_range(1):k_range(2),2))
   call electrons_spin_factors(k_range,bands_range,spin_factors)
   YAMBO_ALLOC(magn_factors,(bands_range(1):bands_range(2),k_range(1):k_range(2),3))
   call electrons_magn_factors(k_range,bands_range,magn_factors)
 endif
 !
 l_elph_lifetimes=l_elph_scatt.and.l_RT_CCA_Kernel
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
 n_gamma=0
 !
 ch3_dummy="YPP"
 !
 do_state=.false.
 !
 do i_qp=1,QP_n_states
   !
   ib    =QP_table(i_qp,1)
   ik    =QP_table(i_qp,3)
   i_spin=spin(QP_table(i_qp,:))
   !
   if (ib<RT_bands(1)) cycle
   if (ib>RT_bands(2)) cycle
   !
   do_state(ib,ik,i_spin)=.true.
   !
 enddo
 ! 
 do i_T=1,n_T_steps
   !
   io_Time=RT_time(i_T)
   !
   call RT_apply(RT_bands,En,kpt,Time=io_Time,keep_RT_carriers=.TRUE.)
   !
   if (l_elel_scatt    .and.i_T==1) n_gamma=n_gamma+2
   !
   if (l_elph_lifetimes.and.i_T==1) n_gamma=n_gamma+4
   !
   do i_RT=1,RT_carriers%nstates
     !
     ib     =RT_carriers%table(i_RT,1)
     ik     =RT_carriers%table(i_RT,2)
     ik_RT  =RT_carriers%table(i_RT,3)
     i_spin =RT_carriers%table(i_RT,4)
     !
     if (ib<RT_bands(1)) cycle
     if (ib>RT_bands(2)) cycle
     !
     if(.not.do_state(ib,ik,i_spin)) cycle
     !
     output_fname='YPP-RT_occupations_k'//trim(intc(ik))//'_kRT'//trim(intc(ik_RT))//'_b'//trim(intc(ib))
     if (i_T==1) call open_file()
     !
     data_to_plot(1)=io_TIME/FS2AUT
     data_to_plot(2)=RT_carriers%delta_f(i_RT)
     !
     if (n_gamma>0) then
       if (l_elph_lifetimes) then
         tmp_lifetimes(1)=RT_carriers%dGAMMA_E_P_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,3)
         tmp_lifetimes(2)=RT_carriers%dGAMMA_E_P_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,4)
         tmp_lifetimes(3)=RT_carriers%dGAMMA_E_P_El_Abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,5)
         tmp_lifetimes(4)=RT_carriers%dGAMMA_E_P_El_Abs(i_RT)  +RT_carriers%GAMMA_bare(i_RT,6)
       endif
       if (l_elel_scatt) then
         tmp_lifetimes(n_gamma-1)=RT_carriers%dGAMMA_E_E_Electron(i_RT)+RT_carriers%GAMMA_bare(i_RT,1)
         tmp_lifetimes(n_gamma  )=RT_carriers%dGAMMA_E_E_Hole(i_RT)    +RT_carriers%GAMMA_bare(i_RT,2)
       endif
       data_to_plot(3:n_gamma+2)=  tmp_lifetimes(1:n_gamma)    *HA2EV*1000._SP
       data_to_plot(  n_gamma+3)=RT_carriers%NEQ_Lifetime(i_RT)*HA2EV*1000._SP
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
 YAMBO_FREE(spin_factors)
 YAMBO_FREE(magn_factors)
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
     ! Kpt on the Coarse Grid
     !========================
     call c2a(v_in=kpt%pt(ik,:),v_out=k_cc,mode='ki2c')
     call c2a(v_in=kpt%pt(ik,:),v_out=k_al,mode='ki2a')
     ! 
     dumb_ch(1)='K ['//trim(intc(ik))//'] : '//trim(real2ch(k_cc(1)))//' '//&
&                                              trim(real2ch(k_cc(2)))//' '//&
&                                              trim(real2ch(k_cc(3)))//' ( cc) '
     dumb_ch(2)='        '//trim(real2ch(k_al(1)))//' '//&
&                           trim(real2ch(k_al(2)))//' '//&
&                           trim(real2ch(k_al(3)))//' (rlu) '
     !
     if(ik>  9) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik> 99) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik>999) dumb_ch(2)=' '//trim(dumb_ch(2))
     ! 
     call msg('o '//trim(output_fname),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(output_fname),"#",trim(dumb_ch(2)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(output_fname),"#")
     !
     !
     ! Kpt on the Fine Grid
     !======================
     call c2a(v_in=RT_carriers%kpt(ik_RT,:),v_out=k_cc,mode='ki2c')
     call c2a(v_in=RT_carriers%kpt(ik_RT,:),v_out=k_al,mode='ki2a')
     !
     dumb_ch(1)='K_FGd ['//trim(intc(ik_RT))//'] : '//trim(real2ch(k_cc(1)))//' '//&
&                                                     trim(real2ch(k_cc(2)))//' '//&
&                                                     trim(real2ch(k_cc(3)))//' ( cc) '
     !
     dumb_ch(2)='            '//trim(real2ch(k_al(1)))//' '//&
&                               trim(real2ch(k_al(2)))//' '//&
&                               trim(real2ch(k_al(3)))//' (rlu) '
     !
     if(ik_RT>   9) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik_RT>  99) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik_RT> 999) dumb_ch(2)=' '//trim(dumb_ch(2))
     if(ik_RT>9999) dumb_ch(2)=' '//trim(dumb_ch(2))
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
     if (l_elel_scatt.or.l_elph_lifetimes) then
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
