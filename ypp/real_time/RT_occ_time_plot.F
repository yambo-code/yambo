! 
!        Copyright (C) 2000-2020 the YAMBO team
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
 use pars,           ONLY:schlen,lchlen,rZERO
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc,real2ch
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,n_sp_pol,n_spinor
 use QP_m,           ONLY:QP_table,QP_n_states
 use YPP_real_time,  ONLY:RT_time,RT_conf
 use YPP_interfaces, ONLY:k_special
 use YPPm,           ONLY:BANDS_path,CIRCUIT_k
 use LIVE_t,         ONLY:live_timing
 use com,            ONLY:msg,of_open_close
 use vec_operate,    ONLY:c2a,v_norm
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
 integer               :: i_RT,i_qp,ik_RT,ik,ib,i_spin,i_T,n_gamma,n_output,k_range(2),bands_range(2),i_a,n_axis,Nk_path,last_ik
 real(SP)              :: io_Time,tmp_lifetimes(6),data_to_plot(9),k_al(3),k_cc(3),axis(3,3),&
&                         data_to_plot_cumulative(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,9),O(3)
 character(schlen)     :: titles(9),specs(9,2),dumb_ch(2)
 character(lchlen)     :: output_fname_short(2),output_fname(2,RT_carriers%nstates)
 character(3)          :: ch3_dummy
 logical               :: l_elph_lifetimes,do_state(RT_carriers%nb(1):RT_carriers%nb(2),kpt%nibz,n_sp_pol),&
&                         l_OCCgroup_k_and_b,l_OCCgroup_path,l_OCCgroup,l_E_select
 !
 real(SP),allocatable  :: spin_factors(:,:,:),magn_factors(:,:,:)
 !
 if(all(QP_table==0)) return
 !
 ! Setup
 !-------
 call parser('OCCgroup',   l_OCCgroup)
 call parser('CarrEnRnge', l_E_select)
 !
 data_to_plot_cumulative=rZERO
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
 ! Select the states by using a circuit
 !======================================
 call k_special( )
 call k_circuit_made_of_special_points( BANDS_path, 1 )
 n_axis=3
 call PROJECTION_axis(n_axis,axis,O)
 if ( CIRCUIT_k%nbz>0 ) then
   call c2a(v_in=O,mode='ki2c')
   do i_a=1,3
     call c2a(v_in=axis(i_a,:),mode='ki2c')
   enddo
   last_ik=0
   Nk_path=0
 endif
 !
 l_OCCgroup_path   =l_OCCgroup.and.CIRCUIT_k%nbz>0
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
 if (l_OCCgroup_path) then
   n_output=3
   titles(2:3) =(/'Delta_Occ[Ho]','Delta_Occ[El]'/)
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
 call live_timing('Time Plot(s)',n_T_steps)
 ! 
 do i_T=1,n_T_steps
   !
   io_Time=RT_time(i_T)
   !
   call RT_apply(RT_bands,En,kpt,Time=io_Time,keep_RT_carriers=.TRUE.)
   !
   l_OCCgroup_k_and_b=l_OCCgroup.and.RT_carriers%nk>kpt%nibz.and.CIRCUIT_k%nbz==0
   !
   if (l_elel_scatt    .and.i_T==1) n_gamma=n_gamma+2
   !
   if (l_elph_lifetimes.and.i_T==1) n_gamma=n_gamma+4
   !
   data_to_plot(1)               =io_TIME/FS2AUT
   data_to_plot_cumulative(:,:,1)=io_TIME/FS2AUT
   if (l_OCCgroup_path) data_to_plot(2:3)=rZERO
   !
   RT_loop: do i_RT=1,RT_carriers%nstates
     !
     ib     =RT_carriers%table(i_RT,1)
     ik     =RT_carriers%table(i_RT,2)
     ik_RT  =RT_carriers%table(i_RT,3)
     i_spin =RT_carriers%table(i_RT,4)
     !
     if (ib<RT_bands(1)) cycle
     if (ib>RT_bands(2)) cycle
     !  
     if ( l_OCCgroup_path ) then
       !
       call c2a(v_in=RT_carriers%kpt(ik_RT,:),v_out=k_cc,mode='ki2c')
       !
       do i_a=1,2
         k_al(i_a)= dot_product(k_cc-O,axis(i_a,:))/v_norm(axis(i_a,:))
       enddo
       !
       if (k_al(1)<0..or.k_al(1)>v_norm(axis(1,:))) cycle RT_loop
       if (k_al(2)<0..or.k_al(2)>v_norm(axis(2,:))) cycle RT_loop
       !
     endif  
     !
     if (.not.do_state(ib,ik,i_spin)) cycle
     !
     if (l_E_select) then
       if (RT_carriers%E_bare(i_RT)<RT_conf%Carriers_W_range(1).or.RT_carriers%E_bare(i_RT)>RT_conf%Carriers_W_range(2)) cycle
     endif
     !  
     if (.not.l_OCCgroup_path) then
       output_fname_short(1)='YPP-RT_occupations_k'//trim(intc(ik))//'_kRT'//trim(intc(ik_RT))//'_b'//trim(intc(ib))
       if (i_T==1) output_fname(1,i_RT)=output_fname_short(1)
       call open_file(output_fname(1,i_RT),output_fname_short(1))
     endif
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
       if (.not.l_OCCgroup_path) then
         data_to_plot(3:n_gamma+2)=  tmp_lifetimes(1:n_gamma)    *HA2EV*1000._SP
         data_to_plot(  n_gamma+3)=RT_carriers%NEQ_Lifetime(i_RT)*HA2EV*1000._SP
       endif
     endif
     !
     if (l_OCCgroup_k_and_b) then
       data_to_plot_cumulative(ik,ib,2:)=data_to_plot_cumulative(ik,ib,2:)+data_to_plot(2:)*RT_carriers%k_weight(ik_RT)
     endif
     !
     if (l_OCCgroup_path) then
       if (ik_RT/=last_ik) then
         last_ik=ik_RT
         Nk_path=Nk_path+1
       endif
       if (ib<=En%nbf  ) data_to_plot(2)=data_to_plot(2)+RT_carriers%delta_f(i_RT)*RT_carriers%k_weight(ik_RT)
       if (ib>=En%nbf+1) data_to_plot(3)=data_to_plot(3)+RT_carriers%delta_f(i_RT)*RT_carriers%k_weight(ik_RT)
     else
       data_to_plot(2)=RT_carriers%delta_f(i_RT)
       call msg('o '//trim(output_fname_short(1)),'',data_to_plot(:n_gamma+3),INDENT=-2,USE_TABS=.true.)
       call of_open_close(output_fname(1,i_RT))
     endif
     !
   enddo RT_loop
   !
   if (l_OCCgroup_k_and_b) then
     i_RT=0
     do ik=1,kpt%nibz
       do ib=RT_carriers%nb(1),RT_carriers%nb(2)
         do i_spin=1,n_sp_pol
           if (.not.do_state(ib,ik,i_spin)) cycle
           i_RT=i_RT+1
           output_fname_short(2)='YPP-RT_cumulative_occupations_k'//trim(intc(ik))//'_b'//trim(intc(ib))
           if (i_T==1) output_fname(2,i_RT)=output_fname_short(2)
           call open_file( output_fname(2,i_RT),output_fname_short(2))
           call msg('o '//trim(output_fname_short(2)),'',data_to_plot_cumulative(ik,ib,:n_gamma+3),INDENT=-2,USE_TABS=.true.)
           call of_open_close(output_fname(2,i_RT))
         enddo
       enddo
     enddo
   endif
   !
   if (l_OCCgroup_path) then
     output_fname_short(1)='YPP-RT_cumulative_occupations'
     if (i_T==1) output_fname(1,1)=output_fname_short(1)
     call open_file( output_fname(1,1),output_fname_short(1))
     call msg('o '//trim(output_fname_short(1)),'',(/data_to_plot(1),data_to_plot(2:3)/),INDENT=-2,USE_TABS=.true.)
     call of_open_close(output_fname(1,1))
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 YAMBO_FREE(spin_factors)
 YAMBO_FREE(magn_factors)
 !
 contains
   !
   subroutine open_file(fname,fname_short)
     !
     character(lchlen)  :: fname_short,fname
     !
     if (i_T> 1) then
       call of_open_close(fname,'oa')
       return
     endif
     !
     call of_open_close(fname,'ot')
     !
     call msg('o '//trim(fname_short),'#')
     !
     if (l_OCCgroup_path) then
       call msg('o '//trim(fname_short),"#",trim(intc(Nk_path))//" K-points included",INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),'#')
     else
       !
       ! Kpt on the Coarse Grid
       !========================
       call c2a(v_in=kpt%pt(ik,:),v_out=k_cc,mode='ki2c')
       call c2a(v_in=kpt%pt(ik,:),v_out=k_al,mode='ki2a')
       ! 
       dumb_ch(1)='K ['//trim(intc(ik))//'] : '//trim(real2ch(k_cc(1)))//' '//&
&                                                trim(real2ch(k_cc(2)))//' '//&
&                                                trim(real2ch(k_cc(3)))//' ( cc) '
       dumb_ch(2)='        '//trim(real2ch(k_al(1)))//' '//&
&                             trim(real2ch(k_al(2)))//' '//&
&                             trim(real2ch(k_al(3)))//' (rlu) '
       !
       if(ik>  9) dumb_ch(2)=' '//trim(dumb_ch(2))
       if(ik> 99) dumb_ch(2)=' '//trim(dumb_ch(2))
       if(ik>999) dumb_ch(2)=' '//trim(dumb_ch(2))
       ! 
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(2)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#")
       !
     endif
     !
     if (.not.l_OCCgroup_k_and_b.and..not.l_OCCgroup_path) then
       !
       ! Kpt on the Fine Grid
       !======================
       call c2a(v_in=RT_carriers%kpt(ik_RT,:),v_out=k_cc,mode='ki2c')
       call c2a(v_in=RT_carriers%kpt(ik_RT,:),v_out=k_al,mode='ki2a')
       !
       dumb_ch(1)='K_FGd ['//trim(intc(ik_RT))//'] : '//trim(real2ch(k_cc(1)))//' '//&
&                                                       trim(real2ch(k_cc(2)))//' '//&
&                                                       trim(real2ch(k_cc(3)))//' ( cc) '
       !
       dumb_ch(2)='            '//trim(real2ch(k_al(1)))//' '//&
&                                 trim(real2ch(k_al(2)))//' '//&
&                                 trim(real2ch(k_al(3)))//' (rlu) '
       !
       if(ik_RT>   9) dumb_ch(2)=' '//trim(dumb_ch(2))
       if(ik_RT>  99) dumb_ch(2)=' '//trim(dumb_ch(2))
       if(ik_RT> 999) dumb_ch(2)=' '//trim(dumb_ch(2))
       if(ik_RT>9999) dumb_ch(2)=' '//trim(dumb_ch(2))
       !
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(2)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#")
       !
     endif
     !
     if(n_spinor==2) then
       dumb_ch(1)=" Spin factors  [up,dw]   :     "//trim(real2ch(spin_factors(ib,ik,1)))//&
&                                           "     "//trim(real2ch(spin_factors(ib,ik,2)))
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#")
       dumb_ch(1)=" Magn factor [Mx, My, Mz]: "//trim(real2ch(magn_factors(ib,ik,1)))//&
&                                           " "//trim(real2ch(magn_factors(ib,ik,2)))//&
&                                           " "//trim(real2ch(magn_factors(ib,ik,3)))
       call msg('o '//trim(fname_short),"#",trim(dumb_ch(1)),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),"#")
     endif
     !
     if ((l_elel_scatt.or.l_elph_lifetimes).and..not.l_OCCgroup_path) then
       call msg('o '//trim(fname_short),'#',specs(:n_output,1),INDENT=0,USE_TABS=.TRUE.)
       call msg('o '//trim(fname_short),'#',specs(:n_output,2),INDENT=0,USE_TABS=.TRUE.)
     endif
     !
     call msg('o '//trim(fname_short),'#',titles(:n_output),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(fname_short),'#')
     !
   end subroutine open_file
   !
end subroutine RT_occ_time_plot
