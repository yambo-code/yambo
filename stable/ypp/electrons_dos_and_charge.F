! 
! Copyright (C) 2000-2014 A. Marini and the YAMBO team
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
subroutine electrons_dos_and_charge(Xk,Xen)
 !
 use units,          ONLY:HA2EV,FS2AUT
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use wave_func,      ONLY:WF_load,WF_free,wf,wf_state
 use R_lattice,      ONLY:bz_samp,nkibz,nkbz
 use D_lattice,      ONLY:DL_vol
 use electrons,      ONLY:levels,spin,n_sp_pol,n_spinor,n_spin,spin_occ, &
&                         BZ_RIM_nkpt,BZ_RIM_table,BZ_RIM_nbands
 use FFT_m,          ONLY:fft_size
 use YPP,            ONLY:l_density,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,plot_title,l_dos,dos_broadening,dos_bands,&
&                         dos_E_range,dos_E_steps,OCC_T_ref,OCC_T_range,OCC_deltaT,RT_occupations_ref
 use com,            ONLY:msg,of_open_close
 use functions,      ONLY:Fermi_fnc_derivative
 use QP_CTL_m,       ONLY:QP_apply
 use timing,         ONLY:live_timing
#if defined _YPP_RT
 use real_time,      ONLY:OCCUPATIONS_IO_and_interpolate,RT_output
 use SC,             ONLY:SC_bands,RT_occupations,SC_description,n_SC_descriptions
#endif
 use IO_m,           ONLY:io_control,OP_RD_CL,DUMP,NONE
 use interfaces,     ONLY:PARALLEL_index
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait
 use parser_m,       ONLY:parser
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 real(SP)              :: el_dos(dos_E_steps,n_spin),dos_E,delta_E,f_occ,io_Time,dos_norm,&
&                         el_dos_max,WF_fac(dos_bands(1):dos_bands(2),nkibz,n_spinor)
 integer               :: i_E,ik,ib,i_spin,i_spinor,i_T,n_T_steps,i_fp,io_ID,io_err,ik_rand,i1
 character(schlen)     :: ch_ws,titles(4)
 logical               :: l_TIME_plot,l_3D_plot,l_RIM,l_prt_occ_only
 type(PP_indexes)      :: px
 !
 integer,     external :: ioE_RIM,RT_occupations_check 
 !
 !
 call parser('PrtOccOnly',l_prt_occ_only)
 !
 ! E_rim
 !=======
 !
 call msg("s",'RIM support ...')
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=DUMP,ID=io_ID)
 io_err=ioE_RIM(Xen,io_ID)
 !
 if (io_err==0) then
   call msg("l",'yes')
   l_RIM=.TRUE.
   call OCCUPATIONS_Extend(Xen,Xen)
 else
   call msg("l",'no')
 endif
 !
 l_RIM=io_err==0
 !
 ! DOS
 !=====
 !
 if (l_dos) then
   !
   call section('*','Electronic DOS')
   !
   l_TIME_plot=.FALSE.
   l_3D_plot  =.FALSE.
   n_T_steps=1
   !
   if(n_spinor==2) then
     !
     call live_timing('DOS-spinor-factor',nkibz*(dos_bands(2)-dos_bands(1)+1),SERIAL=.true.)
     do ik=1,nkibz
       call WF_load(0,1,dos_bands,(/ik,ik/),title='-Projected DOS',impose_free_and_alloc=.TRUE.)
       do ib=dos_bands(1),dos_bands(2)
         do i_spinor=1,n_spinor
           WF_fac(ib,ik,i_spinor)=dot_product(wf(:, wf_state(ib,ik,i_spinor) ),wf(:, wf_state(ib,ik,i_spinor) ))
         enddo
         call live_timing(steps=1)
       enddo
       call WF_free()
     enddo
     call live_timing()
     !
   endif
   !
#if defined _YPP_RT
   !
   n_T_steps=RT_occupations_check(Xen,Xk)
   !
   l_TIME_plot=n_T_steps>0
   l_3D_plot=.not.n_T_steps==1
   !
   if (.not.l_TIME_plot.and..not.l_3D_plot) return
   !
   dos_bands=SC_bands
   !
   call msg('s','Time range [fs]:',OCC_T_range/FS2AUT)
   call msg('s','Time step  [fs]:',OCC_deltaT/FS2AUT)
   call msg('s','Time ref   [fs]:',OCC_T_ref/FS2AUT)
   call msg('s','Time steps     :',n_T_steps)
   call msg('s','Energy steps   :',dos_E_steps)
   !
#else
   !
   OCC_T_range=0.
   !
#endif
   !
   if (l_RIM) dos_bands(2)=min(dos_bands(2),BZ_RIM_nbands)
   !
   el_dos_max=0.
   !
   call QP_apply(dos_bands,Xen,Xk,'G',msg_fmt='s')
   !
   ! Output files headers
   !
   if (l_TIME_plot.and.l_3D_plot) then
     !
#if defined _YPP_RT
     call RT_output("open el_dos")
#endif
   else if (.not.l_3D_plot) then
     output_fname='el_dos'
     call of_open_close(trim(output_fname),'ot')
#if defined _YPP_RT
     do i_fp=1,n_SC_descriptions
       call msg('o '//trim(output_fname),'#',trim(SC_description(i_fp)),INDENT=0)
     enddo
#endif
     if (n_spin==1) then
       titles(1)='E[eV]'
       titles(2)='DOS'
       call msg('o dos','#',titles(:2),INDENT=0,USE_TABS=.true.)    
     else
       titles(1)='E[eV]'
       titles(2)='DOS [up]'
       titles(3)='DOS [dn]'
       titles(4)='DOS [up+dn]'
       call msg('o dos','#',titles(:4),INDENT=0,USE_TABS=.true.)    
     endif
     call msg('o dos',"#")
   endif
   !
   if (dos_E_range(1)>dos_E_range(2)) then
     dos_E_range(1)=minval(Xen%E(dos_bands(1):dos_bands(2),:,:))-5.*dos_broadening
     dos_E_range(2)=maxval(Xen%E(dos_bands(1):dos_bands(2),:,:))-5.*dos_broadening
   endif
   delta_E=(dos_E_range(2)-dos_E_range(1))/dos_E_steps
   !
   dos_norm=1./DL_vol/spin_occ
   if (l_RIM) dos_norm=dos_norm/real(sum(BZ_RIM_nkpt))
   !
   ! Fermi distribution
   !====================
   !
#if defined _YPP_RT
   if (n_T_steps==1) then
     call OCCUPATIONS_IO_and_interpolate(Xen,'G',k=Xk,Time=OCC_T_range(1))
     call RT_occupations_repair(Xen,Xk,.TRUE.)
   endif
#endif
   !
   ! Parallel setup and live_timing
   !================================
   !
   call PARALLEL_index(px,(/dos_E_steps/))
   if (n_T_steps==1) call live_timing('DOS',px%n_of_elements(myid+1))
   if (n_T_steps> 1) call live_timing('DOS',n_T_steps,SERIAL=.true.)
   !
   do i_T=1,n_T_steps
     !
     io_Time=min(OCC_T_range(1)+(i_T-1)*OCC_deltaT,OCC_T_range(2))
#if defined _YPP_RT
     call OCCUPATIONS_IO_and_interpolate(Xen,'G',k=Xk,Time=io_Time)
     call RT_occupations_repair(Xen,Xk,.FALSE.)
#endif
     !
     el_dos(:,:)=0.
     !
     do i_E=1,dos_E_steps
       !
       if (.not.px%element_1D(i_E)) cycle
       !
       dos_E=dos_E_range(1)+i_E*delta_E
       !
       do ib=dos_bands(1),dos_bands(2)
         do i_spin=1,n_sp_pol
           !
           if (l_RIM) then
             do ik=1,nkbz
               do i1=1,BZ_RIM_nkpt(ik)
                 ik_rand=BZ_RIM_table(ik,i1)
                 f_occ=spin_occ
                 if(l_prt_occ_only) f_occ=Xen%f_RIM(ib,ik_rand,i_spin)
#if defined _YPP_RT
                 if (l_TIME_plot) f_occ=(RT_occupations(ib,Xk%sstar(ik,1))-RT_occupations_ref(ib,Xk%sstar(ik,1)))
#endif
                 el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*&
&                      Fermi_fnc_derivative(Xen%E_RIM(ib,ik_rand,i_spin)-dos_E,dos_broadening)*dos_norm
               enddo
             enddo
           else
             do ik=1,nkibz
               f_occ=spin_occ
               if(l_prt_occ_only) f_occ=Xen%f(ib,ik,i_spin)
#if defined _YPP_RT
               if (l_TIME_plot) f_occ=(RT_occupations(ib,ik)-RT_occupations_ref(ib,ik))
#endif
               if(n_spinor==2) then
                 do i_spinor=1,n_spinor
                   el_dos(i_E,i_spinor)=el_dos(i_E,i_spinor)+f_occ*WF_fac(ib,ik,i_spinor)*dos_norm*&
&                        Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)*Xk%weights(ik)
                 enddo
               else
                 el_dos(i_E,i_spin)=el_dos(i_E,i_spin)+f_occ*Xk%weights(ik)*dos_norm*&
&                      Fermi_fnc_derivative(Xen%E(ib,ik,i_spin)-dos_E,dos_broadening)
               endif
             enddo
           endif
           !
         enddo
       enddo
       !
       if (n_T_steps==1) call live_timing(steps=1)
       !
     enddo
     !
     call PP_redux_wait(el_dos)
     !
     el_dos_max=max(el_dos_max,maxval(abs(el_dos)))
     !
     if (l_TIME_plot.and.l_3D_plot) call msg('o dos','')
     !
     ! Output file
     !
     do i_E=1,dos_E_steps
       dos_E=dos_E_range(1)+i_E*delta_E
       if (l_TIME_plot) then
#if defined _YPP_RT
         if (     l_3D_plot) call RT_output("el_dos",VALUEs=(/el_dos(i_E,1)/),E=dos_E*HA2EV,TIME=io_Time/FS2AUT)
#endif
         if (.not.l_3D_plot) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,1)/),INDENT=-2,USE_TABS=.true.)
       else
         if (n_spin==1) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,1)/),INDENT=-2,USE_TABS=.true.)
         if (n_spin==2) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,:),el_dos(i_E,1)+el_dos(i_E,2)/),INDENT=-2,USE_TABS=.true.)
       endif
     enddo
     !
     if (n_T_steps> 1) call live_timing(steps=1)
     !
   enddo
   !
   if (n_T_steps>1) call msg('s','DOS max value  :',el_dos_max)
   !
   if (l_TIME_plot.and.l_3D_plot) then
#if defined _YPP_RT
     call RT_output("close el_dos")
#endif
   else if (.not.l_3D_plot) then
     call of_open_close(trim(output_fname))
   endif
   !
   call live_timing()
   !
 endif
 !
 ! DENSITY
 !=========
 !
 if (l_density) then
   !
   call section('*','Density Plot')
   !
   allocate(el_den(fft_size))
   !
   call el_density(Xen,Xk,el_den,.FALSE.) 
   v2plot=el_den
   !
   ch_ws='density_'//trim(intc(plot_dim))
   if (use_cube) output_fname=trim(ch_ws)//'d.cube'
   if (use_xcrysden) output_fname=trim(ch_ws)//'d.xsf'
   if (use_gnuplot)  output_fname=trim(ch_ws)//'d'
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
     call of_open_close(trim(output_fname),'ot')
     call msg('o den',"#")
   endif
   !
   plot_title='density'
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
 endif
 !
end subroutine
