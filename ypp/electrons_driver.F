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
subroutine electrons_driver(Xk,Xen)
 !
 use pars,           ONLY:SP,schlen,lchlen
 Use stderr,         ONLY:intc
 use wave_func,      ONLY:WF_load,wf,WF_free,wf_ng,wf_state
 use FFT_m,          ONLY:fft_size,fft_dim
 use R_lattice,      ONLY:bz_samp,nkibz
 use electrons,      ONLY:levels,n_spin,n_spinor,spin,n_sp_pol
 use QP_m,           ONLY:QP_table,QP_n_states,QP_state
 use YPP,            ONLY:l_density,l_mag,v2plot,output_fname,plot_dim,use_xcrysden,&
&                         use_gnuplot,use_cube,nr,l_sp_wf,deg_energy,mag_dir,l_norm_to_one,&
&                         plot_title,l_dos,l_angular_momentum,&
&                         l_position,l_bands,BANDS_range
 use com,            ONLY:msg,of_open_close,warning
 use functions,      ONLY:Fermi_fnc_derivative
 use xc_functionals, ONLY:magn
#if defined _YPP_ELPH
 use YPP,            ONLY:elph_residual_treshold
 use ELPH,           ONLY:elph_Ham_bands
 use com,            ONLY:jobstr,file_exists
#endif
 implicit none
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 !
 real(SP), allocatable :: el_den(:)
 integer               :: i_qp,ik,ib,ibp,ir,i_wf,i_wfp,i_spin,mag_i_dir,nb_to_load(2),nkpt_to_load(2),ik_ref,iq_ibz,iq_s
 logical               :: flag,l_polaronic_wf
 character(lchlen)     :: ch_ws(2)
 !
#if defined _YPP_ELPH
 !
 !Polaronic Wf
 !
 integer, parameter      :: n_max_res=100
 integer                 :: n_top_residuals,top_residual(n_max_res),i_st,H_size,i_c
 character(lchlen)       :: db_name
 integer                 :: ok_ibz,il,ok_s
 real(SP)                :: polaronic_residual(n_max_res),max_residual
 complex(SP)             :: B_n_ok
 complex(SP), allocatable:: v2plot_c(:),wf_rot(:)
 !
#endif
 !
 flag=any((/l_mag,l_density,l_sp_wf,l_dos,l_bands/))
 !===
 !
 if (.not.flag) return
 !
 call plot_check_and_launch(.true.)
 !
 ! Loading  Wf 
 !
 nb_to_load=(/1,Xen%nbm/)
 nkpt_to_load=(/1,Xk%nibz/)
 !
 flag=l_sp_wf
 !===
 !
 if (l_bands) nb_to_load=BANDS_range
 !
 if (flag) then
   call QP_state_table_setup(Xen)
   nb_to_load   =(/minval(QP_table(:,1)),maxval(QP_table(:,1))/)
   nkpt_to_load =(/minval(QP_table(:,3)),maxval(QP_table(:,3))/)
#if defined _YPP_ELPH
   l_polaronic_wf=.FALSE.
   db_name=trim(jobstr)//'/TOP_RESIDUAL_b'//trim(intc(QP_table(1,1)))//'_k'//trim(intc(QP_table(1,3)))
   if (file_exists(trim(db_name))) then
     l_polaronic_wf=.TRUE.
     open(unit=100,file=trim(db_name),form='unformatted')
     read(100) elph_Ham_bands
     close(100)
     nb_to_load    =elph_Ham_bands
     nkpt_to_load  =(/1,nkibz/)
   endif
#endif
 endif
 !
 flag=.not.l_dos
 !===
 !
 if(flag) then
   call WF_load(wf_ng,1,nb_to_load,nkpt_to_load,space='R',title='-WF',impose_free_and_alloc=.TRUE.)
   nr=fft_dim
   allocate(v2plot(fft_size))
#if defined _YPP_ELPH
   if (l_polaronic_wf) allocate( v2plot_c(fft_size), wf_rot(fft_size) )
#endif
 endif
 !
 !
 ! DOS & DENSITY
 !===============
 !
 if (l_dos.or.l_density)  call electrons_dos_and_charge(Xk,Xen)
 !
 ! BANDS interpolation
 !======================
 !
 if(l_bands) call electrons_bands(Xk,Xen)
 !
 ! SYMMETRIZED WAVEFUNCTIONS (summed over all symmetries and degenerate states)
 !==============================================================================
#if defined _YPP_ELPH
 if (l_polaronic_wf) then
   !
   i_qp=1
   !
   l_norm_to_one=.true.
   !
   do while (i_qp<=QP_n_states) 
     !
     ib    =QP_table(i_qp,1)
     ik    =QP_table(i_qp,3)
     i_spin=spin(QP_table(i_qp,:))
     !
     db_name=trim(jobstr)//'/TOP_RESIDUAL_b'//trim(intc(ib))//'_k'//trim(intc(ik))
     !
     i_qp=i_qp+1
     !
     if (.not.file_exists(trim(db_name))) cycle
     !  
     do i_st=1,2
       n_top_residuals=1
       open(unit=100,file=trim(db_name),form='unformatted')
       do while(.true.)
         read(100,end=2) top_residual(n_top_residuals),polaronic_residual(n_top_residuals)
         if  (polaronic_residual(n_top_residuals)>elph_residual_treshold/100.*max_residual) n_top_residuals=n_top_residuals+1
       enddo
2      n_top_residuals=n_top_residuals-1
       close(100)
       if (i_st==1) max_residual=maxval(polaronic_residual(:n_top_residuals))
     enddo 
     ! 
     call msg('s',':: Found '//trim(intc(n_top_residuals))//' residuals@k'//trim(intc(ik))//&
&                 '_b'//trim(intc(ib))//':', top_residual(:n_top_residuals))
     !
     do i_st=1,n_top_residuals
       !
       if  (polaronic_residual(i_st)<elph_residual_treshold/100.*maxval(polaronic_residual(:n_top_residuals))) cycle
       !
       v2plot_c=cmplx(0.,SP)
       !
       db_name=trim(jobstr)//'/EIGENV_I'//trim(intc(top_residual(i_st)))//'_k'//trim(intc(ik))
       open(unit=100,file=trim(db_name),form='unformatted')
       read(100) H_size
       do i_c=1,H_size
         read(100) ibp,ok_ibz,ok_s,il,iq_ibz,iq_s,B_n_ok
         !
         if (il==0) then
           !
           i_wf=wf_state(ib,ik,1)
           !
           forall(ir=1:fft_size) v2plot_c(ir)=v2plot_c(ir)+wf(ir,i_wf)
           !
         else
           !
           call WF_apply_symm((/ibp,ok_ibz,ok_s,1/),wf_rot)
           !
           forall(ir=1:fft_size) v2plot_c(ir)=v2plot_c(ir)+B_n_ok*wf_rot(ir)
           !
         endif
         !
       enddo
       close(100)
       !
       v2plot  =abs(real(v2plot_c))
       !
       ch_ws(1)='sp_wf_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim))//&
&               '_STATE'//trim(intc(top_residual(i_st)))//'_'//trim(intc(plot_dim))
       if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
       !
       call of_open_close(trim(output_fname),'ot')
       call msg('o wf',"#")
       !
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     enddo
     !
   enddo
   !
 endif
 !
#endif
 !
 ik_ref=-1
 ch_ws(2)='sp_wf'
 !
 if (l_sp_wf &
#if defined _YPP_ELPH
&    .and..not.l_polaronic_wf &
#endif
&   ) then
   !
   v2plot=0.
   !
   if (l_sp_wf)          call section('*','Single Particle wavefunction Plot')
   !
   !
   i_qp=1
   !
   do while (i_qp<=QP_n_states) 
     !
     ! n   =QP_table(i_qp,1)
     ! k   =QP_table(i_qp,3)
     ! sp  =QP_table(i_qp,4)
     !
     ib    =QP_table(i_qp,1)
     ik    =QP_table(i_qp,3)
     i_spin=spin(QP_table(i_qp,:))
     !
     i_qp=i_qp+1
     !
     !
       i_wf=wf_state(ib,ik,i_spin)
       if (n_spinor==2) i_wfp=wf_state(ib,ik,2)
     !
       forall(ir=1:fft_size) v2plot(ir)=real( wf(ir,i_wf)*conjg( wf(ir,i_wf) ) )
       if (n_spinor==2) forall(ir=1:fft_size) v2plot(ir)=v2plot(ir) + real( wf(ir,i_wfp)*conjg( wf(ir,i_wfp) ) )
     !
     ibp=ib+1
     if (ib==Xen%nb) ibp=ib
     if (ib/=Xen%nb.and.abs(Xen%E(ib,ik,i_spin)-Xen%E(ibp,ik,i_spin))<deg_energy) then
       cycle
     else
       !
       if (n_sp_pol==2) then
         if (i_spin==1) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_UP_'//trim(intc(plot_dim))
         if (i_spin==2) ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_DN_'//trim(intc(plot_dim))
         if (i_spin==1) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' UP'
         if (i_spin==2) plot_title='k '//trim(intc(ik))//' b '//trim(intc(ib))//' DN'
       else
         ch_ws(1)=trim(ch_ws(2))//'_k'//trim(intc(ik))//'_b'//trim(intc(ib))//'_'//trim(intc(plot_dim)) 
       endif
       !
       if (use_cube) output_fname=trim(ch_ws(1))//'d.cube'
       if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
       !
       if (use_cube) then 
         call of_open_close(trim(output_fname),'o')
       else
         call of_open_close(trim(output_fname),'ot')
         call msg('o wf',"#")
       endif
       !
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     endif
     !
   enddo
    !
 endif
 !
 ! MAGNETIZATION 
 !===============
 !
 if (l_mag) then
   !
   mag_i_dir=-1
   if (mag_dir=='X'.or.mag_dir=='x') mag_i_dir=1
   if (mag_dir=='Y'.or.mag_dir=='y') mag_i_dir=2
   if (mag_dir=='Z'.or.mag_dir=='z') mag_i_dir=3
   if (mag_i_dir<0) goto 1
   !
   allocate(magn(fft_size,3))
   !
   call section('*','Single Particle Magnetization along '//mag_dir)
   !
   call el_magnetization(Xen,Xk)
   !
   v2plot=magn(:,mag_i_dir)
   !
   if (use_cube) output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d.cube'
   if (use_xcrysden) output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d.xsf'
   if (use_gnuplot)  output_fname='mag_'//trim(mag_dir)//'_'//trim(intc(plot_dim))//'d'
   !
   l_norm_to_one=.false.
   !
   if (use_cube) then 
     call of_open_close(trim(output_fname),'o')
   else
     call of_open_close(trim(output_fname),'ot')
     call msg('o mag',"#")
   endif
   !
   plot_title='magnetization'
   !
   call plot_check_and_launch(.false.)
   !
   call of_open_close(trim(output_fname))
   !
   if (n_sp_pol==2.and.l_density) then
     !
     call section('*','Spin Polarized densities')
     !
     do i_spin=1,2
       !
       ! rho DN
       if (i_spin==1) then
         v2plot=(el_den(:)-magn(:,3))/2.
         ch_ws(1)='density_DN_'//trim(intc(plot_dim))
       else
         !
         ! rho UP
         v2plot=(el_den(:)+magn(:,3))/2.
         ch_ws(1)='density_UP_'//trim(intc(plot_dim))
       endif
       !
       if (use_cube) output_fname=trim(ch_ws(1))//'d.cube'
       if (use_xcrysden) output_fname=trim(ch_ws(1))//'d.xsf'
       if (use_gnuplot)  output_fname=trim(ch_ws(1))//'d'
       !
       if (use_cube) then 
         call of_open_close(trim(output_fname),'o')
       else
         call of_open_close(trim(output_fname),'ot')
         call msg('o density_UP density_DN',"#")
       endif
       !
       if (i_spin==1) plot_title='den DN'
       if (i_spin==2) plot_title='den UP'
       call plot_check_and_launch(.false.)
       !
       call of_open_close(trim(output_fname))
       !
     enddo
     !
   endif
   !
   !
 endif
 !
 !
1 continue
 call WF_free()
 if (allocated(v2plot))   deallocate(v2plot)
 if (allocated(QP_table)) deallocate(QP_table,QP_state)
 if (allocated(magn))     deallocate(magn)
 if (allocated(el_den))   deallocate(el_den)
#if defined _YPP_ELPH 
 if (allocated(v2plot_c)) deallocate(v2plot_c)
#endif
 plot_title=' '
 !
end subroutine
