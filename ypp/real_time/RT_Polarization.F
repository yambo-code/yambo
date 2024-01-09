!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM 
!
subroutine RT_Polarization(en,k,q)
 !
 use pars,           ONLY:cZERO,SP,schlen,rZERO
 use units,          ONLY:HA2EV,AUT2FS
 use D_lattice,      ONLY:DL_vol
 use YPPm,           ONLY:DIPs_kind
 use YPP_real_time,  ONLY:RT_time,Pol,RT_conf,gimme_a_G_Time_Point,RT_pol_mode
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use LIVE_t,         ONLY:live_timing
 use stderr,         ONLY:STRING_same
 use RT_control,     ONLY:Gless_RESTART_RT_IO_t,RT_apply,RT_output
 use real_time,      ONLY:RT_nk,RT_bands,dG_lesser,RT_P,RT_carriers
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:DUMP,OP_RD_CL,REP,NONE,manage_action,OP_IF_START_RD_CL_IF_END
 use parallel_m,     ONLY:PAR_IND_DIPk_ibz,PAR_IND_CON_BANDS_DIP,PAR_IND_VAL_BANDS_DIP,PAR_DIPk_ibz_index
 use parallel_int,   ONLY:PP_redux_wait
 use DIPOLES,        ONLY:DIP_iR
 use functions,      ONLY:Fermi_fnc_derivative
 !
#include<memory.h>
 !
 type(levels)  :: en
 type(bz_samp) :: k,q
 !
 ! Work Space
 !
 integer               :: ID,io_err,i_T,ik,IO_ACT,i_n,i_m,i_s,i_sp_pol,i_RT_n,i_RT_m,i_RT,ic,iE,COM,ik_mem
 integer, external     :: io_RT_components
 real(SP)              :: io_Time,delta_f,delta_E,trans_weight,P_w(RT_conf%W_n_steps),P(RT_conf%W_n_steps,3)
 character(schlen)     :: file_name
 !
 call section('+','Real-Time Polarization analyzer: '//trim(RT_pol_mode)) 
 !=======================================================================
 !
 ! File name
 !-----------
 if (STRING_same(RT_pol_mode,"transitions")) then
   file_name="TD_P_decomposition"
   call  RT_output("open "//trim(file_name))
   P_w(1)=RT_conf%X_W_range(1)
   do iE=2,RT_conf%W_n_steps
     P_w(iE)=RT_conf%X_W_range(1)+real(iE-1,SP)*(RT_conf%X_W_range(2)-RT_conf%X_W_range(1))/real(RT_conf%W_n_steps-1)
   enddo
 endif
 !
 ! I/O...
 !--------
 if (STRING_same(RT_pol_mode,"slice")) then
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1,2/),ID=ID) 
   io_err=io_RT_components('G_lesser',ID)
   if(io_err/=0) then
     call warning(" History of G_lesser not found. Impossible to re-build the Polarization")
     return
   endif
 endif
 !
 ! Dipoles
 !---------
 DIPs_kind="LOAD"
 call DIPOLE_ypp_driver(k,en,q)
 !
 ! Alloc
 !-------
 if (STRING_same(RT_pol_mode,"slice")) then
   YAMBO_ALLOC(dG_lesser,(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2),RT_nk,1))
   dG_lesser=cZERO
   !
   ! Time conf
   !-----------
   YAMBO_FREE(RT_time)
   call RT_time_configuration_setup('Glesser reset',Gless_RESTART_RT_IO_t)
 endif
 !
 ! Loop
 !------
 !
 do i_T=1,RT_conf%T_n_steps
   !
   if (STRING_same(RT_pol_mode,"transitions")) then
     !
     io_Time=RT_time(i_T)
     !
     COM=NONE
     if (i_T==1) COM=REP
     call RT_apply(RT_bands,en,k,Time=io_Time,keep_RT_carriers=.TRUE.,IO_COM=COM,skip_interpolation=.TRUE.)
     !
   endif
   if (i_T==1) call live_timing("Time-loop",RT_conf%T_n_steps)
   !
   if (STRING_same(RT_pol_mode,"slice")) then
     !
     call gimme_a_G_Time_Point(i_T)
     !
#if defined _PAR_IO
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/3/),ID=ID)
     io_err=io_RT_components('G_lesser',ID)
#else
     do ik = 1, RT_nk 
       IO_ACT=manage_action(OP_IF_START_RD_CL_IF_END,ik,1,RT_nk)
       call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/ik+2/),ID=ID)
       io_err=io_RT_components('G_lesser',ID)
     enddo
#endif
     !
     RT_P=cZERO
     !
   endif
   if (STRING_same(RT_pol_mode,"transitions")) P=rZERO
   !
   do i_sp_pol=1,n_sp_pol
     do i_n=RT_bands(1),RT_bands(2)
       if (.not.PAR_IND_VAL_BANDS_DIP%element_1D(i_n)) cycle
       do i_m=RT_bands(1),RT_bands(2)
         if (.not.PAR_IND_CON_BANDS_DIP%element_1D(i_m)) cycle
         if (i_n==i_m) cycle
         K_LOOP: do ik=1,k%nibz
           if (.not.PAR_IND_DIPk_ibz%element_1D(ik)) cycle
           ik_mem=PAR_DIPk_ibz_index(ik)
           if (STRING_same(RT_pol_mode,"transitions")) then
             do i_RT=1,RT_carriers%nstates
               if (RT_carriers%table(i_RT,2)==ik.and.RT_carriers%table(i_RT,4)==i_sp_pol) then
                 if (RT_carriers%table(i_RT,1)==i_n) i_RT_n=i_RT
                 if (RT_carriers%table(i_RT,1)==i_m) i_RT_m=i_RT
               endif
             enddo
             delta_f=RT_carriers%OCC_el%N_ref(i_RT_m)+RT_carriers%OCC_el%dN(i_RT_m)
             delta_f=delta_f-RT_carriers%OCC_el%N_ref(i_RT_n)-RT_carriers%OCC_el%dN(i_RT_n)
             delta_E=RT_carriers%E_bare(i_RT_m)-RT_carriers%E_bare(i_RT_n)
             if (delta_E<RT_conf%X_W_range(1).or.delta_E>RT_conf%X_W_range(2)) cycle K_LOOP
             do ic=1,3
               trans_weight=delta_f*abs(DIP_iR(ic,i_n,i_m,ik_mem,i_sp_pol))**2.
               do iE=1,RT_conf%W_n_steps
                 P(iE,ic)=P(iE,ic)+trans_weight*Fermi_fnc_derivative(delta_E-P_w(iE),RT_conf%damp_factor)
               enddo
             enddo
           endif
           if (STRING_same(RT_pol_mode,"slice")) then
             RT_P=RT_P+DIP_iR(ic,i_n,i_m,ik_mem,i_sp_pol)*dG_lesser(i_m,i_n,ik,1)/DL_vol*k%weights(ik)
           endif
         enddo K_LOOP
       enddo
     enddo
   enddo
   !
   if (STRING_same(RT_pol_mode,"transitions")) then
     call PP_redux_wait(P)
     do iE=1,RT_conf%W_n_steps
       call RT_output(trim(file_name),VALUEs=(/P(iE,:)/),E=P_w(iE)*HA2EV,TIME=RT_time(i_T)*AUT2FS)
     enddo
     call RT_output("space "//trim(file_name))
   else
     call PP_redux_wait(P)
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 ! CLEAN
 YAMBO_FREE(dG_lesser)
 if (STRING_same(RT_pol_mode,"transitions")) call  RT_output("close "//trim(file_name))
 !
end subroutine
