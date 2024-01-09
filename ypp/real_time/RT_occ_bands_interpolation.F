!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): DS AM
!
subroutine RT_occ_bands_interpolation(en,k,q,n_T_steps)
 !
 use pars,           ONLY:SP,schlen,rZERO
 use parser_m,       ONLY:parser
 use units,          ONLY:AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset,n_spin
 use R_lattice,      ONLY:bz_samp
 use YPPm,           ONLY:BANDS_steps,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_bands,l_dipoles,&
&                         l_PROJECT_line,l_PROJECT_plane,PROJECT_N_par,DIPs_mask
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time,integrated_Nel,integrated_Nhole
 use YPP_interfaces, ONLY:PROJECTION_setup,PROJECTION_plot,k_special
 use stderr,         ONLY:real2ch,intc,STRING_remove,STRING_same
 use RT_control,     ONLY:RT_apply,RT_carriers_object,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,electrons_bands_interpolate,&
&                         INTERP_obj,INTERP_mode,GRID_k
 use LIVE_t,         ONLY:live_timing
 !
#include<memory.h>
 !
 integer,       intent(in)    :: n_T_steps
 type(bz_samp), intent(in)    :: k,q
 type(levels),  intent(inout) :: en
 !
 ! Work space
 !
 type(bz_samp)         :: RT_k
 type(levels)          :: CIRCUIT_E,CIRCUIT_dE,RT_E
 real(SP)              :: Max_occ
 real(SP), allocatable :: integrated_dF_elecs(:,:,:),integrated_dF_holes(:,:,:)
 character(schlen)     :: operation,basename,extension,sec_name
 logical               :: WARN_neg_elecs_df,WARN_pos_holes_df,is_PROJECTED,is_INTERPOLATED,QUIET_interp,&
 &                        plot_STD_style,plot_RT_style,l_update_E
 integer               :: ID_circuit,ID_spin,ID_magn,ID_bands(3),ID_grid,IDs(6)
 integer               :: ib,ik,ik_path,ik_RT,i_RT,i_spin,i_T,nfiles(5),nqnt
 logical, external     :: file_exists
 integer, external     :: PROJECTION_k_on_the_path_or_plane
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 call parser("UPDATE_E",l_update_E)
 call parser("STD_style",plot_STD_style)
 plot_RT_style = .not.plot_STD_style
 !
 ID_circuit=-1
 ID_spin   =-1
 ID_magn   =-1
 ID_bands  =-1
 ID_grid   =-1
 IDs       =-1
 !
 ! Logicals & Sectioning...
 sec_name="Levels"
 is_INTERPOLATED = .not.STRING_same(trim(INTERP_mode),"none")
 is_PROJECTED    =l_PROJECT_line.or.l_PROJECT_plane ! => No Energies Interpolation needed!
 if (l_PROJECT_line  ) sec_name="Line projected"
 if (l_PROJECT_plane ) sec_name="Plane projected"
 if (l_RT_lifetimes  ) sec_name=trim(sec_name)//" Lifetimes"
 if (l_RT_occupations) sec_name=trim(sec_name)//" Occupations"
 if (     is_INTERPOLATED) call section('*','Interpolation and plotting:'//trim(sec_name))
 if (.not.is_INTERPOLATED) call section('*','Plotting:'//trim(sec_name))
 !
 ! Checks...
 if (is_INTERPOLATED.and.all(INTERP_grid>0)) call error("Interpolation of occupations/lifetimes only on a band circuit")
 if (.not.is_INTERPOLATED .and. .not.is_PROJECTED) call error("Interpolation-free only in PROJECT mode")
 if (n_sp_pol>1) call error('Occupations for spin polarized systems not implemented')
 if (l_RT_lifetimes .and. .not.allocated(RT_carriers%NEQ_Lifetime)  ) call error('No scattering => no Lifetimes')
 !
 if (BANDS_bands(1)<RT_bands(1).or.BANDS_bands(2)>RT_bands(2)) then
   BANDS_bands(1)=max(BANDS_bands(1),RT_bands(1))
   BANDS_bands(2)=min(BANDS_bands(2),RT_bands(2))
   call warning("User bands restricted to "//trim(intc(BANDS_bands(1)))//" - "//trim(intc(BANDS_bands(2))))
 endif
 !
 if (file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) BANDS_path= " "
 !
 ! Define the circuit
 !====================
 call k_special( )
 call k_circuit_driver( BANDS_path, BANDS_steps )
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 ! Projection
 !============
 call PROJECTION_setup("setup")
 call PROJECTION_setup( "BANDS", BANDS_bands )
 !
 if (is_PROJECTED) then
   YAMBO_ALLOC(integrated_dF_elecs,(PROJECT_N_par,n_T_steps,n_spin))
   YAMBO_ALLOC(integrated_dF_holes,(PROJECT_N_par,n_T_steps,n_spin))
   integrated_dF_elecs=rZERO
   integrated_dF_holes=rZERO
 endif
 !
 call section('=',"Real-Time components maps")
 !============================================
 !
 ! Map the RT k in RT_k
 call RT_carriers_to_RT_k(RT_k)
 call k_ibz2bz(RT_k,'iku',.TRUE.)
 !
 if (.not.l_update_E) then
   ! Eo in RT_E
   call RT_carriers_to_RT_E(en,RT_E,"Eo")
 endif
 !
 if (.not.is_PROJECTED.and.is_INTERPOLATED) then
   !
   call section('=',"Levels interpolation")
   !
   if (.not.l_update_E) then
     !
     ! Perform the energies interpolation
     !====================================
     call electrons_bands_interpolate(RT_E,RT_k,RT_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
     !
   endif
   !
   ! Perform interpolation of spinorial factors and magnetization
   !==============================================================
   if (n_spinor>1) call electrons_spin_and_magn_interpolate("S",k,CIRCUIT_k,RT_bands,ID_spin,ID_magn)
   !
 endif
 !
 ! Perform the occupations interpolation
 !======================================
 YAMBO_ALLOC(integrated_Nel,(n_T_steps,2))
 YAMBO_ALLOC(integrated_Nhole,(n_T_steps,2))
 integrated_Nel  =rZERO
 integrated_Nhole=rZERO
 !
 WARN_neg_elecs_df=.FALSE.
 WARN_pos_holes_df=.FALSE.
 !
 QUIET_interp=trim(INTERP_mode)=="NN".and.N_T_steps==1
 if (QUIET_interp) call live_timing('Time Interpolation(s)',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,en,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.,skip_interpolation=.TRUE.)
   !
   ! RT_carriers -> RT_E (E components)
   !
   call RT_carriers_to_RT_E(en,RT_E,"E")
   !
   if (l_update_E) then
     !
     ! Perform the energies interpolation
     !====================================
     call electrons_bands_interpolate(RT_E,RT_k,RT_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
     !
   endif
   !
   if (.not.is_PROJECTED) extension="circuit"
   if (     is_PROJECTED) extension="project_grid"
   if (l_RT_occupations)  operation="df_on_"//trim(extension)
   if (l_RT_lifetimes)    operation="W_on_"//trim(extension)
   operation=trim(operation)//" Time "//trim(intc(i_T))//"/"//trim(intc(n_T_steps))
   !
   if (is_INTERPOLATED) then
     if (is_PROJECTED) then
       call INTERPOLATION_driver_seed(trim(operation),ID_grid,RT_bands,RT_k,SEED_RT=RT_carriers,QUIET=QUIET_interp)
       call INTERPOLATION_driver_do(trim(operation),ID_grid,GRID_k)
       if (l_RT_occupations) call EVAL_integrated_occupations(ID_grid)
     else
       call INTERPOLATION_driver_seed(trim(operation),ID_circuit,RT_bands,RT_k,SEED_RT=RT_carriers,QUIET=QUIET_interp)
       call INTERPOLATION_driver_do(trim(operation),ID_circuit,CIRCUIT_k)
       if (l_RT_occupations) call EVAL_integrated_occupations(ID_circuit)
     endif
   endif
   !
   do i_RT=1,RT_carriers%nstates
     ib      =RT_carriers%table(i_RT,1)
     ik      =RT_carriers%table(i_RT,2)
     ik_RT   =RT_carriers%table(i_RT,3)
     i_spin  =RT_carriers%table(i_RT,4)
     ! ... Apply the DIPOLES Mask
     if (l_dipoles) RT_carriers%OCC_el%dN(i_RT)=RT_carriers%OCC_el%dN(i_RT)*DIPs_mask(ib,ik,i_spin)
     !
     if (ib<=RT_E%nbf(i_spin)) integrated_Nhole(i_T,1)=integrated_Nhole(i_T,1)-RT_carriers%OCC_el%dN(i_RT)*RT_k%weights(ik_RT)
     if (ib> RT_E%nbf(i_spin)) integrated_Nel(i_T,1)  =integrated_Nel(i_T,1)+RT_carriers%OCC_el%dN(i_RT)*RT_k%weights(ik_RT)
   enddo
   Max_occ=maxval(abs(RT_carriers%OCC_el%dN))
   !
   if (is_PROJECTED) then
     !
     do ik=1,GRID_k%nbz
       ik_path=PROJECTION_k_on_the_path_or_plane(ik)
       if (ik_path==0) cycle
       do ib=BANDS_bands(1),BANDS_bands(2)
         if (ib<=RT_E%nbf(1)) then
           integrated_dF_holes(ik_path,i_T,:)=integrated_dF_holes(ik_path,i_T,:)-&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         else
           integrated_dF_elecs(ik_path,i_T,:)=integrated_dF_elecs(ik_path,i_T,:)+&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         endif
       enddo
     enddo
     !
   endif
   !
   ! OUTPUT
   !=======
   if (.not.is_PROJECTED) then
     !
     if (plot_RT_style ) then
       call RT_plot_interpolated_values(CIRCUIT_E,RT_E,CIRCUIT_dE,i_T,ID_circuit,ID_spin,&
       &                                         Max_occ,WARN_neg_elecs_df,WARN_pos_holes_df)
     endif
     !
     if (plot_STD_style) then
       IDs=0
       nqnt=1
       IDs(1)=ID_circuit
       nfiles(1)=n_sp_pol
       ! DS it is useless to plot each time the
       !    bands if the energies are not updated
       !    However to activate the following if I would need 
       !    to modify the subroutine plot_interpolated_values
       !if(l_update_E.or.i_T==1) then
         nqnt=nqnt+1
         nfiles(2)=n_sp_pol
         IDs(2)=ID_bands(2)
       !endif
       if(n_spinor==2.and.i_T==1) then
         nqnt=nqnt+2
         nfiles(nqnt-1:nqnt)=(/2,3/)
         IDs(nqnt-1:nqnt)=(/ID_spin,ID_magn/)
       endif
       !
       basename="interpolated_"//STRING_remove(real2ch( RT_time(i_T)*AUT2FS )," ","")
       call plot_interpolated_values(k,en,0._SP,CIRCUIT_k,IDs(1:nqnt),BANDS_bands,nqnt,nfiles(1:nqnt),basename,"bands",i_T)
     endif
     !
   endif
   !
   if (l_update_E) then
     if(ID_bands(1)/=-1) call INTERPOLATION_driver_end(ID_bands(1))
     if(ID_bands(2)/=-1) call INTERPOLATION_driver_end(ID_bands(2))
     if(ID_bands(3)/=-1) call INTERPOLATION_driver_end(ID_bands(3))
   endif
   if (     is_PROJECTED) call INTERPOLATION_driver_end(ID_grid)
   if (.not.is_PROJECTED) call INTERPOLATION_driver_end(ID_circuit)
   !
   if (QUIET_interp) call live_timing(steps=1)
   !
 enddo
 !
 if (QUIET_interp) call live_timing()
 !
 if (WARN_neg_elecs_df) call warning("[INTERPOLATION] Found negative electron occupations")
 if (WARN_pos_holes_df) call warning("[INTERPOLATION] Found positive hole occupations")
 !
 ! Projection Output
 !===================
 !
 if (l_PROJECT_line.or.l_PROJECT_plane) then 
   !
   if (l_RT_lifetimes  ) operation="lifetimes"
   if (l_RT_occupations) operation="occupations"
   ! 
   call PROJECTION_plot(trim(operation)//"_electrons", n_T_steps,n_spin, (/1,1/), OBS_K=integrated_dF_elecs, TIME=RT_time ) 
   call PROJECTION_plot(trim(operation)//"_holes", n_T_steps,n_spin, (/1,1/), OBS_K=integrated_dF_holes, TIME=RT_time ) 
   !
 endif
 !
 ! Clean
 !
 call INTERPOLATION_driver_end(0)
 call RT_carriers_object(RT_carriers,WHAT='free')
 call E_reset(CIRCUIT_E)
 call E_reset(RT_E)
 !
 contains
   !
   subroutine EVAL_integrated_occupations(ID)
     integer  :: ID
     real(SP) :: NK
     !
     if (ID==ID_circuit) NK=real(CIRCUIT_k%nbz)
     if (ID==ID_grid)    NK=real(GRID_k%nbz)
     !
     integrated_Nhole(i_T,2)=-sum(INTERP_obj(ID)%INTERP(RT_bands(1):RT_E%nbf(1),:,:))/NK
     integrated_Nel(i_T,2)  = sum(INTERP_obj(ID)%INTERP(RT_E%nbf(1)+1:RT_bands(2),:,:))/NK
     !
   end subroutine
   !
end subroutine RT_occ_bands_interpolation
