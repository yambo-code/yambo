! 
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): CA DS AM
!
subroutine electrons_bands_interpolate(Xen,Xk,BANDS_range,ID_bands,CIRCUIT_E,GRID_E,CIRCUIT_dE)
 !
 use pars,           ONLY:SP
 use YPPm,           ONLY:BANDS_steps,INTERP_grid,CIRCUIT_k,BANDS_from_db,&
&                         CIRCUIT_E_db,l_PROJECT_plane,l_PROJECT_line,INTERP_input_mode
 use electrons,      ONLY:levels,n_sp_pol
#if defined _YPP_RT
 use YPP_real_time,  ONLY:l_RT_occupations,l_RT_lifetimes
#endif
 use R_lattice,      ONLY:bz_samp
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,GRID_k
 !
 implicit none
 !
 type(bz_samp), intent(in)  :: Xk
 type(levels),  intent(in)  :: Xen
 type(levels),  intent(out) :: CIRCUIT_E
 integer     ,  intent(in)  :: BANDS_range(2)
 integer     ,  intent(out) :: ID_bands(3)
 type(levels),  intent(out), optional :: CIRCUIT_dE
 type(levels),  intent(out), optional :: GRID_E
 !
 ! Work space
 !
 logical               :: l_interp_dE,NEAREST,GRID
 real(SP)              :: delta_align
 integer               :: ib,ik,i_sp_pol
 !
 NEAREST=.FALSE.
 if(trim(INTERP_input_mode)=='NN') NEAREST=.TRUE.
 GRID   =.FALSE.
 !
 ID_bands=-1
 !
 if (any(BANDS_range<=0))                call error(" Wrong bands range")
 !
 if (BANDS_range(2)-BANDS_range(1)+1>40) call error(" YPP cannot interpolate more than 40 bands. Reduce bands range")
 !
 l_interp_dE= (allocated(Xen%Eo).and.BANDS_from_db)
 !
 ! Interpolation
 !===============
 ! 1: interpolate on the circuit      if (BANDS_steps>0)
 ! 2: interpolate on the regular grid if (interp_grid>0)
 !
 if (all(INTERP_grid>0)               ) GRID=.TRUE.
#if defined _YPP_RT
 if (l_PROJECT_plane.or.l_PROJECT_line) GRID=.not.l_RT_occupations.and..not.l_RT_lifetimes
#else
 if (l_PROJECT_plane.or.l_PROJECT_line) GRID=.TRUE.
#endif
 !
 if(GRID) then
   !
   if (.not.l_PROJECT_plane.and..not.l_PROJECT_line) call INTERPOLATION_create_the_grid(INTERP_grid)
   call INTERPOLATION_driver_seed("E_INTERP_grid",ID_bands(1),BANDS_range,Xk,SEED_E=Xen,NEAREST=NEAREST)
   call INTERPOLATION_driver_do("E_INTERP_grid",ID_bands(1),GRID_k,OUT_E=GRID_E)
   !
 endif
 !
 if (BANDS_steps> 0) then
   !
   call INTERPOLATION_driver_seed("E_K_PATH",ID_bands(2),BANDS_range,Xk,SEED_E=Xen,NEAREST=NEAREST)
   call INTERPOLATION_driver_do("E_K_PATH",ID_bands(2),CIRCUIT_k,OUT_E=CIRCUIT_E)
   !
   if (l_interp_dE) then
     call INTERPOLATION_driver_seed("dE_K_PATH",ID_bands(3),BANDS_range,Xk,SEED_E=Xen,NEAREST=NEAREST)
     call INTERPOLATION_driver_do("dE_K_PATH",ID_bands(3),CIRCUIT_k,OUT_E=CIRCUIT_dE)
   endif
   !
 endif
 !
 ! Alignement
 !============
 !
 if (allocated(CIRCUIT_E_db%E)) then
   do i_sp_pol=1,n_sp_pol
     delta_align=CIRCUIT_E%E(BANDS_range(1),1,i_sp_pol)-CIRCUIT_E_db%E(BANDS_range(1),1,i_sp_pol)
     do ib=BANDS_range(1),BANDS_range(2)
       do ik=1,CIRCUIT_k%nbz
         CIRCUIT_E_db%E(ib,ik,i_sp_pol)=CIRCUIT_E_db%E(ib,ik,i_sp_pol)+delta_align
       enddo
     enddo
   enddo
 endif
 !
end subroutine electrons_bands_interpolate
