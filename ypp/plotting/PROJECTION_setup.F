! 
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine PROJECTION_setup( what ,bands )
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:STRING_same
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:n_sp_pol,E_reset
 use vec_operate,    ONLY:c2a
 use com,            ONLY:msg,of_open_close
 use YPPm,           ONLY:PROJECT_mode,PROJECT_path,CIRCUIT_k,PROJECT_steps,PROJECT_N_perp,&
&                         l_PROJECT_line,l_PROJECT_plane,SLICE_k,SLICE_E,BANDS_steps,&
&                         PROJECT_plane_X,PROJECT_plane_Y,l_PROJECT_atom,output_fname,PROJECT_N_par
 use YPP_interfaces, ONLY:k_special
 use interpolate,    ONLY:GRID_k
 use LIVE_t,         ONLY:live_timing
 !
 implicit none
 !
 character(*)      :: what
 integer, optional :: bands(2)
 !
 ! Work Space
 !
 integer  :: i_k,i_p1,i_p2
 real(SP) :: axis(3,3),O(3),k_trial(3)
 type(bz_samp) :: PLANE_k
 !
 if (trim(PROJECT_mode)=="none") return
 !
 if (what=="setup") then
   l_PROJECT_atom =STRING_same(trim(PROJECT_mode),"atom")
   l_PROJECT_line =STRING_same(trim(PROJECT_mode),"line")
   l_PROJECT_plane=STRING_same(trim(PROJECT_mode),"plane")
   return
 endif
 !
 call section('p',what//' projection:'//trim(PROJECT_mode))
 !
 if (what=="DOS".or.what=="DIPOLES") then
   call k_special( )
   call k_circuit_driver(PROJECT_path,PROJECT_steps)
 else
   PROJECT_steps=BANDS_steps
 endif
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="PROJECT@"//trim(PROJECT_mode)
 !
 ! Find the axis
 !===============
 call PROJECTION_axis( axis, O )
 !
 ! GRID creation
 !===============
 if (l_PROJECT_line) then
   PROJECT_N_par=CIRCUIT_k%nbz
   call PROJECTION_perpendicular_sampling(CIRCUIT_k,axis(3,:))
 endif
 !
 if (l_PROJECT_plane) then
   !
   call msg("sr","[PROJECT] Direction#1 // plane      ",axis(1,:),"[iku]")
   call msg("sr","          Direction#2 // plane      ",axis(2,:),"[iku]")
   call msg("sr","          Direction  _|_ plane      ",axis(3,:),"[iku]")
   call msg("sr","          Plane Origin              ",O,"[iku]")
   !
   call live_timing('[PROJECT] Points output file',PROJECT_steps+1)
   !
   output_fname="plane_points"
   !
   call of_open_close(trim(output_fname),'ot')
   !
   call msg('o plane','#',(/"kx [cc]","ky [cc]","kz [cc]"/),INDENT=0,USE_TABS=.true.)    
   call msg('o plane',"#")
   !
   PROJECT_plane_X=axis(1,:)
   PROJECT_plane_Y=axis(2,:)
   call c2a(v_in= PROJECT_plane_X,mode='ki2c')
   call c2a(v_in= PROJECT_plane_Y,mode='ki2c')
   !
   allocate(PLANE_k%pt((PROJECT_steps+1)**2,3))
   PLANE_k%nbz=0
   !
   do i_p1=1,PROJECT_steps+1
     do i_p2=PROJECT_steps+1,2*PROJECT_steps+1
       k_trial=CIRCUIT_k%pt(i_p1,:)+CIRCUIT_k%pt(i_p2,:)-O
       PLANE_k%nbz=PLANE_k%nbz+1
       PLANE_k%pt(PLANE_k%nbz,:)=k_trial
       call c2a(v_in= k_trial,mode='ki2c')
       call msg('o plane',' ',k_trial,INDENT=0,USE_TABS=.true.)
     enddo
     call live_timing(steps=1)
   enddo
   !
   PROJECT_N_par=PLANE_k%nbz
   !
   call of_open_close(trim(output_fname))
   !
   call live_timing()
   !
   call PROJECTION_perpendicular_sampling(PLANE_k,axis(3,:))
   !
   call bz_samp_reset(PLANE_k)
   !
 endif
 !
 allocate(GRID_k%weights(GRID_k%nbz))
 GRID_k%weights=1./float(GRID_k%nbz)
 GRID_k%nibz=GRID_k%nbz
 !
 if (what=="DOS") then
   !
   ! Allocation
   !============
   call E_reset(SLICE_E)
   call bz_samp_reset(SLICE_k)
   allocate(SLICE_k%weights(PROJECT_N_perp))
   allocate(SLICE_k%sstar(PROJECT_N_perp,2))
   allocate(SLICE_E%E(bands(1):bands(2),PROJECT_N_perp,n_sp_pol))
   allocate(SLICE_E%f(bands(1):bands(2),PROJECT_N_perp,n_sp_pol))
   SLICE_k%nibz=PROJECT_N_perp
   SLICE_k%nbz =PROJECT_N_perp
   SLICE_k%weights=1./float(PROJECT_N_perp)
   do i_k=1,PROJECT_N_perp
     SLICE_k%sstar(i_k,1)=i_k
   enddo
 endif
 !
end subroutine
