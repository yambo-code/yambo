! 
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine PROJECTION_setup( what ,bands )
 !
 use pars,           ONLY:SP
 use stderr,         ONLY:STRING_same
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:n_sp_pol,E_reset
 use vec_operate,    ONLY:c2a
 use com,            ONLY:msg,of_open_close
 use YPPm,           ONLY:PROJECT_mode,PROJECT_path,CIRCUIT_k,PROJECT_steps,PROJECT_depth,&
&                         l_PROJECT_line,CIRCUIT_k_label,l_PROJECT_plane,SLICE_k,SLICE_E,BANDS_steps,&
&                         PROJECT_plane_X,PROJECT_plane_Y,l_PROJECT_atom,output_fname
 use YPP_interfaces, ONLY:k_special
 use interpolate,    ONLY:GRID_k
 !
 implicit none
 !
 character(*)      :: what
 integer, optional :: bands(2)
 !
 ! Work Space
 !
 integer  :: i_k,n_axis,i_p1,i_p2
 real(SP) :: axis(10,3),O(3),k_trial(3)
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
 if (what=="DOS") then
   !
   call k_special( )
   !
   call k_circuit_driver(PROJECT_path,PROJECT_steps)
   !
 else
   PROJECT_steps=BANDS_steps
 endif
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="PROJECT@"//trim(PROJECT_mode)
 !
 if (PROJECT_depth<0) PROJECT_depth=1
 !
 ! Find the axis
 !===============
 n_axis=count(len_trim(CIRCUIT_k_label(:))>0)
 !
 call PROJECTION_axis( n_axis, axis(:n_axis,:), O )
 !
 ! GRID creation
 !===============
 !
 if (l_PROJECT_line) then
   call PROJECTION_perpendicular_sampling(CIRCUIT_k,axis(n_axis,:))
   call msg("sr","[PROJECT] Direction  _|_ circuit [iku]:",axis(n_axis,:))
   call msg("sr","[PROJECT] Total Points                :",GRID_k%nbz)
 endif
 !
 if (l_PROJECT_plane) then
   !
   if (n_axis>3) call error("Too many axis found to define a plane")
   !
   call msg("sr","[PROJECT] Direction#1 // plane [iku]",axis(1,:))
   call msg("sr","          Direction#2 // plane [iku]",axis(2,:))
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
   enddo
   !
   call of_open_close(trim(output_fname))
   !
   call PROJECTION_perpendicular_sampling(PLANE_k,axis(3,:))
   !
   call msg("sr","[PROJECT] Direction  _|_ plane [iku]:",axis(3,:))
   call msg("sr","[PROJECT] Total Points              :",GRID_k%nbz)
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
   allocate(SLICE_k%weights(PROJECT_depth))
   allocate(SLICE_k%sstar(PROJECT_depth,2))
   allocate(SLICE_E%E(bands(1):bands(2),PROJECT_depth,n_sp_pol))
   allocate(SLICE_E%f(bands(1):bands(2),PROJECT_depth,n_sp_pol))
   SLICE_k%nibz=PROJECT_depth
   SLICE_k%nbz =PROJECT_depth
   SLICE_k%weights=1./float(PROJECT_depth)
   do i_k=1,PROJECT_depth
     SLICE_k%sstar(i_k,1)=i_k
   enddo
 endif
 !
end subroutine
