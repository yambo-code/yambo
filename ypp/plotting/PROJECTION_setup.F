! 
!        Copyright (C) 2000-2019 the YAMBO team
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
 use vec_operate,    ONLY:cross_product,c2a,normalize_v,v_is_zero,iku_v_norm
 use com,            ONLY:msg
 use YPPm,           ONLY:PROJECT_mode,PtsPath,PROJECT_path,&
&                         CIRCUIT_made_of_SPECIAL_k,CIRCUIT_k,PROJECT_steps,PROJECT_depth,n_path_pts,&
&                         l_PROJECT_line,CIRCUIT_k_label,l_PROJECT_plane,SLICE_k,SLICE_E,BANDS_steps,&
&                         PROJECT_plane_X,PROJECT_plane_Y,l_PROJECT_atom
 use interpolate,    ONLY:GRID_k
 !
 implicit none
 !
 character(*)      :: what
 integer, optional :: bands(2)
 !
 ! Work Space
 !
 integer  :: i_k,n_lines,i_l,i_p1,i_p2
 real(SP) :: line_vector(10,3),perp_vector(10,3),versor(3),k_trial(3),k_trial_bz(3),O(3)
 type(bz_samp) :: PLANE_k
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
   PtsPath=PROJECT_path
   n_path_pts=PROJECT_steps
   !
   call k_special(.FALSE.)
   !
   CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
   !
   call k_define_circuit( )
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
 ! Find the versor perpendicular to the circuit...
 !================================================
 n_lines=count(len_trim(CIRCUIT_k_label(:))>0)
 !
 ! ...line versors...
 i_l=0
 K_LOOP: do i_k=1,CIRCUIT_k%nbz-1
   if (len_trim(CIRCUIT_k_label(i_k))>0) then
     i_l=i_l+1
     line_vector(i_l,:)=CIRCUIT_k%pt(i_k+1,:)-CIRCUIT_k%pt(i_k,:)
     line_vector(i_l,:)=line_vector(i_l,:)/iku_v_norm( line_vector(i_l,:) )
     if (i_l==n_lines-1) exit K_LOOP
   endif
 enddo K_LOOP
 !
 ! ...perp versors...
 do i_l=1,n_lines-2
   call c2a(v_in= line_vector(i_l,:),mode='ki2c')
   perp_vector(i_l,:)=normalize_v( cross_product(line_vector(i_l+1,:),line_vector(i_l,:)))
   call c2a(v_in= line_vector(i_l,:),mode='kc2i')
   if (i_l==1) then
     versor=perp_vector(1,:)
   else
     if (.not.v_is_zero( cross_product(versor,perp_vector(i_l,:)) )) then
       call error("More than one perpendicular direction found")
     endif
   endif
 enddo
 !
 !...
 call msg("sr","[PROJECT] Direction [cc]",versor)
 call msg("r", "                   [iku]",versor)
 !
 ! GRID creation
 !===============
 !
 if (l_PROJECT_line) call PROJECTION_perpendicular_sampling(CIRCUIT_k,versor)
 !
 if (l_PROJECT_plane) then
   !
   O=CIRCUIT_k%pt(PROJECT_steps+1,:)
   PROJECT_plane_X=line_vector(1,:)
   PROJECT_plane_Y=line_vector(2,:)
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
     enddo
   enddo
   !
   call PROJECTION_perpendicular_sampling(PLANE_k,versor)
   !
 endif
 !
 allocate(GRID_k%weights(GRID_k%nbz))
 GRID_k%weights=1./float(GRID_k%nbz)
 GRID_k%nibz=GRID_k%nbz
 !
 call msg("sr","          Total Points  ",GRID_k%nbz)
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
 !
end subroutine
