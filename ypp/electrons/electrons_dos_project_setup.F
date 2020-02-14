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
subroutine electrons_dos_project_setup( )
 !
 use zeros,          ONLY:k_iku_zero
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use vec_operate,    ONLY:cross_product,c2a,normalize_v,v_is_zero,k2bz
 use com,            ONLY:msg
 use YPPm,           ONLY:PROJECT_mode,PtsPath,PROJECT_path,&
&                         CIRCUIT_made_of_SPECIAL_k,CIRCUIT_k,PROJECT_grid,n_path_pts,&
&                         l_PROJECT_line,CIRCUIT_k_label,l_PROJECT_plane
 use interpolate,    ONLY:GRID_k
 !
 implicit none
 !
 ! Work Space
 !
 integer  :: i_k,n_lines,i_l,i_k_line,i_k_versor,Nk_versor_direction,n_versor_ticks
 real(SP) :: line_vector(10,3),perp_vector(10,3),versor(3),k_trial(3),k_trial_bz(3)
 !
 call section('+','DOS projection:'//trim(PROJECT_mode))
 !
 PtsPath=PROJECT_path
 n_path_pts=PROJECT_grid(1)
 !
 call k_special(.FALSE.)
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 call k_define_circuit( )
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="PROJECT@"//trim(PROJECT_mode)
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
     line_vector(i_l,:)=normalize_v(CIRCUIT_k%pt(i_k+1,:)-CIRCUIT_k%pt(i_k,:))
     call c2a(v_in= line_vector(i_l,:),mode='ki2c')
     if (i_l==n_lines-1) exit K_LOOP
   endif
 enddo K_LOOP
 !
 ! ...perp versors...
 do i_l=1,n_lines-2
   perp_vector(i_l,:)=normalize_v( cross_product(line_vector(i_l+1,:),line_vector(i_l,:)))
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
 call c2a(v_in= line_vector(i_l,:),mode='kc2i')
 call msg("r", "                   [iku]",versor)
 !
 ! GRID creation
 !===============
 !
 if (l_PROJECT_line) then
   !
   GRID_k%nbz=CIRCUIT_k%nbz*PROJECT_grid(2)
   GRID_k%nibz=GRID_k%nbz
   allocate(GRID_k%pt(GRID_k%nbz,3))
   allocate(GRID_k%weights(GRID_k%nbz))
   !
   LINE_LOOP: do i_k_line=1,CIRCUIT_k%nbz
     Nk_versor_direction=1
     i_k=(i_k_line-1)*PROJECT_grid(2)+1
     GRID_k%pt(i_k,:)=CIRCUIT_k%pt(i_k_line,:)
     GRID_k%weights(i_k)=1./float(GRID_k%nbz)
     n_versor_ticks=7
     do while(Nk_versor_direction<PROJECT_grid(2)) 
       n_versor_ticks=n_versor_ticks*2
       do i_k_versor=1,n_versor_ticks   
         k_trial=CIRCUIT_k%pt(i_k_line,:)+versor*(2.*float(i_k_versor+1)/float(n_versor_ticks)-1.)
         call k2bz(v_in=k_trial,v_out=k_trial_bz)
         if (.not.v_is_zero(k_trial-k_trial_bz,zero_=k_iku_zero)) cycle
         Nk_versor_direction=Nk_versor_direction+1
         i_k=(i_k_line-1)*PROJECT_grid(2)+Nk_versor_direction
         GRID_k%pt(i_k,:)=k_trial
         GRID_k%weights(i_k)=1./float(GRID_k%nbz)
         if (Nk_versor_direction==PROJECT_grid(2)) then
           cycle LINE_LOOP
         endif
       enddo
     enddo
   enddo LINE_LOOP
   !
 endif
 !
 if (l_PROJECT_plane) then
   !
   GRID_k%nbz=PROJECT_grid(1)**2*PROJECT_grid(2)
   !
 endif
 !
end subroutine
