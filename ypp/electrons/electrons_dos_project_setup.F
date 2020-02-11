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
subroutine electrons_dos_project_setup( k, E)
 !
 use pars,           ONLY:SP
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use vec_operate,    ONLY:cross_product,c2a
 use electrons,      ONLY:levels
 use YPPm,           ONLY:PROJECT_mode,PtsPath,PROJECT_path,&
&                         CIRCUIT_made_of_SPECIAL_k,CIRCUIT_k,PROJECT_grid,n_path_pts,&
&                         l_PROJECT_line,l_PROJECT_plane,CIRCUIT_k_label
 use interpolate,    ONLY:GRID_k
 !
#include<memory.h>
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer  :: ik,n_lines
 real(SP) :: line_vector(10,3),perp_vector(10,3)
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
 call k_define_circuit(k)
 !
 call bz_samp_reset(GRID_k)
 GRID_k%description="PROJECT@"//trim(PROJECT_mode)
 !
 ! Find the versor perpendicular to the circuit
 !
 n_lines=count(len_trim(CIRCUIT_k_label(:))>0)
 write (*,*) n_lines
 stop
 !
 do ik=1,CIRCUIT_k%nbz
   if (len_trim(CIRCUIT_k_label(ik))>0) then
     n_lines=n_lines+1
     if (ik==1) line_vector(n_lines,:)=CIRCUIT_k%pt(ik+1,:)-CIRCUIT_k%pt(ik,:)
     if (ik==1) line_vector(n_lines,:)=CIRCUIT_k%pt(ik+1,:)-CIRCUIT_k%pt(ik,:)
     call c2a(v_in= line_vector(n_lines,:),mode='ki2c')
     if (n_lines>1) then
       perp_vector(n_lines-1,:)=cross_product(line_vector(n_lines,:),line_vector(n_lines-1,:))
       write (*,*) "LINE ",n_lines,"A:", line_vector(n_lines-1,:)
       write (*,*) "LINE ",n_lines,"B:", line_vector(n_lines,:)
       write (*,*) "LINE ",n_lines,"P:", perp_vector(n_lines-1,:)
     endif
   endif
 enddo
 !
 if (l_PROJECT_line) then
   !
   GRID_k%nbz=CIRCUIT_k%nbz*PROJECT_grid(2)
   !
 endif
 !
 stop
 !
end subroutine
