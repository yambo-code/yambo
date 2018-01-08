!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM PM
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
subroutine SCATTERING_QED_transverse_matrix(i_k,i_q_bz,i_G,i_G0,k,q,T_matrix,T_vec,Photon_E)
 !
 ! T_matrix(i,j) = \delta_{i,j} - q_photon(i)*q_photon(j)/q_photon^2
 ! when q_photon -> 0, T_matrix(i,j) = \delta_{i,j}
 !
 use units,               ONLY:SPEED_OF_LIGHT
 use pars,                ONLY:SP,pi
 use D_lattice,           ONLY:alat
 use R_lattice,           ONLY:g_vec,bz_samp
 use vec_operate,         ONLY:v_norm
 !
 implicit none
 !
 real(SP)                  ::T_matrix(3,3),T_vec(3),Photon_E
 type(bz_samp), intent(in) ::q,k
 integer,       intent(in) ::i_q_bz,i_G,i_G0,i_k
 !
 ! WorkSpace
 !
 integer  ::i_c,i_cp
 real(SP) ::q_photon(3)
 !
 do i_c=1,3
   T_vec(i_c)    = (2._SP*k%pt(i_k,i_c)+g_vec(i_G0,i_c))*2._SP*pi/alat(i_c)-q%ptbz(i_q_bz,i_c) 
   q_photon(i_c) = q%ptbz(i_q_bz,i_c) + g_vec(i_g,i_c)*2._SP*pi/alat(i_c)
 enddo
 !
 Photon_E=SPEED_OF_LIGHT*v_norm(q_photon)
 !
 T_matrix(:,:) = 0._SP
 !
 do i_c=1,3
   T_matrix(i_c,i_c)=1._SP
 enddo
 !
 if (i_q_bz==1.and.i_G==1) return
 !
 do i_c=1,3
   do i_cp=i_c,3           
     T_matrix(i_c,i_cp)= T_matrix(i_c,i_cp)-q_photon(i_c)*q_photon(i_cp)/v_norm(q_photon)**2
     T_matrix(i_cp,i_c)= T_matrix(i_c,i_cp)
   enddo
 enddo
 !
end subroutine SCATTERING_QED_transverse_matrix
