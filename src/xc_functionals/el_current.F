!
! Copyright (C) 2000-2012 D. Sangalli and the YAMBO team
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
subroutine el_current(en,Xk,ik,J,J_diam,l_directions)
 !
 ! Electronic current: j = (q/m)* < [p -   (q/c) *A] >
 !  (q=-1,m=1) ==>       =  -1    < [p + (1/137.)*A] >
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_spin,n_sp_pol,Boltz_f
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:DL_sop
 use FFT_m,         ONLY:fft_size,fft_rot_r
 use wave_func,     ONLY:wf_state,wf,wf_x,wf_y,wf_z
 use units,         ONLY:SPEED_OF_LIGHT
 !
 implicit none
 type(bz_samp)::Xk
 type(levels) ::en       
 !
 integer  :: ik
 logical  :: l_directions(3)
 real(SP) :: J(3,n_sp_pol,fft_size)
 real(SP) :: J_diam(3,n_sp_pol,fft_size)
 !
 !
 !
end subroutine
