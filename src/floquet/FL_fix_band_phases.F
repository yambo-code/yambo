!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG IA
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
subroutine FL_fix_band_phases(FL_V_bands,nbf,iter_num)
 !
 use pars,           ONLY:SP,cONE,cZERO,cI,pi,rZERO
 use units,          ONLY:HA2EV
 use com,            ONLY:msg
 use QP_m,           ONLY:QP_nk
 use fields,         ONLY:Efield
 use linear_algebra, ONLY:DIAGO
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 use wrapper,        ONLY:M_by_V,Vstar_dot_V
 use nl_optics,      ONLY:NL_damping,NL_bands
 use fl_optics,      ONLY:max_fl_mode,n_tot_fl_modes,FL_states
 !
 implicit none
 !
 complex(SP), intent(inout) :: FL_V_bands(NL_bands(1):NL_bands(2),n_tot_fl_modes,nbf)
 integer,     intent(in) :: nbf,iter_num
 !
 ! Work Space
 !
 integer     :: i_m,i1,i2,i1_fl,i2_fl,i1_fks,i2_fks,i3_fks,i1_deg,i2_deg,i_central_fks
 real(SP)    :: local_phase_V_bands(NL_bands(1):NL_bands(2),n_tot_fl_modes,nbf)
 complex(SP) :: sum_over_fl(NL_bands(1):NL_bands(2),n_tot_fl_modes,nbf)
 !
 do i_m=NL_bands(1),nbf
   !
   do i1=NL_bands(1),NL_bands(2)
     !
     do i1_fl=1,n_tot_fl_modes
       !
      !if (i1 > nbf) then
      !FL_V_bands(i1,i1_fl,4) = FL_V_bands(i1,i1_fl,4) * exp(+1*cI*pi/3._SP) *0._SP
      !endif
      !print*, i1, i1_fl-(max_fl_mode+1), FL_V_bands(i1,i1_fl,4), atan2(aimag(FL_V_bands(i1,i1_fl,4)),real(FL_V_bands(i1,i1_fl,4)))
       !
     enddo
     !
   enddo
   !
 enddo
 !
 !
 !
 !
 !
 !
 !
 !
 !
 !
end subroutine
