!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine K_inversion_engine(iw_2start,iw_2do,px,W,Lo,Lo_dim,do_it_full)
 !
 use pars,           ONLY:SP
 use parallel_m,     ONLY:PP_indexes
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:Epsilon_ii,global_gauge
 use LIVE_t,         ONLY:live_timing
 use R_lattice,      ONLY:q_norm
 use BS,             ONLY:BS_mat,BS_H_dim,Co_factor,BSS_INV_err,BSS_dipoles
 !
 implicit none
 !
 integer          :: iw_2start,iw_2do,Lo_dim
 type(PP_indexes) :: px
 type(w_samp)     :: W
 complex(SP)      :: Lo(BS_H_dim,W%n_freqs,Lo_dim)
 logical          :: do_it_full
 !
 ! Work Space
 !
 integer :: i2,iw
 logical :: perform_inv
 complex(SP)      ::gauge_factor
 integer, external::MATRIX_resolvant_via_perturbative_inversion
 integer, external::MATRIX_resolvant_via_exact_inversion
 !
 do i2=1,iw_2do
   !
   iw=i2+iw_2start-1
   !
   ! The parallel loop is controlled in the manin body loop 
   ! when a full inversion is performed
   !
   if (.not.do_it_full) then
     if(.not.px%element_1D(iw)) then
       BSS_inv_err(iw)=0
       cycle
     endif
   endif
   !
   if (     do_it_full) perform_inv= BSS_INV_err(iw)/= 0
   if (.not.do_it_full) perform_inv= BSS_INV_err(iw)==-1
   !
   if (trim(global_gauge)=='length'  ) gauge_factor=-Co_factor/q_norm(1)**2
   if (trim(global_gauge)=='velocity') gauge_factor=-Co_factor/ W%p(iw) **2
   !       
   if (perform_inv) then
     !
     if (do_it_full) then
       BSS_INV_err(iw)=MATRIX_resolvant_via_exact_inversion(BS_H_dim,Lo_dim,BSS_dipoles,gauge_factor,&
&                                                           Epsilon_ii(iw,2),Lo(:,iw,:Lo_dim),BS_mat)
     else
       BSS_INV_err(iw)=MATRIX_resolvant_via_perturbative_inversion(BS_H_dim,Lo_dim,BSS_dipoles,gauge_factor,&
&                                                                  Epsilon_ii(iw,2),Lo(:,iw,:Lo_dim),BS_mat)
     endif
     !
   endif       
   !
   if (.not.do_it_full) call live_timing(steps=1)
   !
 enddo
 !
 if (.not.do_it_full) call live_timing()
 !
end subroutine K_inversion_engine
