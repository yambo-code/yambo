!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine X_checks(Xen,X)
!==========================
 use X_m,           ONLY:X_t,use_X_RIM
 use electrons,     ONLY:levels,BZ_RIM_nbands
 use drivers,       ONLY:l_bs_fxc,l_col_cut
 use R_lattice,     ONLY:bz_samp,nqibz
 use TDDFT,         ONLY:FXC_n_g_corr
 implicit none
 type(X_t)    :: X 
 type(levels) ::Xen

 if (X%iq(1)<0) X%iq(1)=1
 if (X%ib(1)<0) X%ib(1)=1
 if (X%iq(2)<0) X%iq(2)=1
 if (X%ib(2)<0) X%ib(2)=1
!
! In these two lines I ensure the at least all the metallic
! bands are included in the X summation
!
 if (X%ib(1)>Xen%nbf+1) X%ib(1)=Xen%nbf+1
 if (X%ib(2)<Xen%nbm) X%ib(2)=Xen%nbm
!
! Other checks
!
 if (X%iq(2)>nqibz) X%iq(2)=nqibz
 if (X%iq(1)>X%iq(2)) X%iq(1)=X%iq(2)
 if (X%ib(1)<0.or.X%ib(1)>Xen%nbm.or.X%ib(2)<X%ib(1)) X%ib(1)=1
 if (X%ib(2)>Xen%nb.or.X%ib(2)<Xen%nbf) X%ib(2)=Xen%nb
!
! Here  I call the coarse grid generator only if I am not
! going to use the X RIM
!
 use_X_RIM=X%ib(2)<=BZ_RIM_nbands.and.(.not.l_bs_fxc)
!
! Fxc Checks
!
 if (FXC_n_g_corr>X%ng) FXC_n_g_corr=X%ng
!
! In case the columbian is cutoffed a resonant/antiresonant 
! only calculation is meaningless because of the dependence 
! of the alpha from X
!
 if (l_col_cut) then
   if ( trim(X%ordering) == 'a' .or. trim(X%ordering) == 'r' )  X%ordering='t'
 endif
!
 end subroutine
