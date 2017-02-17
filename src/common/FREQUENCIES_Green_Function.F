!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine FREQUENCIES_Green_Function(iqp,W,E,Spectral_Function)
 !
 use pars,       ONLY:SP,cI
 use units
 use R_lattice,  ONLY:nkibz
 use electrons,  ONLY:n_bands,spin,n_sp_pol
 use frequency,  ONLY:w_samp
 use QP_m,       ONLY:QP_table,QP_G_dr,QP_G_er,QP_G_zoom_er
 implicit none
 !
 integer,      intent(in)    :: iqp
 type(w_samp), intent(inout) :: W
 real(SP),     intent(in)    :: E(n_bands,nkibz,n_sp_pol)
 logical,      intent(in)    :: Spectral_Function
 !
 ! Work Space
 !
 integer :: iw,n_small_freqs
 real(SP):: wide_range(2),Eo,small_step,large_step,running_step
 !
 W%dr=QP_G_dr
 if (allocated(QP_G_zoom_er).and..not.Spectral_Function) then
   W%er=QP_G_zoom_er(iqp,:)
   call FREQUENCIES_setup(W)
 else if (.not.Spectral_Function) then
   Eo=E(QP_table(iqp,1),QP_table(iqp,3),spin(QP_table(iqp,:)))
   W%er=QP_G_er+Eo
   call FREQUENCIES_setup(W)
 else if (Spectral_Function) then
   !
   !
 endif
 !
end subroutine
