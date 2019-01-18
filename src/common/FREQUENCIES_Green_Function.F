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
subroutine FREQUENCIES_Green_Function(iqp,W,E,Spectral_Function)
 !
 use pars,       ONLY:SP,cI
 use R_lattice,  ONLY:nkibz
 use electrons,  ONLY:n_bands,spin,n_sp_pol
 use frequency,  ONLY:w_samp
 use QP_m,       ONLY:QP_table,QP_G_dr,QP_G_er,QP_G_zoom_er,GF_energy_range_mode
#if defined _ELPH
 use ELPH,       ONLY:W_debye,QP_PH_n_G_bands
#endif
 implicit none
 !
 integer,      intent(in)    :: iqp
 type(w_samp), intent(inout) :: W
 real(SP),     intent(in)    :: E(n_bands,nkibz,n_sp_pol)
 logical,      intent(in)    :: Spectral_Function
 !
 ! Work Space
 !
#if defined _ELPH
 integer :: iw,n_small_freqs
 real(SP):: wide_range(2),small_step,large_step,running_step
#endif
 real(SP):: Eo
 !
 W%dr=QP_G_dr
 if (allocated(QP_G_zoom_er).and..not.Spectral_Function) then
   W%er=QP_G_zoom_er(iqp,:)
   call FREQUENCIES_setup(W)
 else if (.not.Spectral_Function) then
   Eo=E(QP_table(iqp,1),QP_table(iqp,3),spin(QP_table(iqp,:)))
   W%er=QP_G_er
   if (trim(GF_energy_range_mode)=="centered") W%er=W%er+Eo
   call FREQUENCIES_setup(W)
 else if (Spectral_Function) then
   !
#if defined _ELPH
   !
   ! 50% of the total points are concentrated in the small region around 
   ! the bare pole
   !
   wide_range=(/minval(E(:,:,:))-2.*W_debye,maxval(E(:QP_PH_n_G_bands,:,:))+2.*W_debye/)
   W%er=wide_range
   call FREQUENCIES_setup(W)
   Eo=E(QP_table(iqp,1),QP_table(iqp,3),spin(QP_table(iqp,:)))
   !
   n_small_freqs=W%n_freqs/2
   !
   small_step=( QP_G_er(2)-QP_G_er(1))/real(n_small_freqs)
   large_step=( wide_range(2)-wide_range(1) - ( QP_G_er(2)-QP_G_er(1)) )/real(W%n_freqs-n_small_freqs) 
   running_step=large_step
   !
   do iw=2,W%n_freqs
     !
     W%p(iw)=real(W%p(iw-1))+running_step+cI*aimag(W%p(iw))
     !
     if (real(W%p(iw)) >Eo+QP_G_er(1)) running_step=small_step
     if (real(W%p(iw)) >Eo+QP_G_er(2)) running_step=large_step
     !
   enddo
   !
#endif
   !
 endif
 !
end subroutine
