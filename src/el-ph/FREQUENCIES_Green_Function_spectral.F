!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine FREQUENCIES_Green_Function_spectral(iqp,W,E)
 !
 use pars,       ONLY:SP,cI
 use R_lattice,  ONLY:nkibz
 use electrons,  ONLY:n_bands,spin,n_sp_pol
 use frequency,  ONLY:w_samp
 use QP_m,       ONLY:QP_table,QP_G_solver,QP_G_dr,QP_G_er,QP_G_ir,&
&                     QP_G_zoom_er,GF_energy_range_mode
 use ELPH,       ONLY:PH_W_debye,QP_PH_n_G_bands
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
 real(SP):: wide_range(2),small_step,large_step,running_step
 real(SP):: Eo
 !
 W%dr=QP_G_dr
   !
   ! 50% of the total points are concentrated in the small region around
   ! the bare pole
   !
   wide_range=(/minval(E(:,:,:))-2.*PH_W_debye,maxval(E(QP_PH_n_G_bands(1):QP_PH_n_G_bands(2),:,:))+2.*PH_W_debye/)
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
 !
end subroutine
