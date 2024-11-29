!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine FREQUENCIES_Green_Function(iqp,W,E)
 !
 use pars,       ONLY:SP,cI
 use R_lattice,  ONLY:nkibz
 use electrons,  ONLY:n_bands,spin,n_sp_pol
 use frequency,  ONLY:w_samp
 use QP_m,       ONLY:QP_table,QP_G_solver,QP_G_dr,QP_G_er,QP_G_ir,&
&                     QP_G_zoom_er,GF_energy_range_mode
 implicit none
 !
 integer,      intent(in)    :: iqp
 type(w_samp), intent(inout) :: W
 real(SP),     intent(in)    :: E(n_bands,nkibz,n_sp_pol)
 !
 ! Work Space
 !
 real(SP):: Eo
 !
 W%dr=QP_G_dr
 if (allocated(QP_G_zoom_er)) then
   W%er=QP_G_zoom_er(iqp,:)
   call FREQUENCIES_setup(W)
 else
   Eo=E(QP_table(iqp,1),QP_table(iqp,3),spin(QP_table(iqp,:)))
   W%er=QP_G_er
   W%ir=QP_G_ir
   W%grid_type=QP_G_solver
   if(QP_G_solver=="1l".or.QP_G_solver=="2l") then
     W%grid_type="mp"
     W%samp_type=QP_G_solver
     W%samp_grid="ho"
   endif
   if (trim(GF_energy_range_mode)=="centered") W%er=W%er+Eo
   call FREQUENCIES_setup(W)
   !
 endif
 !
end subroutine
