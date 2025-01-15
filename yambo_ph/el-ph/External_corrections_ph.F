!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine External_corrections_ph(en,Xen,Ken,DIPen,Xk,k,X)
 !
 use drivers,       ONLY:l_optics,l_chi,l_em1s,l_em1d,l_acfdt,l_ppa,l_bss,l_gw0,l_bse,l_alda_fxc
 use pars,          ONLY:schlen
 use X_m,           ONLY:X_t
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:OP_IF_START_RD_CL_IF_END,manage_action,NONE,DUMP,OP_RD_CL
 use drivers,       ONLY:l_elph_corr
 use ELPH,          ONLY:GKKP,elph_nQ,elph_branches,QP_PH_n_G_bands
 !
 implicit none
 !
 type(levels) ::en,Xen,Ken,DIPen
 type(X_t)    ::X(5) 
 type(bz_samp)::k,Xk
 !
 ! Work Space
 ! 
 integer  :: iq,i_sp_pol
 integer  :: elph_branches_save(2)
 integer  :: QP_PH_n_G_bands_save(2)
 character(schlen) :: db_name
 !
 integer           :: ID,io_err,io_ELPH_err(2)
 integer, external :: io_ELPH
 !
 logical       ::l_apply_QP_corrections,l_dynamical
 integer       ::X_kind,bands_to_correct(2)
 !
 ! AM, March 2024. When levels are corrected it may happen that the databases used to correct (QP or carriers)
 ! have been produced WITHOUT e-p. In this case the E-P related variables may be erroneously overwritten
 !
 elph_branches_save  =elph_branches
 QP_PH_n_G_bands_save=QP_PH_n_G_bands
 !
 ! Green's Function (k+q levels loaded for the _ELPH)
 !
 if (l_gw0.and.l_elph_corr) then
   !
   ! Check which databases are present
   ! =================================
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
   io_ELPH_err(1)=io_ELPH(ID,'gkkp')
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
   io_ELPH_err(2)=io_ELPH(ID,'gkkp_expanded')
   !
   if(io_ELPH_err(1)==0) then
      db_name='gkkp'
      if(io_ELPH_err(2)==0) call error('Both gkkp and gkkp_expanded databases are present, please remove one of the two!')
   elseif(io_ELPH_err(2)==0.and.io_ELPH_err(1)/=0) then
      db_name='gkkp_expanded'
   endif
   !
   do iq=1,elph_nQ
     call io_control(ACTION=manage_action(OP_IF_START_RD_CL_IF_END,iq,1,elph_nQ),COM=NONE,SEC=(/iq+1/),ID=ID)
     io_err=io_ELPH(ID,db_name//' no_matrix_elements')
     if (iq>0.and.io_err==0) then
       call QP_apply(GKKP%Nb,GKKP%E_kpq(iq),k,"G",msg_fmt='rs',main_section=.TRUE.)
#if defined _RT 
       if (l_apply_RT_corrections) call RT_apply(GKKP%Nb,GKKP%E_kpq(iq),k,what="G",VERBOSE=.true.)
#endif
     endif
   enddo
   !
 endif
 !
 elph_branches  =elph_branches_save
 QP_PH_n_G_bands=QP_PH_n_G_bands_save
 !
end subroutine
