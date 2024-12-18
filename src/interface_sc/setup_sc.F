!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine setup_sc(en,Xen,Ken,k,Xk)
 !
 use pars,           ONLY:SP,pi,schlen
 use wave_func,      ONLY:WF_buffered_IO 
 use R_lattice,      ONLY:bz_samp
 use drivers,        ONLY:l_sc_run
 use electrons,      ONLY:levels,n_sp_pol
 use IO_m,           ONLY:REP,DUMP,NONE,OP_RD_CL
 use SC,             ONLY:load_SC_components,found_SC_DB,load_SC_components
 use global_XC,      ONLY:G_kind,G_xc_functional,X_kind,&
&                         X_xc_functional,setup_global_XC,K_kind,K_xc_functional,loaded_WF_xc_string,&
&                         X_perturbation,G_perturbation,K_perturbation,WF_kind,WF_xc_functional,WF_perturbation
 !
#include <y_memory.h>
 !
 type(levels)    ::en,Xen,Ken
 type(bz_samp)   ::k,Xk
 !
 ! Work Space
 !
 call setup(en,Xen,Ken,k,Xk)
 !
 ! SC Energies 
 !-------------
 !
 ! First check if it possible to load the SC energies
 !
 call load_SC_components('E',E=en,COM_=REP,MODE_=DUMP,ik=0,&
&                         kind=G_kind,xc_functional=G_xc_functional,perturbation=G_perturbation)
 if (found_SC_DB) call setup_global_XC('G',G_kind,G_xc_functional,G_perturbation)
 call load_SC_components('E',E=Xen,COM_=NONE,MODE_=DUMP,ik=0,&
&                         kind=X_kind,xc_functional=X_xc_functional,perturbation=X_perturbation)
 if (found_SC_DB) call setup_global_XC('X',X_kind,X_xc_functional,X_perturbation)
 if (found_SC_DB.and.WF_buffered_IO) then
   call warning(' SC WFs and WF_buffered not supported. Turning off buffering')
   WF_buffered_IO=.false.
 endif
 !
 K_kind=X_kind
 K_xc_functional=X_xc_functional
 K_perturbation=X_perturbation
 call setup_global_XC('K',K_kind,K_xc_functional,K_perturbation)
 !
 if (.not.l_sc_run) then
   !
   ! SC wavefunctions
   !
   ! Here I only check for the existence of the SC WF DB.
   !
   call load_SC_components('check_WF_DB',n_bands=0,&
&                          kind=WF_kind,xc_functional=WF_xc_functional,perturbation=WF_perturbation)
   !
   if (found_SC_DB) then
     call setup_global_XC('WF',WF_kind,WF_xc_functional,WF_perturbation)
     call warning(trim(loaded_WF_xc_string)//' wavefunctions found')
   endif
   !
 endif
 !
 ! Copy Ho eigenvalues in Xen%Eo for dipoles
 !
 if (l_sc_run .and. .not.allocated(Xen%Eo) .and. found_SC_DB) then
   YAMBO_ALLOC(Xen%Eo,(Xen%nb,Xen%nk,n_sp_pol))
   Xen%Eo(:,:,:)=en%Eo(:,:en%nk,:)
 endif
 !
end subroutine
