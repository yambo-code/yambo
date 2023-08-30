!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine INIT_QP_ctl_switch(what)
 !
 use electrons,  ONLY:n_sp_pol
 !
 implicit none
 !
 character(1)::what
 !
 ! Occupations...
 !
 call initactivate(1,what//'fnFdb')
 !
 ! QP database ...
 !
 call initactivate(1,what//'fnQPdb '//what//'fnQP_INTERP_NN '//what//'fnQP_INTERP_shells ')
 !
 ! DbGd extension
 !
 call initactivate(1,what//'fnQP_DbGd_INTERP_mode')
 !
 ! Stretching
 !
 if (n_sp_pol==1) then
   call initactivate(1,what//'fnQP_E '//what//'fnQP_Z')   
   call initactivate(1,what//'fnQP_Wv_E '//what//'fnQP_Wv '//what//'fnQP_Wv_dos ')
   call initactivate(1,what//'fnQP_Wc_E '//what//'fnQP_Wc '//what//'fnQP_Wc_dos ')
 else
   call initactivate(1,what//'fnQP_up_E '//what//'fnQP_up_Z')   
   call initactivate(1,what//'fnQP_up_Wv_E '//what//'fnQP_up_Wv '//what//'fnQP_up_Wv_dos ')
   call initactivate(1,what//'fnQP_up_Wc_E '//what//'fnQP_up_Wc '//what//'fnQP_up_Wc_dos ')
   call initactivate(1,what//'fnQP_dn_E '//what//'fnQP_dn_Z')   
   call initactivate(1,what//'fnQP_dn_Wv_E '//what//'fnQP_dn_Wv '//what//'fnQP_dn_Wv_dos ')
   call initactivate(1,what//'fnQP_dn_Wc_E '//what//'fnQP_dn_Wc '//what//'fnQP_dn_Wc_dos ')
 endif
 !
end subroutine
