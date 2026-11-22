!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details):
!
! Input options that ChASE only accepts through its setters. They act on the
! active solver, so call this between *chase_init* and *chase(...).
!
subroutine CHASE_set_options()
 !
 use iso_c_binding,  ONLY:c_int
 use linear_algebra, ONLY:CHASE_max_deg,CHASE_deg_extra,CHASE_max_iter,&
&                         CHASE_lanczos_iter,CHASE_num_lanczos
 use chase_diag,     ONLY:chase_set_max_deg,chase_set_deg_extra,chase_set_max_iter,&
&                         chase_set_lanczos_iter,chase_set_num_lanczos
 !
 implicit none
 !
 if (CHASE_max_deg>0)      call chase_set_max_deg(int(CHASE_max_deg,c_int))
 if (CHASE_deg_extra>=0)   call chase_set_deg_extra(int(CHASE_deg_extra,c_int))
 if (CHASE_max_iter>0)     call chase_set_max_iter(int(CHASE_max_iter,c_int))
 if (CHASE_lanczos_iter>0) call chase_set_lanczos_iter(int(CHASE_lanczos_iter,c_int))
 if (CHASE_num_lanczos>0)  call chase_set_num_lanczos(int(CHASE_num_lanczos,c_int))
 !
end subroutine CHASE_set_options
