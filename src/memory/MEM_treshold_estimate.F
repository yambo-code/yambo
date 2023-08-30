!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine MEM_treshold_estimate( )
 !
 use pars,         ONLY:SP
 use units,        ONLY:Kilobyte,Megabyte
 use electrons,    ONLY:n_spin,n_bands
 use R_lattice,    ONLY:nkibz
 use wave_func,    ONLY:wf_ng
 use memory,       ONLY:MEM_treshold
 !
 implicit none
 !
 ! In Kb!
 !
 MEM_treshold=max(int(n_spin*nkibz*n_bands*wf_ng*2.*SP/Kilobyte/10.),int(50.*Megabyte/Kilobyte))
 !
end subroutine
