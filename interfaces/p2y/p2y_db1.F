!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): CH
!
subroutine p2y_db1(en,k)
 !
 use P2Ym,                  ONLY : get_dimensions,get_cell,get_atoms,get_symmetries,get_xc,get_k_points,get_more,get_energies,&
&                                  get_R_vectors,get_IGK
 use electrons,             ONLY : levels
 use com,                   ONLY : msg
 use mod_com2y,             ONLY : print_interface_dimensions
 use xc_functionals,        ONLY : xc_string,GS_xc_FUNCTIONAL,GS_exx_FRACTION,GS_exx_SCREENING
 use R_lattice,             ONLY : ng_vec,g_vec,bz_samp
 use wave_func,             ONLY : wf_ng, wf_nc_k, wf_ncx, wf_igk
 use mod_com2y,             ONLY : force_noWFs
#include<memory.h>
 type(levels),     intent(out)  :: en     ! Energies
 type(bz_samp),    intent(out)  :: k      ! K/Q points
 !---------------------------------------------------------------------*
 !    Read dimensions                                                  *
 !---------------------------------------------------------------------*
 call msg('s','Header/K-points/Energies...')
 !
 call get_dimensions(en,k)
 call msg("l"," done")
 !
 !---------------------------------------------------------------------*
 !    Read cell data                                                   *
 !---------------------------------------------------------------------*
 !
 call msg('s','Cell data...')
 !
 call get_cell
 call msg("l"," done")
 !
 !---------------------------------------------------------------------*
 !    Read atomic data                                                 *
 !---------------------------------------------------------------------*
 !
 call msg('s','Atomic data...')
 !
 call get_atoms
 call msg("l"," done")
 !
 !---------------------------------------------------------------------*
 !    Read symmetries                                                  *
 !---------------------------------------------------------------------*
 !
 call msg('s','Symmetries...')
 !
 call get_symmetries
 !
 !---------------------------------------------------------------------*
 !    Read XC functional                                               *
 !---------------------------------------------------------------------*
 !
 call get_xc
 call msg('s','XC functional...'//xc_string(GS_xc_FUNCTIONAL))
 call msg('s','EXX fraction...',GS_exx_FRACTION)
 call msg('s','EXX screening...',GS_exx_SCREENING)
 !
 !---------------------------------------------------------------------*
 !    Read k-point mesh                                                *
 !---------------------------------------------------------------------*
 !
 call msg('s','K-points mesh...')
 !
 call get_k_points(k)
 !
 call msg('l',' done')
 !
 !---------------------------------------------------------------------*
 !    Read miscellaneous data                                          *
 !---------------------------------------------------------------------*
 !
 call get_more
 !
 if(.not.force_noWFs) then
   !---------------------------------------------------------------------*
   !    Read G-vectors                                                   *
   !---------------------------------------------------------------------*
   !
   call msg('s','RL vectors...')
   !
   call get_R_vectors
   !
   call msg('l',' done')
   !
   !---------------------------------------------------------------------*
   !    Read igk arrays                                                  *
   !---------------------------------------------------------------------*
   call msg('s','IGK arrays...')
   !
   call get_IGK(k)
   !
   call msg('l',' done')
   !
 else
   !
   wf_ng      =1
   ng_vec     =1
   YAMBO_ALLOC(g_vec,(1,3))
   g_vec(1,:)=(/0,0,0/)
   YAMBO_ALLOC(wf_nc_k,(k%nibz))
   wf_nc_k(:)=1
   wf_ncx    =1
   YAMBO_ALLOC(wf_igk,(1,k%nibz))
   wf_igk(1,:)=1
   !
   call warning('IGK array and RL vectors skipped! ')
   !
 endif
 !---------------------------------------------------------------------*
 !    Read eigenvalues                                                 *
 !---------------------------------------------------------------------*
 !
 call msg('s','Energies...')
 !
 call get_energies(en,k)
 !
 call msg('l',' done')
 !
 !---------------------------------------------------------------------*
 !    Report                                                           *
 !---------------------------------------------------------------------*
 call print_interface_dimensions(en,k)
 !
end subroutine p2y_db1
