!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
module PHOTOLUM
 !
 use BS,         ONLY:l_BS_photolum
 use pars,       ONLY:SP
#include<memory.h>
 !
 real(SP)                 :: PL_prefactor
 !
 ! PL Spectrum
 !
 real(SP),    allocatable :: PL(:,:)
 !
 ! PL direction
 !
 real(SP)                 :: PL_weights(3)
 !
 ! BS dipoles
 !
 complex(SP), allocatable :: BSS_dipoles_PL(:,:)
 complex(SP), allocatable :: BSS_PL_f(:)
 !
 contains
   !
   subroutine PL_init( )
     use pars,       ONLY:pi,rZERO
     use units,      ONLY:SPEED_OF_LIGHT
     use R_lattice,  ONLY:RL_vol,nqbz
     use vec_operate,ONLY:v_norm
     use BS_solvers, ONLY:BSS_n_freqs
     PL_prefactor =32._SP*pi**3*SPEED_OF_LIGHT*2._SP/3._SP*RL_vol/nqbz/(2._SP*pi)**3
     if (allocated(PL))  return
     PL_weights=PL_weights/v_norm(PL_weights)
   end subroutine
   !
   subroutine BSS_PL_alloc( )
     use pars,       ONLY:cZERO,rZERO
     use BS,         ONLY:BS_H_dim,BS_K_dim
     integer :: f_PL_dim
     if (.not.l_BS_photolum) return
     YAMBO_ALLOC(BSS_dipoles_PL,(3,BS_H_dim))
     BSS_dipoles_PL = cZERO
     f_PL_dim=BS_H_dim
     if(BS_H_dim==BS_K_dim(1)) f_PL_dim=2*BS_K_dim(1)
     YAMBO_ALLOC(BSS_PL_f,(BS_H_dim))
     BSS_PL_f = rZERO
   end subroutine
   !
   subroutine BSS_PL_free( )
     if (.not.l_BS_photolum) return
     if (allocated(BSS_dipoles_PL)) then
       YAMBO_FREE(BSS_dipoles_PL)
     endif
     if (allocated(BSS_PL_f)) then
       YAMBO_FREE(BSS_PL_f)
     endif
   end subroutine
   !
end module PHOTOLUM
