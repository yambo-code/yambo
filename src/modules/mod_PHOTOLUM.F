!
!        Copyright (C) 2000-2018 the YAMBO team
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
module PHOTOLUM
 !
 use drivers,    ONLY:l_photolum
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
 complex(SP), allocatable :: BSS_PL_f(:,:)
 !
 contains
   !
   subroutine PL_init( )
     !
     use pars,       ONLY:pi,rZERO
     use units,      ONLY:SPEED_OF_LIGHT
     use R_lattice,  ONLY:RL_vol,nqbz
     use vec_operate,ONLY:v_norm
     use BS_solvers, ONLY:BSS_n_freqs
     !
     integer :: ic
     !
     PL_prefactor =32._SP*pi**3*SPEED_OF_LIGHT*2._SP/3._SP*RL_vol/nqbz/(2._SP*pi)**3
     !
     if (allocated(PL))  return
     !
     YAMBO_ALLOC(PL,(BSS_n_freqs,3))!2(int)3(nonint)
     PL = rZERO
     PL_weights=PL_weights/v_norm(PL_weights)
     !
   end subroutine
   !
   subroutine BSS_PL_alloc( )
     !
     use pars,       ONLY:cZERO,rZERO
     use BS,         ONLY:BS_H_dim,BS_K_dim
     !
     if (.not.l_photolum) return
     !
     YAMBO_ALLOC(BSS_dipoles_PL,(BS_H_dim,3))
     BSS_dipoles_PL = cZERO
     !
     YAMBO_ALLOC(BSS_PL_f,(2,BS_K_dim))
     BSS_PL_f = rZERO
     !
   end subroutine
   !
   subroutine BSS_PL_free( )
     !
     if (.not.l_photolum) return
     !
     if (allocated(BSS_dipoles_PL)) then
       YAMBO_FREE(BSS_dipoles_PL)
     endif
     if (allocated(BSS_PL_f)) then
       YAMBO_FREE(BSS_PL_f)
     endif
     !
   end subroutine
   !
end module PHOTOLUM
