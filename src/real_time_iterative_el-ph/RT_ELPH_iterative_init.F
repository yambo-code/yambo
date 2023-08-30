!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_ELPH_iterative_init()
 !
 use pars,           ONLY:cZERO
 use RT_lifetimes,   ONLY:l_RT_iterative_ELPH_lifetimes
 use RT_iterative,   ONLY:THETA_matrix,THETA_matrix_index
 use R_lattice,      ONLY:nqbz,nXkibz
 use real_time,      ONLY:RT_bands
 use ELPH,           ONLY:ph_modes
 !
#include<memory.h>
 !
 ! Work Space
 !
 integer             ::iq,i_counter,il,i_s
 !
 if (.not.l_RT_iterative_ELPH_lifetimes) return
 !
 YAMBO_ALLOC(THETA_matrix,(2*nqbz*ph_modes,RT_bands(2)-RT_bands(1)+1,RT_bands(2)-RT_bands(1)+1,nXkibz))
 THETA_matrix=cZERO
 YAMBO_ALLOC(THETA_matrix_index,(nqbz,ph_modes,2))
 !
 i_counter=0
 do iq=1,nqbz
   do il=1,ph_modes
     do i_s=1,2
       i_counter=i_counter+1
       THETA_matrix_index(iq,il,i_s)=i_counter
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_iterative_init
