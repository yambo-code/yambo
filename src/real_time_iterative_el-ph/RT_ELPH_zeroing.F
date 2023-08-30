!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine RT_ELPH_zeroing()
 !
 ! The use of the THETA matrix is tricky when the run is parallel.
 ! The point is that every CPU evolves a certain group of q-points.
 ! If I do an all-to-all or a global loading from the DB 
 ! each CPU will have non-zero values for q-points that it will not evolve. During the run,
 ! therefore, values relative to different times will be wrongly merged.
 !
 use drivers,        ONLY:l_elph_scatt
 use pars,           ONLY:cZERO
 use R_lattice,      ONLY:nqbz
 use RT_iterative,   ONLY:THETA_matrix,THETA_matrix_index
 use ELPH,           ONLY:ph_modes
 use parallel_m,     ONLY:ncpu
 implicit none
 !
 ! Work Space
 !
 integer :: iq,il,i_s
 !
 if (.not.l_elph_scatt.or.ncpu==1) return
 !
 do iq=1,nqbz
   do il=1,ph_modes
     do i_s=1,2
       THETA_matrix(THETA_matrix_index(iq,il,i_s),:,:,:) = cZERO
     enddo
   enddo
 enddo
 !
end subroutine RT_ELPH_zeroing
