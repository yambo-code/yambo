!
! License-Identifier: GPL
!
! Copyright (C) 2013 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine PLASMA_build_up(E,k,q)
 !
 use pars,           ONLY:SP,schlen
 use stderr,         ONLY:intc
 use com,            ONLY:msg
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,n_sp_pol
 use plasma,         ONLY:PLASMA_alloc,PLASMA_redux_percent,N_plasma_poles_global,EH_gas
 use collision_ext,  ONLY:COLL_bands,GW_NEQ_collisions
 use parallel_m,     ONLY:PAR_nQ_bz,PAR_IND_Q_bz,PAR_Q_bz_index,PAR_COM_Q_INDEX
 use parallel_int,   ONLY:PP_redux_wait
 use timing_m,       ONLY:timing
 implicit none
 !
 type(levels)  :: E 
 type(bz_samp) :: k,q
 !
 ! Work space
 !
 character(schlen)      :: ch
 integer                :: iqbz,i_q_mem,n_poles,N_poles_bare
 real(SP), allocatable  :: poles(:)
 integer, external      :: PLASMA_tables_and_dimensions
 !
 call timing('PLASMA_build_up',OPR='start')
 !
 call k_build_up_BZ_tables(q)
 call k_build_up_BZ_tables(k)
 !
 N_poles_bare          = k%nbz*n_sp_pol*(COLL_bands(2)-COLL_bands(1)+1)**2
 N_plasma_poles_global = N_poles_bare
 EH_gas%max_N_poles    = N_poles_bare
 !
 call PLASMA_alloc(EH_gas,(/q%nbz,PAR_nQ_bz,GW_NEQ_collisions%N/))
 allocate(poles(N_poles_bare))
 poles=0._SP
 !
 ! Plasma approximation level from input
 EH_gas%redux_percent=PLASMA_redux_percent
 !
 do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q_bz%element_1D(iqbz)) cycle
   i_q_mem=PAR_Q_bz_index(iqbz)
   !
   ! Define the poles @ iq, count the real number
   n_poles=PLASMA_tables_and_dimensions(-iqbz,E,k,q,poles,N_poles_bare)
   !
   ! Group the poles
   call FREQUENCIES_coarse_grid('COLL',poles(:n_poles),n_poles,EH_gas%redux_percent,(/0.0_SP/),.FALSE.)
   !
   ! Create the Poles_tab with the sorted index
   n_poles=PLASMA_tables_and_dimensions( iqbz,E,k,q,poles(:n_poles),n_poles)
   !
 enddo
 !
 deallocate(poles)
 !
 EH_gas%max_N_poles=maxval(EH_gas%N_poles)
 call PP_redux_wait(EH_gas%N_poles,COMM=PAR_COM_Q_INDEX%COMM)
 N_plasma_poles_global =maxval(EH_gas%N_poles)
 !
 write (ch,'(3a)') "[PLASMA] Poles reduction (ALL q): "//&
&      trim(intc(N_poles_bare*q%nbz))//" => "//trim(intc(sum(EH_gas%N_poles)))
 call msg('rs',trim(ch))
 !
 call timing('PLASMA_build_up',OPR='stop')
 !
end subroutine PLASMA_build_up
