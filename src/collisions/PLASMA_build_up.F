!
! Copyright (C) 2000-2013 D. Sangalli and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine PLASMA_build_up(E,k,q)
 !
 use pars,          ONLY:SP,schlen
 use stderr,        ONLY:intc
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:n_atoms
 use SC,            ONLY:SC_bands
 use electrons,     ONLY:levels,n_sp_pol
 use plasma,        ONLY:Plasma_alloc,Plasma_redux_percent,N_plasma_poles_global,EH_gas,N_plasma_poles_CPU_resolved
 use QP_m,          ONLY:QP_n_states
 use parallel_m,    ONLY:PAR_nQ,PAR_IND_Q,PAR_Q_index,PP_redux_wait,PAR_COM_Q_index
 !
 implicit none
 !
 type(levels)  :: E 
 type(bz_samp) :: k,q
 !
 ! Work space
 !
 character(schlen)      :: ch
 integer                :: iqbz,i_q_mem,n_poles,iplasma,N_poles_bare
 real(SP), allocatable  :: poles(:)
 integer, external      :: PLASMA_tables_and_dimensions
 !
 call k_sym2sym(q,'k')
 call k_sym2sym(k,'k')
 !
 N_poles_bare                = k%nbz*n_sp_pol*(SC_bands(2)-SC_bands(1)+1)**2
 N_plasma_poles_global       = N_poles_bare
 N_plasma_poles_CPU_resolved = N_plasma_poles_global
 !
 do iplasma=1,1
   !
   call Plasma_alloc(EH_gas(iplasma),(/q%nbz,PAR_nQ,QP_n_states/))
   allocate(poles(N_plasma_poles_global))
   poles=0._SP
   !
   ! Plasma approximation level from input
   if (iplasma==1) EH_gas(1)%redux_percent=Plasma_redux_percent
   ! I should obtain about the same number of poles as the number of phonons
   if (iplasma==2) EH_gas(2)%redux_percent=100._SP*                        &
&    real(n_atoms,SP)*3._SP*real(q%nbz,SP)/real(sum(EH_gas(1)%N_poles),SP) &
&    /EH_gas(1)%redux_percent*100._SP
   !
   do iqbz=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_index(iqbz)
     !
     ! Define the poles @ iq, count the real number
     n_poles=PLASMA_tables_and_dimensions(-iqbz,E,k,q,poles,N_plasma_poles_global,EH_gas(iplasma))
     !
     ! Group the poles
     call FREQUENCIES_coarse_grid('COLL',poles(:n_poles),n_poles,EH_gas(iplasma)%redux_percent)
     !
     ! Create the Poles_tab with the sorted index
     n_poles=PLASMA_tables_and_dimensions( iqbz,E,k,q,poles(:n_poles),n_poles,EH_gas(iplasma))
     !
   enddo
   !
   deallocate(poles)
   !
   N_plasma_poles_CPU_resolved=maxval(EH_gas(iplasma)%N_poles)
   call PP_redux_wait(EH_gas(iplasma)%N_poles,COMM=PAR_COM_Q_index%COMM)
   N_plasma_poles_global =maxval(EH_gas(iplasma)%N_poles)
   !
 enddo
 !
 do iplasma=1,1
   write (ch,'(3a)') "[PLASMA #"//trim(intc(iplasma))//"] Poles reduction (ALL q):"//&
&        trim(intc(N_poles_bare))//" => "//trim(intc(N_plasma_poles_CPU_resolved))
   call msg('rs',trim(ch))
 enddo
 !
end subroutine PLASMA_build_up
