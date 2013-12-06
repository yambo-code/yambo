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
subroutine Plasma_driver(E,k,q)
 !
 use pars,          ONLY:SP,schlen
 use stderr,        ONLY:intc
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:n_atoms
 use SC,            ONLY:SC_bands
 use electrons,     ONLY:levels,n_sp_pol
 use plasma,        ONLY:Plasma_alloc,Plasma_redux_percent,max_N_plasma_poles,Eh_gas
 use QP_m,          ONLY:QP_n_states,QP_nb,QP_state
 use parallel_m,    ONLY:PAR_nPlasma,PAR_nQ,PAR_IND_Q,PAR_Q_index,PP_redux_wait,PAR_COM_Q_A2A
 use interfaces,    ONLY:PARALLEL_global_indexes
 !
 implicit none
 !
 type(levels)  :: E 
 type(bz_samp) :: k,q
 !
 ! Work space
 !
 character(schlen)      :: ch
 integer                :: max_N_poles,iqbz,i_q_mem,n_poles,iplasma
 real(SP), allocatable  :: poles(:)
 !
 integer, external      :: Plasma_setup
 !
 call section('*','Plasma gas')
 !=======================================
 !Procedure copied from collisions_eval
 if(allocated(QP_state)) deallocate(QP_state)
 QP_nb=SC_bands(2)
 call QP_state_table_setup(E)
 !
 call k_sym2sym(q,'k')
 call k_sym2sym(k,'k')
 !
 max_N_poles=k%nbz*n_sp_pol*(SC_bands(2)-SC_bands(1)+1)**2
 EH_gas(:)%max_N_poles=max_N_poles
 !
 ! PARALLEL Setup
 !================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(E,k,q,"Real_Time")
 !
 do iplasma=1,1
   !
   call Plasma_alloc(EH_gas(iplasma),(/q%nbz,PAR_nQ,QP_n_states/))
   allocate(poles(max_N_poles))
   poles=0._SP
   !
   ! Plasma approximation level frim input
   if(iplasma==1) EH_gas(1)%redux_percent=Plasma_redux_percent
   ! I should obtain about the same number of poles as the number of phonons
   if(iplasma==2) EH_gas(2)%redux_percent=100._SP                           &
&    *real(n_atoms,SP)*3._SP*real(q%nbz,SP)/real(sum(EH_gas(1)%N_poles),SP) &
&    /EH_gas(1)%redux_percent*100._SP
   !
   do iqbz=1,q%nbz
     !
     if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
     i_q_mem=PAR_Q_index(iqbz)
     !
     n_poles=max_N_poles
     !
     ! Define the poles @ iq, count the real number
     n_poles=Plasma_setup(-iqbz,E,k,q,poles,n_poles,EH_gas(iplasma))
     !
     ! Group the poles
     call FREQUENCIES_coarse_grid('COLL',poles(:n_poles),n_poles,EH_gas(iplasma)%redux_percent)
     !
     ! Create the Poles_tab with the sorted index
     n_poles=Plasma_setup( iqbz,E,k,q,poles(:n_poles),n_poles,EH_gas(iplasma))
     !
   enddo
   !
   deallocate(poles)
   !
   call PP_redux_wait(EH_gas(iplasma)%N_poles,COMM=PAR_COM_Q_A2A%COMM)
   EH_gas(iplasma)%N_poles=EH_gas(iplasma)%N_poles/(PAR_COM_Q_A2A%n_CPU)
   EH_gas(iplasma)%max_N_poles=maxval(EH_gas(iplasma)%N_poles)
   !
   if(iplasma==1) max_N_Plasma_poles=EH_gas(iplasma)%max_N_poles
   !
 enddo
 !
 ! PARALLEL Setup
 !================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(E,k,q,"Real_Time",EH_gas=EH_gas(1))
 !
 do iplasma=1,1
   write (ch,'(3a)') "[PLASMA #"//trim(intc(iplasma))//"] Poles reduction:"//&
&        trim(intc(k%nbz*(SC_bands(2)-SC_bands(1)+1)**2*n_sp_pol*q%nbz))//" => "//&
&        trim(intc(sum(EH_gas(iplasma)%N_poles)))
   call msg('rs',trim(ch))
 enddo
 !
end subroutine Plasma_driver
