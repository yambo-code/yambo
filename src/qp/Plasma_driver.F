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
 use memory_m,      ONLY:mem_est
 use com,           ONLY:msg
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym
 use SC,            ONLY:SC_bands
 use electrons,     ONLY:levels,n_sp_pol
 use plasma,        ONLY:Plasma_tab,Plasma_tab_m1,N_plasma_poles,&
&                        Plasma_grid,Max_N_plasma_poles,&
&                        Plasma_redux_percent
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
 integer                :: Nt,iqbz,i_q_mem,n_poles
 real(SP), allocatable  :: poles(:)
 !
 !integer, allocatable   :: local_grid(:,:,:)
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
 ! PARALLEL Setup
 !================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(E,k,q,"Real_Time")
 !
 Nt=(SC_bands(2)-SC_bands(1)+1)**2
 ! This would work with E_center>0 and is=+1 / -1
 !Nt=(SC_bands(2)-SC_bands(1)+1)*(SC_bands(2)-SC_bands(1))/2
 allocate(poles(k%nbz*Nt*n_sp_pol),N_plasma_poles(q%nbz))
 allocate(Plasma_tab(k%nbz*Nt*n_sp_pol,PAR_nQ,2))
 allocate(Plasma_tab_m1(QP_n_states,nsym,PAR_nQ))
 call mem_est("Plasma_tab Plasma_tab_m1",(/size(Plasma_tab),size(Plasma_tab_m1)/),(/SP,SP/))
 !
 do iqbz=1,q%nbz
   !
   if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
   i_q_mem=PAR_Q_index(iqbz)
   !
   ! Define the poles @ iq
   n_poles=Plasma_setup(-iqbz,E,k,q,poles)
   !
   ! Group the poles
   call FREQUENCIES_coarse_grid('COLL',poles,n_poles,Plasma_redux_percent)
   !
   ! Create the Poles_tab with the sorted index
   n_poles=Plasma_setup( iqbz,E,k,q,poles)
   !
 enddo
 !
 call PP_redux_wait(N_plasma_poles)
 N_plasma_poles=N_plasma_poles/(PAR_COM_Q_A2A%n_CPU)
 max_N_plasma_poles=maxval(N_plasma_poles)
 !
 ! PARALLEL Setup
 !================
 call PARALLEL_global_indexes(E,k,q," ",RESET=.TRUE.)
 call PARALLEL_global_indexes(E,k,q,"Real_Time")
 !
 !allocate(local_grid(max_N_plasma_poles,PAR_nQ,2))
 !do iqbz=1,q%nbz
 !  if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
 !  i_q_mem=PAR_Q_index(iqbz)
 !  local_grid(1:N_plasma_poles(iqbz),i_q_mem,:)=Plasma_grid(1:N_plasma_poles(iqbz),i_q_mem,:)
 !enddo
 !deallocate(Plasma_grid)
 !!
 !allocate(Plasma_grid(max_N_plasma_poles,PAR_nQ,2))
 !Plasma_grid=-1
 !do iqbz=1,q%nbz
 !  if (.not.PAR_IND_Q%element_1D(iqbz)) cycle
 !  i_q_mem=PAR_Q_index(iqbz)
 !  Plasma_grid(1:N_plasma_poles(iqbz),i_q_mem,:)=local_grid(1:N_plasma_poles(iqbz),i_q_mem,:)
 !enddo
 !deallocate(local_grid)
 !
 write (ch,'(3a)') "[Pl] N(m,m',p,q) --> N(J,q)  :"
 call msg('rs',trim(ch),(/sum(N_plasma_poles),k%nbz*(SC_bands(2)-SC_bands(1)+1)**2*n_sp_pol*q%nbz/))
 !
end subroutine Plasma_driver
