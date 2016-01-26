!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
integer function PLASMA_tables_and_dimensions(iqbz,E,k,q,poles,N)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp,qindx_C
 use electrons,     ONLY:levels
 use QP_m,          ONLY:QP_n_states,QP_table
 use frequency,     ONLY:ordered_grid_index,coarse_grid_index,bare_grid_N,Coarse_grid_N,coarse_grid_Pt
 use plasma,        ONLY:EH_gas
 use parallel_m,    ONLY:PAR_nQ,PAR_Q_index,PAR_IND_Q
 use memory_m,      ONLY:mem_est
 !
 implicit none
 !
 integer          :: iqbz,N
 type(levels)     :: E 
 type(bz_samp)    :: k,q
 real(SP)         :: poles(N)
 !
 ! Local variables
 !
 !
 logical       :: l_reallocate=.false.
 integer       :: i_qp,i_q,i_q_s,i_m,i_mp,i_p,i_p_bz,i_p_star,i_p_s,i_q_mem,i_q_mem_local,&
&                 i_pmq,i_pmq_bz,i_pmq_G0,iqbz_local,n_poles,i_pole,i_cg
 real(SP)      :: E_center
 !
 integer, allocatable :: local_grid(:,:,:)
 !
 i_pmq_G0=0
 n_poles=0
 i_q_mem=PAR_Q_index(iabs(iqbz))
 !
 i_q   = q%sstar(iabs(iqbz),1)
 i_q_s = q%sstar(iabs(iqbz),2)
 !
 do i_qp=1,QP_n_states
   !
   i_m  = QP_table(i_qp,1)
   i_mp = QP_table(i_qp,2)
   i_p  = QP_table(i_qp,3)
   !
   do i_p_star=1,k%nstar(i_p)
     !
     i_p_s =k%star(i_p,i_p_star)
     i_p_bz=k%k_table(i_p,i_p_s)
     !
     i_pmq_bz = qindx_C(i_p_bz,iabs(iqbz),1)
     i_pmq   = k%sstar(i_pmq_bz,1)
     !
     E_center=E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
     !
     !IMPLEMENTATION WITH ABS and EMISSION<
     !if(E_center<0._SP) cycle
     !IMPLEMENTATION WITH ABS and EMISSION>
     !
     n_poles=n_poles+1
     i_pole=n_poles
     if (iqbz<0) poles(i_pole)=E_center
     !
     if (iqbz>0) then
       i_pole=ordered_grid_index(n_poles)
       EH_gas%tab(i_pole,i_q_mem,:)=(/i_qp,i_p_s/)
       poles(i_pole)=E_center       
       !
       i_pole=coarse_grid_index(n_poles)
       EH_gas%tab_m1(i_qp,i_p_s,i_q_mem)=i_pole
     endif
     !
   enddo
 enddo
 !
 PLASMA_tables_and_dimensions=n_poles
 !
 if (iqbz>0 ) then
   !
   if (i_q_mem==1) then
     allocate(EH_gas%grid(Coarse_grid_N,PAR_nQ,2))
     EH_gas%grid=-1
     EH_gas%N_poles=0
     l_reallocate=.false.
   endif
   !
   if (i_q_mem>1 ) l_reallocate= ( Coarse_grid_N>EH_gas%max_N_poles )
   !
   EH_gas%N_poles(iqbz)=Coarse_grid_N
   EH_gas%max_N_poles=maxval(EH_gas%N_poles(1:iqbz))
   !
   if (l_reallocate) then
     !
     allocate(local_grid(EH_gas%max_N_poles,iqbz-1,2))
     do iqbz_local=1,iqbz-1
       if (.not.PAR_IND_Q%element_1D(iqbz_local)) cycle
       i_q_mem_local=PAR_Q_index(iqbz_local)
       local_grid(1:EH_gas%N_poles(iqbz_local),i_q_mem_local,:)=EH_gas%grid(1:EH_gas%N_poles(iqbz_local),i_q_mem_local,:)
     enddo
     deallocate(EH_gas%grid)
     !
     allocate(EH_gas%grid(EH_gas%max_N_poles,q%nbz,2))
     EH_gas%grid=-1
     do iqbz_local=1,iqbz-1
       if (.not.PAR_IND_Q%element_1D(iqbz_local)) cycle
       i_q_mem_local=PAR_Q_index(iqbz_local)
       EH_gas%grid(1:EH_gas%N_poles(iqbz_local),i_q_mem_local,:)=local_grid(1:EH_gas%N_poles(iqbz_local),i_q_mem_local,:)
     enddo
     deallocate(local_grid)
     !
   endif
   !
   do i_cg=1,EH_gas%N_poles(iqbz)
     n_poles=sum(bare_grid_N(1:i_cg-1))
     EH_gas%grid(i_cg,i_q_mem,:)=(/n_poles+1,n_poles+bare_grid_N(i_cg)/)
   enddo
   !
   deallocate(ordered_grid_index,coarse_grid_index,bare_grid_N,coarse_grid_Pt)
   call mem_est("RGi CGi BGn CGp")
   !
 endif
 !
end function PLASMA_tables_and_dimensions
