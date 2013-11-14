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
integer function Plasma_setup(iqbz,E,k,q,poles)
 !
 use pars,          ONLY:SP
 use R_lattice,     ONLY:bz_samp
 use SC,            ONLY:SC_bands
 use electrons,     ONLY:levels,n_sp_pol
 use QP_m,          ONLY:QP_n_states,QP_table
 use frequency,     ONLY:ordered_grid_index,coarse_grid_index,bare_grid_N,Coarse_grid_N,coarse_grid_Pt
 use plasma,        ONLY:Plasma_tab,Plasma_tab_m1,Plasma_grid,N_plasma_poles,max_N_plasma_poles
 use parallel_m,    ONLY:PAR_nQ,PAR_Q_index,PAR_IND_Q
 !
 implicit none
 !
 integer       :: iqbz
 type(levels)  :: E 
 type(bz_samp) :: k,q
 real(SP)      :: poles(k%nbz*(SC_bands(2)-SC_bands(1)+1)**2*n_sp_pol)
 !
 ! Local variables
 !
 logical       :: l_reallocate=.false.
 integer       :: i_qp,i_q,i_q_s,i_m,i_mp,i_p,i_p_star,i_p_s,i_q_mem,i_q_mem_local,&
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
     !
     call RT_EE_scattering_kinematics(k,q,i_p,i_p_s,i_q,i_q_s,i_pmq_bz,i_pmq_G0)
     !
     i_pmq =k%sstar(i_pmq_bz,1)
     !
     E_center=E%E(i_m,i_p,1)-E%E(i_mp,i_pmq,1)
     !
     ! With the following two lines I would move from N**2 to N*(N-1)/2 transitions
     ! I.e. in silicon with 6 bands from 36 to 15 ...
     !if(i_m==i_mp .and. iqbz==1) cycle
     !if(E_center<0.) cycle
     !
     n_poles=n_poles+1
     i_pole=n_poles
     if (iqbz<0) poles(i_pole)=E_center
     !
     if (iqbz>0) then
       i_pole=ordered_grid_index(n_poles)
       Plasma_tab(i_pole,i_q_mem,:)=(/i_qp,i_p_s/)
       poles(i_pole)=E_center       
       !
       i_pole=coarse_grid_index(n_poles)
       Plasma_tab_m1(i_qp,i_p_s,i_q_mem)=i_pole
     endif
     !
   enddo
 enddo
 !
 Plasma_setup=n_poles
 !
 if (iqbz>0 ) then
   !
   if (i_q_mem==1) then
     allocate(Plasma_grid(Coarse_grid_N,PAR_nQ,2))
     Plasma_grid=-1
     N_plasma_poles=0
     l_reallocate=.false.
   endif
   !
   if (i_q_mem>1 ) l_reallocate= ( Coarse_grid_N>max_N_plasma_poles )
   !
   N_plasma_poles(iqbz)=Coarse_grid_N
   max_N_plasma_poles=maxval(N_plasma_poles)
   !
   if (l_reallocate) then
     !
     allocate(local_grid(max_N_plasma_poles,iqbz-1,2))
     do iqbz_local=1,iqbz-1
       if (.not.PAR_IND_Q%element_1D(iqbz_local)) cycle
       i_q_mem_local=PAR_Q_index(iqbz_local)
       local_grid(1:N_plasma_poles(iqbz_local),i_q_mem_local,:)=Plasma_grid(1:N_plasma_poles(iqbz_local),i_q_mem_local,:)
     enddo
     deallocate(Plasma_grid)
     !
     allocate(Plasma_grid(max_N_plasma_poles,q%nbz,2))
     Plasma_grid=-1
     do iqbz_local=1,iqbz-1
       if (.not.PAR_IND_Q%element_1D(iqbz_local)) cycle
       i_q_mem_local=PAR_Q_index(iqbz_local)
       Plasma_grid(1:N_plasma_poles(iqbz_local),i_q_mem_local,:)=local_grid(1:N_plasma_poles(iqbz_local),i_q_mem_local,:)
     enddo
     deallocate(local_grid)
     !
   endif
   !
   ! DEBUG <
   !write(*,*) 'iq= ',iqbz,Coarse_grid_N,sum(bare_grid_N)
   ! DEBUG >
   do i_cg=1,N_plasma_poles(iqbz)
     n_poles=sum(bare_grid_N(1:i_cg-1))
     Plasma_grid(i_cg,i_q_mem,:)=(/n_poles+1,n_poles+bare_grid_N(i_cg)/)
     ! DEBUG <
     !write(*,*) 'Pole corsed grid= ',i_cg,poles(n_poles+1)
     !write(*,*) 'Associated poles= ',bare_grid_N(i_cg),poles(n_poles+2:n_poles+bare_grid_N(i_cg))
     ! DEBUG >
   enddo
   !
   deallocate(ordered_grid_index,coarse_grid_index,bare_grid_N,coarse_grid_Pt)
   !
 endif
 !
end function Plasma_setup
