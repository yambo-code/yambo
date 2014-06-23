!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine K_exchange(divide_by_qpg,iq,Xk,i_T)
 !
 use BS,             ONLY:BS_n_g_exch
 use D_lattice,      ONLY:nsym,i_time_rev,sop_inv
 use BS,             ONLY:BS_T_grp,BS_collision,BS_oscillators_alloc
 use R_lattice,      ONLY:g_rot,qindx_X,bz_samp,bare_qpg
 use parallel_m,     ONLY:PAR_COM_eh_A2A,PAR_IND_eh,PP_redux_wait
 !
 implicit none
 type(bz_samp) :: Xk
 integer       :: iq,i_T
 logical       :: divide_by_qpg
 !
 ! Work Space
 !
 integer :: i_T_el,i_c,i_v,i_spin,i_k_bz,i_k,i_s,i_g1,i_g2,i_p_bz,i_p,i_sp
 !
 ! Oscillators already done ?
 !
 if (allocated( BS_T_grp(i_T)%O_x )) return
 !
 ! Allocate
 !
 call BS_oscillators_alloc(.TRUE.,i_T,.FALSE.,0)
 !
 ! Now the evaluation of the oscillators.
 !
 ! Note that, in contrast to the previous implementation, each set of CPU's
 ! has one specific K assigned. Thus no rotation can be performed in the internal
 ! group of transitions. 
 !
 i_k_bz=BS_T_grp(i_T)%ik
 i_k   =Xk%sstar(i_k_bz,1)
 i_s   =Xk%sstar(i_k_bz,2)
 !
 i_p_bz=qindx_X(iq,i_k_bz,1)
 i_p   =Xk%sstar(i_p_bz,1)
 i_sp  =Xk%sstar(i_p_bz,2)
 !
 BS_collision%ngrho=BS_n_g_exch
 !
 allocate(BS_collision%rhotw(BS_n_g_exch))
 !
 T_loop: do i_T_el=1,BS_T_grp(i_T)%size
   !
   if (.not.PAR_IND_eh(i_k_bz)%element_1D(i_T_el)) cycle
   !
   i_v   =BS_T_grp(i_T)%table(i_T_el,1)
   i_c   =BS_T_grp(i_T)%table(i_T_el,2)
   i_spin=BS_T_grp(i_T)%table(i_T_el,3)
   !
   BS_collision%is=(/i_c,i_k,i_s,i_spin/)
   BS_collision%os=(/i_v,i_p,i_sp,i_spin/)
   BS_collision%qs=(/qindx_X(iq,i_k_bz,2),iq,1/)
   !
   if (iq==1) then
     !
     BS_collision%is(3)=1
     BS_collision%os(3)=1
     !
     call scatterBamp(BS_collision)
     !
!$omp parallel do default(shared), private(i_g1,i_g2)
     do i_g1=1,BS_n_g_exch
       !
       i_g2=g_rot(sop_inv(i_s),i_g1)
       !
       if (divide_by_qpg)      BS_T_grp(i_T)%O_x(i_g1,i_T_el)=BS_collision%rhotw(i_g2)/bare_qpg(iq,i_g2)
       if (.not.divide_by_qpg) BS_T_grp(i_T)%O_x(i_g1,i_T_el)=BS_collision%rhotw(i_g2)
       !
       if (i_s>nsym/(i_time_rev+1))  BS_T_grp(i_T)%O_x(i_g1,i_T_el)=conjg(BS_T_grp(i_T)%O_x(i_g1,i_T_el))
       !
     enddo
!$omp end parallel do
     !
   else
     !
     call scatterBamp(BS_collision)
     !
!$omp parallel do default(shared), private(i_g1)
     do i_g1=1,BS_n_g_exch
       !
       if (divide_by_qpg)      BS_T_grp(i_T)%O_x(i_g1,i_T_el)=BS_collision%rhotw(i_g1)/bare_qpg(iq,i_g1)
       if (.not.divide_by_qpg) BS_T_grp(i_T)%O_x(i_g1,i_T_el)=BS_collision%rhotw(i_g1)
       !
     enddo
!$omp end parallel do
     !
   endif
   !
 enddo T_loop
 !
 deallocate(BS_collision%rhotw)
 !
 ! A2A
 !
 call PP_redux_wait( BS_T_grp(i_T)%O_x , COMM=PAR_COM_eh_A2A%COMM )
 !
end subroutine
