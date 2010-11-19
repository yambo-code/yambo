!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
subroutine WF_states_setup(k,Xw,b_to_load,k_to_load,qp_to_load,cg_npts,&
&                          bg_npts,qpoint,par_loop,report)
 !
 ! Generic subroutine to distribute WF among different processors
 !
 ! it supports parallelilzation on: 
 !
 ! 1) quasiparticles; 2) bands and kpoints; 3) elctrong-hole poles;
 !
 !
 use pars,            ONLY:SP
 use wave_func,       ONLY:states_to_load
 use QP_m,            ONLY:QP_nb,QP_nk,QP_table,QP_n_states
 use electrons,       ONLY:n_bands,n_sp_pol
 use R_lattice,       ONLY:nkibz,qindx_X    
 use par_proc_m,      ONLY:pp_indexes,pp_indexes_reset,myid,pp_redux_wait,ncpu
 use par_indexes_m,   ONLY:par_indexes
 use R_lattice,       ONLY:bz_samp,qindx_S,nqbz
 use X_m,             ONLY:X_poles_tab
 use WF_distribute,   ONLY:Distributed_Memory
 use frequency,       ONLY:w_samp
 use com,             ONLY:msg
 !
 implicit none
 !
 type(bz_samp), intent(in),optional :: k
 type(w_samp),  intent(in),optional :: Xw
 logical, intent(in), optional :: par_loop,report
 integer, intent(in), optional :: k_to_load(2),b_to_load(2),qp_to_load(2)
 integer, intent(in), optional :: qpoint,cg_npts,bg_npts(*)
 !
 ! Work Space
 !
 integer :: i1,i2,iq,i_qp,ib,ik,is,iv,ic,ikp,ikbz,ikpbz,n_poles
 real(SP):: N_loaded(ncpu)
 logical :: par_loops
 type(pp_indexes) ::px
 !                                 
 if(.not.Distributed_Memory) return
 !
 if (present(report)) then
   N_loaded=0.
   N_loaded(myid+1)=real(count(states_to_load.eqv..TRUE.))/real(n_sp_pol*nkibz* (b_to_load(2)-b_to_load(1)+1))*100.
   call pp_redux_wait(N_loaded) 
   call msg('rs','[Distribute] Average allocated memory is [o/o]:',sum(N_loaded)/real(ncpu))
   return
 endif
 !
 par_loops=.TRUE.
 if(present(par_loop)) par_loops=par_loop
 !
 call pp_indexes_reset(px)
 !
 if(.not.allocated(states_to_load)) then
   allocate(states_to_load(n_bands,nkibz,n_sp_pol))
   states_to_load=.FALSE.
 endif
 !
 if(present(qp_to_load).and..not.present(b_to_load)) then
   !
   ! Parallelization only on Quasi-particle
   ! 
   call par_indexes(px,(/qp_to_load(2)/),(/qp_to_load(1)/))
   !
   do i_qp=qp_to_load(1),qp_to_load(2)
     !
     if(.not.px%element_1D(i_qp).and.par_loops) cycle
     !
     states_to_load(QP_table(i_qp,1),QP_table(i_qp,3),:)=.TRUE.
     states_to_load(QP_table(i_qp,2),QP_table(i_qp,3),:)=.TRUE.
     !
   enddo
   !
 elseif(present(qp_to_load).and.present(b_to_load).and.present(k)) then
   ! 
   ! Parallelization on scattering states
   !
   call par_indexes(px,(/nqbz,b_to_load(2)/),(/1,b_to_load(1)/))
   !
   do i_qp=qp_to_load(1),qp_to_load(2)
     !
     do iq=1,nqbz
       do ib=b_to_load(1),b_to_load(2)
         !
         if(.not.px%element_2D(iq,ib).and.par_loops) cycle 
         ! 
         ikp  =k%sstar(qindx_S(QP_table(i_qp,3),iq,1),1)
         states_to_load(ib,ikp,:)=.TRUE.
         !
       enddo
     enddo
     !
   enddo
   !
 elseif(present(b_to_load).and.present(k_to_load)) then
   !
   ! Parallelization on bands and k-points
   !
   call par_indexes(px,(/k_to_load(2),b_to_load(2)/),(/k_to_load(1),b_to_load(1)/))
   !
   do ik=k_to_load(1),k_to_load(2)
     do ib=b_to_load(1),b_to_load(2)
       ! 
       if(.not.px%element_2D(ik,ib).and.par_loops) cycle
       states_to_load(ib,ik,:)=.TRUE.
       !
     enddo
   enddo
   !
 elseif(present(cg_npts).and.present(bg_npts).and.present(k).and.present(qpoint).and.present(Xw)) then
  !
  ! Electron-Hole poles distribution 
  !
  allocate(px%weight_1D(cg_npts))
  px%weight_1D(1:cg_npts)=bg_npts(1:cg_npts)+Xw%n(2)
  call par_indexes(px,(/cg_npts/))
  !
  do i1 = 1,cg_npts
    n_poles = sum(bg_npts(1:i1-1))
    !
    if (.not.px%element_1D(i1).and.par_loops) cycle
    !
    do i2=1,bg_npts(i1)
      !
      n_poles=n_poles+1
      !
      ikbz   = X_poles_tab(n_poles,1)
      iv     = X_poles_tab(n_poles,2)
      ic     = X_poles_tab(n_poles,3)
      !
      ikpbz  = ikbz
      if(qpoint/=1) ikpbz  = qindx_X(qpoint,ikbz,1)
      !
      ik =k%sstar(ikbz,1)
      ikp=k%sstar(ikpbz,1)
      !
      states_to_load(iv,ikp,:)=.TRUE.
      states_to_load(ic,ik,:) =.TRUE.
      !
    enddo
  enddo
  !
 endif 
 !
 call pp_indexes_reset(px)
 !
end subroutine
