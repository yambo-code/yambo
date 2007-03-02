!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine K_diagonalization(iq,W) 
!==================================
!
! Generic diagonalization method to solve resonant and non resonant
! Hamiltonians.
!
 use pars,           ONLY:SP,DP,pi
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,Kernel_by_V,&
&                         BS_K_coupling,BSS_write_eig_2_db
 use memory_m,       ONLY:mem_est
#if defined PJ_SPIN
 use electrons,      ONLY:spin_occ,n_spin
#endif
 use matrix_operate, ONLY:mat_dia_inv,DIAGO,USE_LK,USE_SLK,INV
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm
 use com,            ONLY:isec
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,ncpu,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,VERIFY,OP_WR_CL
 use parser_m,       ONLY:parser
 implicit none
 type(w_samp)  :: W
 integer       :: iq
!ws
 type(pp_indexes)     ::px
 integer              ::i1,i2,lib_driver,BS_H_dim,i_of
#if defined _DOUBLE
 complex(DP):: zdotu,zdotc
#else
 complex(SP):: cdotu,cdotc
#endif
!
! Residuals & Energies
!
 complex(SP), allocatable ::BS_R(:) 
 complex(SP), allocatable ::BS_E(:)
!
! Resonant K
!
 real(SP),    allocatable ::BS_E_diago(:)
!
! Coupling
!
 complex(SP), allocatable ::BS_V_left(:,:)
 complex(SP), allocatable ::BS_V_right(:,:)
 complex(SP), allocatable ::BS_R_left(:)
 complex(SP), allocatable ::BS_R_right(:)
 complex(SP), allocatable ::BS_overlap(:,:)
!
! I/O
!
 integer           ::io_err,ID
 integer, external ::ioBS_diago
!  
! Sectioning
!============================================
 if (isec(2)/=0) then
   call section('=','Diagonalization solver')
 else if (isec(2)==0) then
   call section('+','Diagonalization solver')
 endif
!
! Eigenstates 2 DB ?
!====================
 call parser('WRbsWF',BSS_write_eig_2_db)
!
! Allocation & Par Procs
!=======================
 BS_H_dim=BS_K_dim
 if (BS_K_coupling) BS_H_dim=2*BS_K_dim
 allocate(BS_R(BS_H_dim),BS_E(BS_H_dim))
 call mem_est("BS_R",(/BS_H_dim,BS_H_dim/))
 if (BS_K_coupling) then
   allocate(BS_V_left(BS_H_dim,BS_H_dim),&
&           BS_V_right(BS_H_dim,BS_H_dim))
   call mem_est("BS_V_left BS_V_right",&
&              (/BS_H_dim,size(BS_V_left),size(BS_V_right)/))
   allocate(BS_R_left(BS_H_dim),BS_R_right(BS_H_dim),&
&           BS_overlap(BS_H_dim,BS_H_dim))
   call mem_est("BS_R_left BS_R_right BS_overlap",&
&              (/BS_H_dim,BS_H_dim,size(BS_overlap)/))
 else
   allocate(BS_E_diago(BS_H_dim))
   call mem_est("BS_E_diago",(/BS_H_dim/),(/SP/))
 endif

 call pp_indexes_reset(px)
 call par_indexes(px,(/BS_H_dim/))
!
! Kernel loading
!================
 call Kernel_by_V(iq)
!
! If Coupling the half lower part of K must be filled
!======================================================
!
 if (BS_K_coupling) then
!
! Anti-resonant
   forall(i1=BS_K_dim+1:2*BS_K_dim,i2=BS_K_dim+1:2*BS_K_dim) &
&        BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2-BS_K_dim))
!
! Anti-coupling
   forall(i1=BS_K_dim+1:2*BS_K_dim,i2=1:BS_K_dim) &
&        BS_mat(i1,i2)=-conjg(BS_mat(i1-BS_K_dim,i2+BS_K_dim))
 endif
!
! Diagonalization DB (IN)
!=========================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 io_err=ioBS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
!
! Initialize the output file 
!===========================
 call K_dump_to_o_file(iq,-2)
!
 if(io_err<0) then
!
!Diagonalization of the excitonic hamiltonian
!============================================
   lib_driver=USE_LK
   if (ncpu>1) lib_driver=USE_SLK
   call live_timing('BSK diagonalize',1)
   if (BS_K_coupling) then
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_cmpl=BS_E,&
&                     V_left=BS_V_left,V_right=BS_V_right)
   else
     call mat_dia_inv(DIAGO,lib_driver,BS_mat,E_real=BS_E_diago)
     BS_E=BS_E_diago
   endif
   call live_timing(steps=1)
   call live_timing
!
   if (BS_K_coupling) then
!=========================================================
!   The right eigenvector v(j) of A satisfies
!                   A * v(j) = lambda(j) * v(j)
!  where lambda(j) is its eigenvalue.
!  The left eigenvector u(j) of A satisfies
!                u(j)**H * A = lambda(j) * u(j)**H
!  where u(j)**H denotes the conjugate transpose of u(j).
!=========================================================
! Remember that
!
! 1/(w-H)= \sum_ij |i right><i left|j right>^{-1) <j left| /(w-E_i)
!
! [1] BS_R_right(i)=<q0|j><j|i_R>
!                  =conjg(BSS_rhoq0(j))*BS_V_right(j,i)
!---------------------------------------------------------
!
     call live_timing('BSK R residuals',px%stps(myid+1))
     do i1=1,BS_H_dim
       BS_R_right(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R_right(i1)=zdotc(BS_H_dim,BSS_rhoq0,1,BS_V_right(:,i1),1)
#else
       BS_R_right(i1)=cdotc(BS_H_dim,BSS_rhoq0,1,BS_V_right(:,i1),1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_right)
     call live_timing
!
! [2] BS_R(i)= <i_K|j><j|q0> 
!            = conjg( BS_V_left(k,i))*BSS_rhoq0(k)*sign(k)
!------------------------------------------------------------
!
     call live_timing('BSK L residuals',px%stps(myid+1))
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)
     do i1=1,BS_H_dim
       BS_R(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R(i1)=zdotc(BS_H_dim,BS_V_left(:,i1),1,BSS_rhoq0,1)
#else
       BS_R(i1)=cdotc(BS_H_dim,BS_V_left(:,i1),1,BSS_rhoq0,1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R)
     BSS_rhoq0(BS_K_dim+1:)=-BSS_rhoq0(BS_K_dim+1:)
     call live_timing
!
! [3] BS_overlap(i,j)=conjg(BS_V_left(k,i))*BS_V_right(k,j)
!----------------------------------------------------------
     call live_timing('BSK overlap mat',1)
#if defined _DOUBLE
     call zgemm('c','n',BS_H_dim,BS_H_dim,BS_H_dim,(1.,0.),&
&               BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,(0.,0.),&
&               BS_overlap,BS_H_dim)
#else
     call cgemm('c','n',BS_H_dim,BS_H_dim,BS_H_dim,(1.,0.),&
&               BS_V_left,BS_H_dim,BS_V_right,BS_H_dim,(0.,0.),&
&               BS_overlap,BS_H_dim)
#endif
     call mat_dia_inv(INV,USE_SLK,BS_overlap)
     call live_timing(steps=1)
     call live_timing()

! [4] BS_R_left(i)=BS_overlap(i,j)BS_R(j)
!---------------------------------------------------
     call live_timing('BSK L x overlap',px%stps(myid+1))
     do i1=1,BS_H_dim
       BS_R_left(i1)=cmplx(0.,0.,SP)
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R_left(i1)=zdotu(BS_H_dim,BS_overlap(i1,:),1,bs_R,1)
#else
       BS_R_left(i1)=cdotu(BS_H_dim,BS_overlap(i1,:),1,bs_R,1)
#endif
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R_left)
     call live_timing

! [5] BS_R(i)=BS_R_left(i)BS_R_right(i)
!---------------------------------------------------
     do i1=1,BS_H_dim
       BS_R(i1)=BS_R_left(i1)*BS_R_right(i1)
     enddo
   else
!-----------------------------------------
!Resonant Residuals:
!
! BS_R(i)=\sum_k <q0|k><k|i> =
!         \sum_k BS_mat(k,i) x conjg( BSS_rhoq0(k) )
!-------------------------------------------------------
     call live_timing('BSK   residuals',px%stps(myid+1))
     BS_R=(0.,0.)
     do i1=1,BS_K_dim
       if (.not.px%i1p(i1)) cycle
#if defined _DOUBLE
       BS_R(i1)=zdotc(BS_K_dim,BSS_rhoq0,1,BS_mat(:,i1),1)
#else
       BS_R(i1)=cdotc(BS_K_dim,BSS_rhoq0,1,BS_mat(:,i1),1)
#endif
       BS_R(i1)=BS_R(i1)*conjg( BS_R(i1) )
       call live_timing(steps=1)
     enddo
     call pp_redux_wait(BS_R)
     call live_timing
   endif
 endif
!
!Now I calculate epsilon
!========================
!
! eps2(iw)= 1 - Sum BS_R(K)/(w +I eta - E)
!
 X_epsilon(1,:)=(0.,0.)
 X_epsilon(2,:)=(0.,0.)
 X_epsilon(3,:)=X_epsilon(3,:)/real(ncpu,SP)
 call live_timing('BSK res epsilon',px%stps(myid+1))
 do i1=1,BS_H_dim
   if (.not.px%i1p(i1)) cycle
   if (BS_K_coupling) then
     if (real(BS_E(i1))>0.) then
       X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(W%p(:)-BS_E(i1))
     else
       X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(conjg(W%p(:))-BS_E(i1))
     endif
   else
     X_epsilon(2,:)=X_epsilon(2,:)-BS_R(i1)/(W%p(:)-BS_E(i1))
   endif
   call live_timing(steps=1)
 enddo
 call live_timing
 call pp_redux_wait(X_epsilon)
!
#if defined PJ_SPIN
 X_epsilon(2,:)=1.+X_epsilon(2,:)*(spin_occ/(2.*pi)**3.)*d3k_factor*4.*pi/(q_norm(1))**2
#else
 X_epsilon(2,:)=1.+X_epsilon(2,:)*d3k_factor/pi**2./q_norm(1)**2
#endif

 X_epsilon(1,:)=W%p(:)
 call pp_indexes_reset(px)
!
!I write the output file 
!=======================
 call K_dump_to_o_file(iq,2)
!
! Diagonalization DB (OUT)
!=========================
 if (io_err/=0) then
   call io_control(ACTION=OP_WR_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
   io_err=ioBS_diago(iq,BS_H_dim,BS_E,BS_R,ID)
 endif
!
!CLEAN
!=====
!
 if (allocated(BS_mat)) deallocate(BS_mat)
 deallocate(BS_R,BS_E)
 call mem_est("BS_R BS_E")
 if (BS_K_coupling) then
   deallocate(BS_V_left,BS_V_right,BS_R_left,BS_R_right)
   call mem_est("BS_V_left BS_V_right BS_R_left BS_R_right")
 else
   deallocate(BS_E_diago)
   call mem_est("BS_E_diago")
 endif
!
 end subroutine
