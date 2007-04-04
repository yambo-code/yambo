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
 subroutine K_inversion(iq,W,Ken,Xk) 
 !
 use pars,           ONLY:SP,pi
 use X_m,            ONLY:X_epsilon
 use BS,             ONLY:BS_mat,BS_K_dim,BSS_rhoq0,&
&                         BS_K_coupling,BS_eh_table,BS_eh_f,BS_eh_E,BS_bands
 use memory_m,       ONLY:mem_est
 use electrons,      ONLY:spin_occ,levels,BZ_RIM_nbands
 use matrix_operate, ONLY:mat_dia_inv,USE_LK,USE_SLK,INV
 use frequency,      ONLY:w_samp
 use R_lattice,      ONLY:d3k_factor,q_norm,bz_samp
 use com,            ONLY:isec,warning
 use timing,         ONLY:live_timing
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,ncpu,myid,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 use IO_m,           ONLY:io_control,OP_RD_CL,REP,VERIFY
 implicit none
 type(w_samp) :: W
 integer      :: iq
 type(levels) :: Ken
 type(bz_samp):: Xk
 !
 ! Work Space
 !
 type(pp_indexes)     ::px
 integer              ::i1,i2,ik_bz,iv,ic,iw,icv
 complex(SP)          ::Lo(BS_K_dim,W%n(1)),Lo_lambda_basis(BS_K_dim,BS_K_dim),&
&                       lambda_by_Lo(BS_K_dim)
#if defined _DOUBLE
 complex(DP):: zdotu,zdotc
#else
 complex(SP):: cdotu,cdotc
#endif
 !
 ! Residuals & Energies
 ! 
 complex(SP), allocatable ::BS_R(:)
 complex(SP), allocatable ::BS_R_iter(:)
 complex(SP), allocatable ::BS_E(:)
 !
 ! I/O
 !
 integer           ::io_err,ID
 integer, external ::ioBS_diago
 !
 ! Sectioning
 !
 if (isec(2)/=0) then
   call section('=','Inversion solver')
 else if (isec(2)==0) then
   call section('+','Inversion solver')
 endif
 !
 ! Inversion Method still unsupported with coupling
 !
 call warning('Inversion solver still unsupported')
 return
 !
 if (BS_K_coupling) then
   call warning('Inversion solver still unsupported for non-resonant K')
   return
 endif
 !
 ! Allocation & Par Procs
 !
 call mem_est("Lo Lo_lambda",(/size(Lo),size(Lo_lambda_basis)/))
 call pp_indexes_reset(px)
 call par_indexes(px,(/BS_K_dim/))
 !
 allocate(BS_R(BS_K_dim), BS_R_iter(BS_K_dim),BS_E(BS_K_dim),BS_mat(BS_K_dim,BS_K_dim))
 call mem_est("BS_R BS_R_iter BS_E BS_mat",(/BS_K_dim,BS_K_dim,BS_K_dim,size(BS_mat)/))
 !
 ! Diagonalization DB (IN)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2,3/),ID=ID)
 io_err=ioBS_diago(iq,BS_K_dim,BS_E,BS_R,ID)
 !
 if(io_err<0) goto 1
 !
 ! Initialize the output file 
 !
 call K_dump_to_o_file(iq,-3)
 !
 ! Green`s function and RIM non-interacting eps2
 !
 call live_timing('Green`s function',px%stps(myid+1))
 !
 X_epsilon(2,:)=(0.,0.)
 X_epsilon(3,:)=(0.,0.)
 !
 do i1=1,BS_K_dim
   !
   if (.not.px%i1p(i1)) cycle
   !
   ik_bz=BS_eh_table(i1,1)
   iv   =BS_eh_table(i1,2)
   ic   =BS_eh_table(i1,3)
   !
   call X_RIM_Gf(1,(/ik_bz,iv,ic,1/),(/1,W%n(1)/),W,Ken,Xk,Lo(i1,:),"r")
   !
   X_epsilon(3,:)=X_epsilon(3,:)-BSS_rhoq0(i1)*conjg(BSS_rhoq0(i1))*Lo(i1,:)
   !
   Lo(i1,:)=-(0.,1.)*BS_eh_f(i1)/Lo(i1,:)+(W%p(:)-BS_eh_E(i1))
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 call pp_redux_wait(Lo)
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/W%n(2)/))
 !
 call live_timing('Frequency loop',px%stps(myid+1))
 !
 do iw=1,W%n(2)
   !
   if (.not.px%i1p(iw)) cycle
   !
   do i2=1,BS_K_dim
     forall(icv=1:BS_K_dim) lambda_by_Lo(icv)=Lo(icv,iw)*BS_mat(icv,i2)
     do i1=1,BS_K_dim
#if defined _DOUBLE
       Lo_lambda_basis(i1,i2)=zdotc(BS_K_dim,BS_mat(:,i1),1,lambda_by_Lo,1)
#else
       Lo_lambda_basis(i1,i2)=cdotc(BS_K_dim,BS_mat(:,i1),1,lambda_by_Lo,1)
#endif
     enddo
   enddo
   !
   do i1=1,BS_K_dim
     X_epsilon(2,iw)=X_epsilon(2,iw)-BS_R(i1)*conjg(BS_R(i1))/(W%p(iw)-BS_E(i1))
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 X_epsilon(2:3,:)=1.+X_epsilon(2:3,:)*real(spin_occ)/(2.*pi)**3.*d3k_factor*4.*pi/(q_norm(1))**2
 !
 call K_dump_to_o_file(iq,3)
 !
1 continue
 !
 ! CLEAN
 !
 call pp_indexes_reset(px)
 call mem_est("Lo Lo_lambda")
 !
 deallocate(BS_E,BS_R,BS_R_iter,BS_mat)
 call mem_est("BS_E BS_R BS_R_iter BS_mat")
 !
 end subroutine
