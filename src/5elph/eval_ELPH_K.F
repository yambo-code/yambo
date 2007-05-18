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
integer function eval_ELPH_K(E,k,q,residuals_only,frequency)
 !
 ! This routine calculates (eventually the residuals only) elph 
 ! contribution to the DBSE kernel.
 !
 use pars,          ONLY:SP
 use memory_m,      ONLY:mem_est
 use D_lattice,     ONLY:sop_inv,sop_tab
 use R_lattice,     ONLY:bz_samp,qindx_B,ik_is_table
 use com,           ONLY:warning,error,isec
 use IO_m,          ONLY:io_control,OP_RD,REP,RD_CL_IF_END
 use electrons,     ONLY:levels
 use BS,            ONLY:BS_K_dim,BS_eh_table
 use ELPH,          ONLY:ph_modes,elph_nb,elph_nq,elph_use_q_grid,&
&                        setup_k_plus_q_levels,ph_qpt,elph_gkkp_free,&
&                        ph_freqs_sq,elph_gkkp,E_k_plus_q,elph_nq_used,&
&                        Rnd_Qpts_at,Rnd_Qpts_corresp,EXCPH_mat
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,myid,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use timing,        ONLY:live_timing
 use functions,     ONLY:bose_f
 implicit none
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 logical       ::residuals_only
 complex(SP)   ::frequency
 !
 ! Work Space ...
 !
 ! ... k/q-points ...
 !
 integer :: iq_db,iq_ibz,iq_bz,iq_s,iq_ref,ik1_bz,ik2_bz,&
&           ik1_ibz,ik2_ibz,ik1_bz_gkkp,ik1_s,ik2_s
 integer, external    ::k_the_nearest
 complex(SP)          ::E_diff(2)
 ! 
 ! ... I/O
 !
 integer          ::io_err,ID
 integer, external::ioELPH
 !
 ! ... dummies
 !
 integer     :: i1,i2,ii1,ii2,ic1,ic2,iv1,iv2,il_ph
 complex(SP) :: elph_gkkp_sq
 real(SP)    :: ph_E
 type(pp_indexes) ::px
 !
 eval_ELPH_K=1
 if (residuals_only.and.allocated(EXCPH_mat)) return
 !
 if (isec(2)/=0) then
   call section('=','Electron-Phonon contribution to the DBSE Kernel')
 else if (isec(2)==0) then
   call section('+','Electron-Phonon contribution to the DBSE Kernel')
 endif
 !
 call k_sym2sym(k,'k')
 call k_ibz2bz(q,'iku',.TRUE.)
 !
 ! ELPH DATABASE
 !
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),ID=ID)
 eval_ELPH_K=ioELPH(ID)
 if (eval_ELPH_K/=0) then
   call warning('El-Ph database not found')
   return
 endif
 !
 if (elph_use_q_grid) elph_nq_used=q%nbz
 !
 ! Random <-> Uniform grids correspondance
 !
 if (.not.allocated(EXCPH_mat)) then
   allocate(Rnd_Qpts_corresp(elph_nq_used),Rnd_Qpts_at(q%nbz))
   forall(i1=1:elph_nq_used) Rnd_Qpts_corresp(i1)=i1
   Rnd_Qpts_at=0
   if (.not.elph_use_q_grid) then
     do i1=1,elph_nq
       !
       ! The index iq_bz of the uniform grid nearest to the ph q-pt
       !
       iq_bz =k_the_nearest(ph_qpt(i1,:),q%ptbz(:,:),q%nbz)
       Rnd_Qpts_at(iq_bz)=Rnd_Qpts_at(iq_bz)+1
       Rnd_Qpts_corresp(i1)=iq_bz
     enddo
     if (any(Rnd_Qpts_at==0)) call error('Some Q-pts have empty random region.') 
   else
     Rnd_Qpts_at=1
   endif
   !
   ! El-Ph Matrix
   !
   allocate (EXCPH_mat(BS_K_dim,BS_K_dim))
   call mem_est('EXCPH_mat',(/BS_K_dim**2/))
 endif
 !
 ! El-Ph q-loop
 !
 iq_ref=0
 EXCPH_mat=(0.,0.)
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/elph_nq_used/))
 call pp_redux_wait
 !
 do i1=1,elph_nq_used
   !
   iq_db=i1
   iq_bz=i1
   !
   if (elph_use_q_grid)      iq_db=q%sstar(i1,1)
   if (.not.elph_use_q_grid) iq_bz=Rnd_Qpts_corresp(i1)
   !
   iq_ibz=q%sstar(iq_bz,1)
   iq_s  =q%sstar(iq_bz,2)
   !
   if (iq_db/=iabs(iq_ref)) then
     !
     call io_control(ACTION=RD_CL_IF_END,SEC=(/iq_db+1/),ID=ID)
     io_err=ioELPH(ID)
     iq_ref=iq_db
     if (io_err<0) iq_ref=-iq_db
     !
     if (.not.elph_use_q_grid) call setup_k_plus_q_levels(E%Efermi(1))
     !
     ! I call live_timing here as in ioELPH the elph_alloc() can 
     ! send a screen message about the allocated memory that can interferee
     ! with the live_timing hashes
     !
     if (iq_db==1) call live_timing('El-PH loop',px%stps(myid+1))
     !
   endif
   !
   if (.not.px%i1p(i1)) cycle
   !
   if (iq_ref<0) then
     call live_timing(steps=1)
     cycle
   endif
   !
   ! Now I loop the BSE indexes matching the  elements corresponding to
   ! the iq_bz transition
   !
   do ii1=1,BS_K_dim
     ik1_bz =BS_eh_table(ii1,1)
     ik1_ibz=k%sstar(ik1_bz,1)
     ik1_s  =k%sstar(ik1_bz,2)
     iv1    =BS_eh_table(ii1,2)
     ic1    =BS_eh_table(ii1,3)
     !
     ! When using a uniform Q grid I cycle on the q symmetries
     ! as well. To rotate the gkkp m.e. I use ik1/2_bz_gkkp
     !
     ik1_bz_gkkp=ik1_bz
     if (elph_use_q_grid) then
       !
       ! WARNING ! When iqs includes TR the rotation is wrong.
       !
       ik1_bz_gkkp=ik_is_table(ik1_ibz,sop_tab(sop_inv(iq_s),ik1_s))
     endif
     !
     do ii2=1,ii1-1
       ik2_bz=BS_eh_table(ii2,1)
       ik2_ibz=k%sstar(ik2_bz,1)
       ik2_s  =k%sstar(ik2_bz,2)
       !
       if (qindx_B(ik1_bz,ik2_bz,1)/=iq_bz) cycle
       !
       iv2   =BS_eh_table(ii2,2)
       ic2   =BS_eh_table(ii2,3)
       !
       E_diff(1) = frequency-(E%E(ic1,ik1_ibz,1)-E%E(iv2,ik2_ibz,1))
       E_diff(2) = frequency-(E%E(ic2,ik2_ibz,1)-E%E(iv1,ik1_ibz,1))
       if (elph_use_q_grid) E_diff=(/&
&          frequency-(E%E(ic1,ik1_ibz,1)-E_k_plus_q(iv2,ik2_bz,1)),&
&          frequency-(E_k_plus_q(ic2,ik2_bz,1)-E%E(iv1,ik1_ibz,1))/)
       !
       elph_gkkp_sq=(0.,0.)
       !
       do il_ph=1,ph_modes
         !
         ! Skip Acoustic modes @ Gamma
         !
         if (iq_ibz==1.and.il_ph<=3) cycle
         !
         ph_E=sqrt(abs(ph_freqs_sq(iq_db,il_ph)))
         !
         elph_gkkp_sq=elph_gkkp_sq&
&                     -conjg(elph_gkkp(ik1_bz_gkkp,il_ph,ic2,ic1))*&
&                            elph_gkkp(ik1_bz_gkkp,il_ph,iv2,iv1)/2./ph_E*&
&                      (bose_f(ph_E)+1.)/real(q%nbz)/real(Rnd_Qpts_at(iq_bz))
       enddo
       !
       if (residuals_only) then
         EXCPH_mat(ii1,ii2)=EXCPH_mat(ii1,ii2)+elph_gkkp_sq
       else
         EXCPH_mat(ii1,ii2)=EXCPH_mat(ii1,ii2)+elph_gkkp_sq*(1./E_diff(1)+1./E_diff(2))
       endif
       !
     enddo 
   enddo 
   !
   call live_timing(steps=1)
   !
 enddo
 call live_timing()
 !
 ! MPI sum
 !
 do ii1=1,BS_K_dim
   call pp_redux_wait(EXCPH_mat(:,ii1))
 enddo
 !
 ! Impose EXCPH_mat to be hermitian
 !
 do ii1=1,BS_K_dim
   do ii2=ii1+1,BS_K_dim
     EXCPH_mat(ii1,ii2)=conjg(EXCPH_mat(ii2,ii1))
   enddo
 enddo
 !
 ! CLEAN
 !
 call elph_gkkp_free()
 call pp_indexes_reset(px)
 !
end function
