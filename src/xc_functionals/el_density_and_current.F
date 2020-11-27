!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine el_density_and_current(E,k,rho,J,bands,force_spatial_inversion)
 !
 ! Electronic density (and current).
 !
 ! Note that here the identity
 !
 ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
 ! 
 ! where S_k is a symm. op. in the star of k, S is a generic symm. op.
 ! and R_k is the rank of the small group at k.
 !
 ! Using (1) the density is calculated in two steps
 !
 !  rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
 !         = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
 !         = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
 !
 ! where we have used the relation
 !
 ! w_k = nsym/(nkibz * R_k)
 !
 ! Same procedure for the current
 !
 use pars,          ONLY:SP,DP
 use com,           ONLY:msg
 use stderr,        ONLY:real2ch
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use R_lattice,     ONLY:bz_samp,b
 use D_lattice,     ONLY:nsym,i_time_rev,mag_syms
 use FFT_m,         ONLY:fft_size,fft_dim,fft_rot_r,fft_rot_r_inv,fft_g_table
#if defined _FFTW
 use FFT_m,         ONLY:fftw_plan
#endif
 use wave_func,     ONLY:WF
 use parallel_m,    ONLY:PAR_IND_WF_linear,PAR_COM_density
 use parallel_int,  ONLY:PP_redux_wait
 use pseudo,        ONLY:pp_is_uspp,qe_pseudo_alloc,becp
 use qe_pseudo_m,   ONLY:qe_omega=>omega,qe_ngm=>ngm, becsum
#if defined _RT
 use drivers,       ONLY:l_real_time
 use timing_m,      ONLY:timing
#endif
#if defined _YPP_RT
 use RT_occupations,ONLY:RT_el_occ
 use real_time,     ONLY:RTibz
#endif
 !
#include<memory.h>
 !
 type(bz_samp)::k
 type(levels) ::E       
 real(SP), optional             ::J(fft_size,3)
 real(SP), optional             ::rho(fft_size)
 integer,  optional, intent(in) ::bands(2)
 logical,  optional, intent(in) ::force_spatial_inversion
 !
 ! Work Space 
 !
#if defined _YPP_RT
 integer    :: ik_RT
#endif
 integer    :: ib,ik,i_sp_pol,i_spinor,rho_syms,i_symm,i_wf,bands_range(2) !,i_r,i_dir
 integer    :: ig
 real(SP)   :: f_occ,rho_no_sym(fft_size) !,J_no_sym(fft_size,3)
 real(SP)   :: raux
 complex(DP), allocatable :: rhog_aug_DP(:,:), wf_DP(:), aux_DP(:)
 logical    :: warn_done,l_si,l_rho !,l_J
 !
#if defined _RT
 call timing('el_density',OPR='start')
#endif
 !
 ! Basic Setups
 !
 l_rho     =present(rho)
 !l_J       =present(J)
 l_si      =.false.
 if (present(force_spatial_inversion)) l_si=force_spatial_inversion
 !
 if (l_rho) then
   rho       =0._SP
   rho_no_sym=0._SP
 endif
 !if (l_J) then
 !  J         =0._SP
 !  J_no_sym  =0._SP
 !endif
 
 !
 ! USPP contribution
 ! 
 if (l_rho.and.pp_is_uspp) then
   !
   if (.not. qe_pseudo_alloc) call error(' [PP] qe_pseudo not alloc in XC driver')
   !
   becsum=0.0
   !
 endif
 !
 warn_done=.false.
 !
 bands_range=(/1,E%nbm/)
 if(present(bands)) bands_range=bands
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,k%nibz
     !
     do ib=bands_range(1),bands_range(2)
       !
       if (.not.PAR_IND_WF_linear%element_2D(ib,ik)) cycle
       !
       if (size(WF%index,1)<ib)        cycle
       if (size(WF%index,2)<ik)        cycle
       if (size(WF%index,3)<i_sp_pol)  cycle
       !
       f_occ=E%f(ib,ik,i_sp_pol)
#if defined _RT
       if(l_real_time.and.allocated(E%fo)) f_occ=E%fo(ib,ik,i_sp_pol) 
#endif
!GPL_EXCLUDE_START
#if defined _YPP_RT
       if(present(bands)) then
         f_occ=0._SP
         do ik_RT=RTibz%k_range(ik,1),RTibz%k_range(ik,2)
           f_occ=f_occ+RT_el_occ%dN(ib,ik_RT)*RTibz%weights(ik_RT)
         enddo
       endif
#endif
!GPL_EXCLUDE_END
       !
       i_wf=WF%index(ib,ik,i_sp_pol)
       !
       if (i_wf==0) then
         if (.not.warn_done) call warning('Not enough states to calculate rho')
         warn_done=.true.
         cycle
       endif
       !
       !
       if (l_rho) then
         do i_spinor=1,n_spinor
           rho_no_sym(:)=rho_no_sym(:)+f_occ*k%weights(ik)*abs(WF%c(:,i_spinor,i_wf))**2
         enddo
         !
         ! uspp becsum
         !
         if (pp_is_uspp) then
           !
           call sum_bec(ik,i_sp_pol, ib,ib,1, (/f_occ*k%weights(ik)/), becp(ik,i_sp_pol) )
           !
         endif
         !
       endif
       !DEBUG>
       !if (l_J) then
       !  !
       !  call eval_Gradient(WF%c(:,:,i_wf),WF_der,n_spinor,"wave")
       !  !
       !  do i_dir=1,3
       !    do i_spinor=1,n_spinor
       !      do i_r=1,fft_size
       !        J_no_sym(i_r,i_dir)=J_no_sym(i_r,i_dir)+E%f(ib,ik,i_sp_pol)*k%weights(ik)*aimag(WF%c(i_r,i_spinor,i_wf)*WF_der(i_r,i_spinor,i_dir))
       !      enddo
       !    enddo
       !  enddo
       !  !
       !endif
       !DEBUG<
     enddo
     !
   enddo
 enddo
 !
 ! finalize USPP contribution
 !
 if (l_rho.and.pp_is_uspp) then
   !
   YAMBO_ALLOC(aux_DP,(fft_size))
   YAMBO_ALLOC(rhog_aug_DP,(qe_ngm,n_sp_pol))
   rhog_aug_DP=0.0
   !
   call addusdens(qe_ngm,n_sp_pol,qe_ngm,rhog_aug_DP)
   !
   do i_sp_pol=1,n_sp_pol
      !
      aux_DP=0.0
      do ig = 1, qe_ngm
         aux_DP(fft_g_table(ig,1)) = rhog_aug_DP(ig,i_sp_pol)
      enddo
      !
#if defined _FFTW
      call dfftw_destroy_plan(fftw_plan)
      fftw_plan = 0
      call fft_3d(aux_DP,fft_dim,+1,fftw_plan)
#else
      call fft_3d(aux_DP,fft_dim,+1)
#endif
      !
      aux_DP=aux_DP*qe_omega/real(fft_size,DP)
      rho_no_sym(:)=rho_no_sym(:)+real(aux_DP(:),SP)
      !
   enddo
   !
   YAMBO_FREE(rhog_aug_DP)
   YAMBO_FREE(aux_DP)
   !
   raux = sum(rho_no_sym)
   call msg('nr','[PP] USPP charge normalization :  '//trim(real2ch(raux))//' [el/cell]')
   !
 endif
 !
 if (l_rho) call PP_redux_wait(rho_no_sym,COMM=PAR_COM_density%COMM)
 !if ( l_J ) call PP_redux_wait(J_no_sym  ,COMM=PAR_COM_density%COMM)
 !
 ! Symmetrization
 !
 rho_syms=nsym/(i_time_rev+1)
 !
 if (mag_syms) rho_syms=nsym 
 !
 if (l_rho) then
   do i_symm=1,rho_syms
     rho(:)=rho(:)+rho_no_sym(fft_rot_r(:,i_symm))/real(nsym,SP)
   enddo
   if (mag_syms)      return
   if (.not.l_si) rho(:)=real(1+i_time_rev,SP)*rho(:)
   if (     l_si) rho(:)=real(  i_time_rev,SP)*rho(fft_rot_r_inv(:))+rho(:)
 endif
 !
 !if (l_J) then
 !  do i_symm=1,rho_syms
 !    J(:,:)=J(:,:)+real(J_no_sym(fft_rot_r(:,i_symm),:),SP)/real(nsym,SP)
 !  enddo
 !endif
 !
#if defined _RT
 call timing('el_density',OPR='stop')
#endif
 ! 
end subroutine
