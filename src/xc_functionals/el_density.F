!
! Copyright (C) 2000-2013 F. De Fausti and the YAMBO team
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
#if defined _RT
subroutine el_density(G_lesser,en,Xk,rho,lowest_band,force_si)
#else
subroutine el_density(en,Xk,rho,force_si)
#endif
 !
 !Electronic density.
 !
 !Note that here the identity
 !
 ! \sum_{S_k} F(S_k r) = 1/R_k \sum_{S} F(S r) (1)
 ! 
 !where S_k is a symm. op. in the star of k, S is a generic symm. op.
 !and R_k is the rank of the small group at k.
 !
 !Using (1) the density is calculated in two steps
 !
 ! rho(r) = 1/nkibz \sum_{ n k S_k } f_{n k} |wf_{n k}(S_k^-1 r)|^2=
 !        = 1/nkibz \sum_{S} \sum_{n k} f_{n k}/R_k |wf_{n k}(S^-1 r)|^2 =
 !        = 1/nsym \sum_{S} ( \sum_{n k} f_{n k} w_k |wf_{n k}(S^-1 r)|^2 )
 !
 !where we have used the relation
 !
 ! w_k = nsym/(nkibz * rank(k))
 !
 !
 use pars,          ONLY:SP
 use com,           ONLY:warning
 use electrons,     ONLY:levels,n_spin,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev,mag_syms
 use FFT_m,         ONLY:fft_size,fft_rot_r,fft_rot_r_inv
 use wave_func,     ONLY:wf_state,wf
!#if defined _RT
! use SC,            ONLY:SC_bands
!#endif
 use parallel_m,    ONLY:PP_redux_wait,PAR_IND_Xk_ibz,PAR_IND_QP_G_b,&
&                        PAR_COM_QP_A2A,PAR_IND_QP,PAR_COM_QP_A2A
 !
 implicit none
 type(bz_samp)::Xk
!#if defined _RT
! complex(SP), intent(in), optional :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),Xk%nibz)
! integer,     intent(in) :: lowest_band
!#endif
 type(levels) ::en       
 real(SP)     ::rho(fft_size)
 logical,     intent(in) :: force_si
 !
 ! Work Space 
 !
 integer :: ib,ibp,ik,i_spin,ifft,rho_syms,ifft2,ir
 real(SP):: rho_no_sym(fft_size),f_occ
 logical :: warn_done
 integer :: iv_max,iv_min,ID,n_QP_A2A_cpu
 integer, allocatable ::states_filling(:,:,:)
 !
 rho=0._SP
 rho_no_sym=0._SP
 warn_done=.false.
 !
!#if defined _RT
! iv_max=SC_bands(2)
! iv_min=lowest_band
!#else
 iv_max=en%nbm
 iv_min=1
!#endif
 !
 ! March 2013 (Andrea)
 !
 ! The calculation of the density is tricky. Indeed the bands and k-points
 ! are distributed in the PAR_COM_QP_A2A groups in a non predictable manner.
 ! This means the bands and k-points can be duplicated and, for this reason,
 ! only the first CPU of the block that has that (n,k) must use it.
 ! This condiction in ensured by the line:
 ! 
 ! if (.not.all(states_filling(ib,ik,1:ID-1)==0))
 !
 n_QP_A2A_cpu=PAR_COM_QP_A2A%n_CPU
 !
 if (n_QP_A2A_cpu>1) then
   !
   allocate(states_filling(iv_max,Xk%nibz,n_QP_A2A_cpu))
   states_filling=0
   !
   ID=PAR_COM_QP_A2A%CPU_id+1
   !
   do ib=iv_min,iv_max
     do ik=1,Xk%nibz
        if (PAR_IND_QP_G_b%element_1D(ib).and.PAR_IND_Xk_ibz%element_1D(ik)) states_filling(ib,ik,ID)=1
     enddo
   enddo
   !
   call PP_redux_wait(states_filling,COMM=PAR_COM_QP_A2A%COMM)
   !
 endif
 !
 bands_loop: do ib=iv_min,iv_max
   !
   if (.not.PAR_IND_QP_G_b%element_1D(ib)) cycle
   !
   do ik=1,Xk%nibz
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     if (n_QP_A2A_cpu>1) then
       if (.not.all(states_filling(ib,ik,1:ID-1)==0)) cycle
     endif
     !
     do i_spin=1,n_spin
       !
       if (size(wf_state,3)<i_spin) cycle
       if (size(wf_state,1)<ib) cycle
       if (size(wf_state,2)<ik) cycle
       !
       ifft=wf_state(ib,ik,i_spin)
       !
       if (ifft==0) then
         if (.not.warn_done) call warning('Not enough states to calculate rho')
         warn_done=.true.
         cycle
       endif
       !
!#if defined _RT
!        if(ib<SC_bands(1)) then
!          f_occ=en%f(ib,ik,1)
!        else
!          f_occ=real(-(0._SP,1._SP)*G_lesser(ib,ib,ik),SP)
!        endif
!#else
        f_occ=en%f(ib,ik,1)
        if (n_sp_pol==2) f_occ=en%f(ib,ik,i_spin)
!#endif
        rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(ik)*abs(wf(:,ifft))**2.
        !
      enddo
    enddo
  enddo bands_loop
  !
!#if defined _RT
!  !
!  ! Off-diagonal contributions
!  !
!  do ik=1,Xk%nibz
!    !      
!    do ib=iv_min,iv_max
!      !
!      do ibp=ib+1,SC_bands(2)
!        !      
!        if(ib<SC_bands(1).or.ibp<SC_bands(1)) cycle
!        !
!        ifft=wf_state(ib,ik,1)
!        ifft2=wf_state(ibp,ik,1)
!        !
!        if(ifft==0.or.ifft2==0) cycle
!        !
!        rho_no_sym(:)=rho_no_sym(:)-Xk%weights(ik)*2._SP*real(wf(:,ifft) &
!&                 *conjg(wf(:,ifft2))*(0._SP,1._SP)*G_lesser(ib,ibp,ik),SP)
!        !
!      enddo
!    enddo
!    !
!  enddo
!#endif
  !
  if (n_QP_A2A_cpu>1) then
    deallocate(states_filling)
    call PP_redux_wait(rho_no_sym,COMM=PAR_COM_QP_A2A%COMM)
  else
    call PP_redux_wait(rho_no_sym)
  endif
  !
  ! Simmetrization
  !
  rho_syms=nsym/(i_time_rev+1)
  if(mag_syms) rho_syms=nsym 
  do ir=1,rho_syms
    rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(ir,:)),SP)/real(nsym,SP)
  enddo
  if (mag_syms)      return
  if (.not.force_si) rho(:)=(1+i_time_rev)*rho(:)
  if (     force_si) rho(:)=   i_time_rev *real(rho(fft_rot_r_inv(:)),SP)+rho(:)
  !
end subroutine
