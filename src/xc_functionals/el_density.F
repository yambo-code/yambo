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
 use com,           ONLY:warning,error
 use electrons,     ONLY:levels,n_spin,n_sp_pol
 use R_lattice,     ONLY:bz_samp
 use D_lattice,     ONLY:nsym,i_time_rev,mag_syms
 use FFT_m,         ONLY:fft_size,fft_rot_r,fft_rot_r_inv
 use wave_func,     ONLY:wf_state,wf
#if defined _RT
 use SC,            ONLY:SC_bands
#endif
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 !
 implicit none
 type(bz_samp)::Xk
#if defined _RT
 complex(SP), intent(in), optional :: G_lesser(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),Xk%nibz)
 integer,     intent(in) :: lowest_band
#endif
 type(levels) ::en       
 real(SP)     ::rho(fft_size)
 logical,     intent(in) :: force_si
 !
 ! Work Space 
 !
 integer :: i1,i2,ik,i_spin,ifft,rho_syms,ifft2
 real(SP):: rho_no_sym(fft_size),f_occ
 logical :: warn_done
 integer :: iv_max,iv_min
 type(pp_indexes) ::px
 !
 rho=0._SP
 rho_no_sym=0._SP
 warn_done=.false.
 !
#if defined _RT
 iv_max=SC_bands(2)
 iv_min=lowest_band
#else
 iv_max=en%nbm
 iv_min=1
#endif
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/Xk%nibz,iv_max/),low_range=(/1,iv_min/))
 !
 do i1=iv_min,iv_max
   do i2=1,Xk%nibz
     !
     if (.not.px%element_2D(i2,i1)) cycle
     !        
     i_spin=1
     !
     ! GPL_EXCLUDE_START SPIN_IGNORE
     !
     do i_spin=1,n_spin
       !
       if (size(wf_state,3)<i_spin) cycle
       !
       ! GPL_EXCLUDE_END SPIN_IGNORE
       !
       if (size(wf_state,1)<i1) cycle
       if (size(wf_state,2)<i2) cycle
       !
       ifft=wf_state(i1,i2,i_spin)
       !
       if (ifft==0) then
         if (.not.warn_done) call warning('Not enough states to calculate rho')
         warn_done=.true.
         cycle
       endif
       !
#if defined _RT
        if(i1<SC_bands(1)) then
          f_occ=en%f(i1,i2,1)
        else
          f_occ=real(-(0._SP,1._SP)*G_lesser(i1,i1,i2),SP)
        endif
#else
        f_occ=en%f(i1,i2,1)
        !
        ! GPL_EXCLUDE_START SPIN_IGNORE
        !
        if (n_sp_pol==2) f_occ=en%f(i1,i2,i_spin)
        !
        ! GPL_EXCLUDE_END SPIN_IGNORE
        !
#endif
        rho_no_sym(:)=rho_no_sym(:)+f_occ*Xk%weights(i2)*abs(wf(:,ifft))**2.
        !
        ! GPL_EXCLUDE_START SPIN_IGNORE
        !
      enddo
      !
      ! GPL_EXCLUDE_END SPIN_IGNORE
      !
    enddo
  enddo
  !
#if defined _RT
  !
  ! Off-diagonal contributions
  !
  do ik=1,Xk%nibz
    !      
    do i1=iv_min,iv_max
      !
      if (.not.px%element_2D(ik,i1)) cycle
      !
      do i2=i1+1,SC_bands(2)
        !      
        if(i1<SC_bands(1).or.i2<SC_bands(1)) cycle
        !
        ifft=wf_state(i1,ik,1)
        ifft2=wf_state(i2,ik,1)
        !
        if(ifft==0.or.ifft2==0) cycle
        !
        rho_no_sym(:)=rho_no_sym(:)-Xk%weights(ik)*2._SP*real(wf(:,ifft) &
&                 *conjg(wf(:,ifft2))*(0._SP,1._SP)*G_lesser(i1,i2,ik),SP)
        !
      enddo
    enddo
    !
  enddo
#endif
  !
  call pp_redux_wait(rho_no_sym)
  call pp_indexes_reset(px)
  !
  ! Simmetrization
  !
  rho_syms=nsym/(i_time_rev+1)
  if(mag_syms) rho_syms=nsym 
  do i1=1,rho_syms
    rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(i1,:)),SP)/real(nsym,SP)
  enddo
  if (mag_syms)      return
  if (.not.force_si) rho(:)=(1+i_time_rev)*rho(:)
  if (     force_si) rho(:)=   i_time_rev *real(rho(fft_rot_r_inv(:)),SP)+rho(:)
  !
end subroutine
