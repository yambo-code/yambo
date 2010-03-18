!
! Copyright (C) 2000-2010 D. De Fausti and the YAMBO team
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
subroutine el_density(G_lesser,Xk,rho,force_si)
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
  use D_lattice,     ONLY:nsym,i_time_rev
  use FFT_m,         ONLY:fft_size,fft_rot_r
  use wave_func,     ONLY:wf_state,wf
#if defined _RT
  use SC,            ONLY:SC_bands
#endif
#if defined _RT || defined _DISTRIBUTED
  use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
  use par_indexes_m, ONLY:par_indexes
#endif
  !
  implicit none
  type(bz_samp)::Xk
#if defined _RT
  complex(SP), intent(in), optional :: G_lesser(SC_bands,SC_bands,Xk%nibz)
#else
  type(levels) ::en       
#endif
  real(SP)     ::rho(fft_size)
  logical,     intent(in) :: force_si
  !
  ! Work Space 
  !
  integer :: i1,i2,ik,i_spin,ifft,rho_syms,ifft2
  real(SP):: rho_no_sym(fft_size),f_occ
  logical :: warn_done
  integer :: iv_max
#if defined _RT || defined _DISTRIBUTED
  type(pp_indexes) ::px
#endif
  !
  rho=0._SP
  rho_no_sym=0._SP
  warn_done=.false.
  !
#if defined _RT
  iv_max=SC_bands
#else
  iv_max=en%nbm
#endif
  !
#if defined _RT || defined _DISTRIBUTED
  call pp_indexes_reset(px)
  call par_indexes(px,(/Xk%nibz,iv_max/))
#endif
  !
  do i1=1,iv_max
    do i2=1,Xk%nibz
      !
#if defined _RT || defined _DISTRIBUTED
      if (.not.px%element_2D(i2,i1)) cycle
#endif
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
        f_occ=real(-(0._SP,1._SP)*G_lesser(i1,i1,i2),SP)
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
    do i1=1,SC_bands
      !
      if (.not.px%element_2D(ik,i1)) cycle
      !
      do i2=i1+1,SC_bands
        !      
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
#if defined _RT || defined _DISTRIBUTED
  call pp_redux_wait(rho_no_sym)
  call pp_indexes_reset(px)
#endif
  !
  ! Simmetrization
  !
  rho_syms=nsym/(i_time_rev+1)
  if (force_si) rho_syms=nsym
  do i1=1,rho_syms
    rho(:)=rho(:)+real(rho_no_sym(fft_rot_r(i1,:)),SP)/real(nsym,SP)
  enddo
  !
  if (.not.force_si) rho=rho*(1+i_time_rev)
  !
end subroutine
