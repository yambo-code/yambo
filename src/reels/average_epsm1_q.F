!
! Copyright (C) 2000-2011 C. Hogan and the YAMBO team
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
subroutine average_epsm1_q(epsm1_av, iq, Xen, Xk, X, q, wq)
  !
  ! Calculates eps^1(iq) averaged on iq and iq-1
  !
  use X_m,                     only : X_t
  use pars,                    only : SP
  use units,                   only : HA2EV
  use R_lattice,               only : bz_samp
  use frequency,               only : w_samp
  use electrons,               only : levels
  use eels_qpt,                only : iqmin, iqmax, nqpar, qpar, print_qpar, iqpar 
  use eels_gvec,               only : setup_iGzrot, modify_Xgspace
  use eels_wgrid,              only : nw, nwp, select_wgrid, add_broad, nwsec
  use eels_inveps,             only : epsm1_avA, average_inveps, interpolate_inveps, rotate_eps
  use reels_module,            only : lepsmix
  implicit none
  integer,              intent(in) :: iq     ! The index in iqpar
  type(levels),         intent(in) :: Xen
  type(bz_samp),        intent(in) :: Xk, q  ! K/Q points
  type(X_t),         intent(inout) :: X      ! Polarization
  type(w_samp),      intent(inout) :: wq     ! Energy ranges
  complex(SP),         intent(out) :: epsm1_av(X%ng, X%ng, nwsec)  
! ws
  complex(SP)                      :: epsm1_avB(X%ng, X%ng, nwp)
  complex(SP)                      :: epsm1(X%ng,X%ng,nw)
  integer                          :: iqi, isi, iqf, ngblk, is
  !
  ! Define some useful indices
  !
  ngblk = X%ng
  iqf = iqpar(iq)         ! iqf = iq(fbz)
  iqi = q%sstar(iqf, 1) ! iqi = iq(ibz)
  isi = q%sstar(iqf, 2)
  call print_qpar(iq,iqf,iqi,qpar(iq,:),q%ptbz(iqf,:),q%pt(iqi,:))
  !
  ! Generate what?
  !
  call setup_iGzrot(iqi, isi,   ngblk )
  !
  ! Generate g_rot and bare_qpg on the new Gzrot mesh
  !
  call modify_Xgspace( iqi,  ngblk )
  !
  ! Select the w-grid for this iq, put into wq
  !
  call select_wgrid(wq, iq)
  call add_broad(wq)
  !
  ! Calculate eps^1 for the irreducible q (IBZ)
  !
  write(*,*) "calc_eps"
  call calc_eps( iqi, Xen, Xk, X, q, wq, epsm1, ngblk, nw, iqf )
  !
  call rotate_eps( iqi, iqf, isi, epsm1 )
  !
  ! If no symmetry present, force to be symmetric (physical) in plane
  !
  call average_inveps
  !
  ! If not interpolating, copy and return (nwsec = nw)
  !
  if(.not.lepsmix) then
    epsm1_av(1:ngblk,1:ngblk,1:nwsec) = epsm1(1:ngblk,1:ngblk,1:nwsec)
    return
  endif
  !
  ! Mixing: Calculate epsm1 interpolated from A(iq-1) and B(iq)
  !
! select case(iqmin-iq)
! case(0)
  if(iq.eq.iqmin) then
    !
    ! No interpolation for first iqpar
    !
    write(*,*) "CASE 0 av <- epsm1",iq,lepsmix,nwsec
    epsm1_av(1:ngblk,1:ngblk,1:nwsec) = epsm1(1:ngblk,1:ngblk,1:nwsec)
    !
    ! Save upper part of epsm1 for next iq
    !
    epsm1_avA(:,:, 1:nwp-1 ) = epsm1(:,:, nwp+1:nwsec ) 
    epsm1_avA(:,:, nwp     )     = 0.0_SP
    !
  else if(iq.eq.iqmax) then
    write(*,*) "CASE max av <- epsm1",iq,lepsmix,nwsec
    epsm1_avB(:,:,1:nwp) = epsm1(:,:,   1:nwp ) 
    call interpolate_inveps( epsm1_av(:,:,1:nwp), epsm1_avB )
    epsm1_av(1:ngblk,1:ngblk,nwp+1:nwsec) = epsm1(1:ngblk,1:ngblk,nwp+1:nwsec)
  else
! case default
    !
    ! Interpolate eps^-1 for iq-1 and iq exact frequencies
    !
    write(*,*) "CASE else av <- epsm1",iq,lepsmix,nwsec
    epsm1_avB(:,:,1:nwp) = epsm1(:,:,   1:nwp ) 
    call interpolate_inveps( epsm1_av, epsm1_avB )
    epsm1_avA(:,:, 1:nwp-1 ) = epsm1(:,:, nwp+1:nwsec ) 
    epsm1_avA(:,:, nwp     )     = 0.0_SP
    !
  endif
! end select
  !
  ! Copy upper half for next iq
  !
!   write(*,*) "copyint",size(epsm1_avA,3),size(epsm1,3),nwp,nw
! epsm1_avA(:,:,1:nwp) = epsm1(:,:, nwp:nw  ) 
    write(*,*) "done average epsm1"
  !
  return
end subroutine average_epsm1_q
