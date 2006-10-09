!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
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
subroutine calc_eps( iq,  Xen,Xk, X, q, wq, epsm1, ngblk, nw ,iqf )
  !
  ! Here we calculate the required eps^{-1}_GzGz'(Q_||).
  ! Since X_os is calculated on the IBZ only, we calculate instead
  ! the equivalent eps^{-1}_G1z_G1z'(q)
  !                where: G1z = R^{-1}Gz and Q_||(FBZ) = R q(IBZ)
  ! X_GG' for a non-standard set of G,G' can be done simply by changing
  !     grot(1,iG=1:ngblk) = (iGnew=1:ngblk) for the chosen iGnew
  ! and cint%bqpg (the |q+G| indices)
  !
  ! arrays: iGzindx contain list of Gz
  !         iGzrotindx is list   of G1z
  !
  ! eps(hw) is calculated on the input mesh wq(1:nw)
  !
  use pars,            ONLY : PI, HARTREE, lchlen
  use com,            ONLY : msg
  use parser_m,       ONLY : parser
  use X_m,                    ONLY : X_t, X_mat, X_epsilon, X_alloc
  use frequency,               ONLY : w_samp
  use R_lattice,               ONLY : bz_samp, bare_qpg, ng_vec, g_vec
  use electrons,              ONLY : levels
  use eels_qpt
  use eels_gvec
! use eels_wgrid
  use eels_inveps
  use surface_geometry
  implicit none
  type(levels),       intent(in) :: Xen    ! Energies
  type(bz_samp),   intent(in) :: Xk, q  ! K/Q points
  type(X_t), intent(inout) :: X   ! Polarization
  type(w_samp), intent(in)       ::  wq    ! Energy ranges
  integer,            intent(in) :: ngblk, nw, iq, iqf  ! IBZ, FBZ
  complex(SP),        intent(out):: epsm1(ngblk,ngblk,nw)
!ws
  complex(SP)                    :: eps(ngblk,ngblk,nw),head(nw)
  integer                        :: i1,i2,fr(2),nau, irq, rtype, npol
  integer                        :: iGtmp(9,3), nwp
  complex(SP)                    :: epstmp(9,9)
 real(SP) :: v_module,od(5), deltab
  character(lchlen)              :: prmsg

  call X_alloc('X',(/X%ng,X%ng,wq%n(2)/)) ! X(G,G') matrix

  nwp = (nw+1)/2


  do i1 = 1, wq % n(1), wq % n(2) ! loop over all hw in units of n(2) blocks
    !
    fr = (/ i1, i1+wq%n(2)-1 /) ! the small block of frequencies
    !
    call X_os(X_mat,q,iq,fr,Xen,Xk,wq,X)   
    !
    eps(1:X%ng,1:X%ng,fr(1):fr(2)) = X_mat(:,:,:)  ! Save for all frequencies
    !
    head(fr(1):fr(2)) = 1.0_SP - X_mat(1,1,:)*4.0_SP*pi/ &
&                         bare_qpg(iq,1) / bare_qpg(iq,1)
    ! 
    call Xs(iq,fr,X,wq)
    !
    epsm1(1:X%ng,1:X%ng,fr(1):fr(2)) = X_mat(:,:,:)  ! Save for all frequencies
    !
  enddo
  !
  ! Fill in eps prefactors for eps: use modified cint
  !
  forall(i1=1:X%ng,i2=1:X%ng) &
&    eps(i1,i2,nwp)=1.0_SP - eps(i1,i2,nwp)*4.0_SP*pi / &
&                         bare_qpg(iq,1) / bare_qpg(iq,1)
  !
  !-> print eps block for iw* <----------------------------
  !
  call sort_eps(iGtmp,epstmp,iGz0indx,gvecaff,eps(:,:,nwp),ngblk)
  if(lorderG) write(prmsg,101) nwp, real(wq%p(nwp))*HARTREE,q%ptbz(iqf,1:3)
  if(.not.lorderG) write(prmsg,102) nwp, real(wq%p(nwp))*HARTREE,q%pt(iq,1:3)
  call print_eps(iGtmp,epstmp,ngblk,prmsg)
  !--------------------------------------------------------
  !
  !-> print epsm1 block for iw*
  !
  call sort_eps(iGtmp,epstmp,iGz0indx,gvecaff,epsm1(:,:,nwp),ngblk)
  if(lorderG) write(prmsg,103) nwp, real(wq%p(nwp))*HARTREE,q%ptbz(iqf,1:3)
  if(.not.lorderG) write(prmsg,104) nwp, real(wq%p(nwp))*HARTREE,q%pt(iq,1:3)
  call print_eps(iGtmp,epstmp,ngblk,prmsg)
  !--------------------------------------------------------
  !
  call X_alloc('X') ! X_mat
  !
  return
100 format(a,a,i4,a,f8.3,a,3f8.3)
101 format("---> eps_GzGz'(Q_||): iw*=",i3," hw=",f8.3," eV, Q=",3f8.3)
102 format("---> eps_GG'(q): iw*=",i3," hw=",f8.3," eV, q(IBZ)=",3f8.3)
103 format("---> eps^{-1}_GzGz'(Q_||): iw*=",i3," hw=",f8.3," eV, Q=",3f8.3)
104 format("---> eps^{-1}_GG'(q): iw*=",i3," hw=",f8.3," eV, q(IBZ)=",3f8.3)
end subroutine calc_eps
