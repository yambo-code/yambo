!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, IM
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
subroutine qe_pseudo_init()
 !
 use pars,              ONLY: DP, pi_DP
 !
 ! data already stored by p2y
 use pw_data,           ONLY: nat_, tau_, ityp_, nsp_, psfile, nspin_, noncolin_, lspinorb_, &
                              alat_, a1_, a2_, a3_, ecutrho_, npwx_, ngm_, igv_
 !
 ! qe_pseudo internal modules
 use io_files,          ONLY: pseudo_dir, qe_psfile => psfile
 use ions_base,         ONLY: qe_nsp => nsp, qe_nat => nat, qe_tau => tau, qe_ityp => ityp
 use cell_base,         ONLY: qe_omega => omega, qe_tpiba => tpiba
 use lsda_mod,          ONLY: qe_nspin => nspin
 use noncollin_module,  ONLY: qe_noncolin => noncolin, qe_npol => npol 
 use spin_orb,          ONLY: qe_lspinorb => lspinorb
 use control_flags,     ONLY: qe_gamma_only => gamma_only
 use wvfct,             ONLY: qe_npwx => npwx
 use gvect,             ONLY: qe_g => g, qe_gg => gg, qe_ngm=>ngm, qe_ecutrho=>ecutrho, &
                              qe_gcutm => gcutm, qe_mill=>mill
 !
 use matrix_inversion,  ONLY: invmat
 !
#include<memory.h>
 !
 character(14) :: subname="qe_pseudo_init"
 real(DP):: amat(3,3),bmat(3,3)
 integer :: i,ig,isp

 qe_nsp = nsp_
 qe_nat = nat_
 if (.not. allocated(tau_))  call errore(subname,"tau_ not alloc",10)
 if (.not. allocated(ityp_)) call errore(subname,"ityp_ not alloc",10)
 allocate(qe_tau(3,qe_nat))
 allocate(qe_ityp(qe_nat))
 qe_tau = tau_
 qe_ityp = ityp_
 !
 pseudo_dir="./"
 do isp = 1, nsp_
   qe_psfile(isp) = trim(psfile(isp))
 enddo
 !
 qe_nspin = nspin_
 qe_noncolin = noncolin_
 qe_npol = 2
 qe_lspinorb = lspinorb_
 !
 qe_gamma_only = .false.
 ! 
 amat(:,1)=a1_
 amat(:,2)=a2_
 amat(:,3)=a3_
 call invmat(3,amat,bmat,qe_omega)
 bmat=bmat*2.0_DP * pi_DP
 !
 qe_tpiba = 2.0_DP * pi_DP / alat_
 !
 qe_npwx = npwx_
 qe_ngm = ngm_
 qe_ecutrho = ecutrho_
 qe_gcutm = ecutrho_/qe_tpiba**2
 !
 allocate(qe_g(3,ngm_),qe_gg(ngm_))
 allocate(qe_mill(3,ngm_))
 !
 qe_mill=igv_
 !
 do ig = 1, ngm_
    qe_g(:,ig)=0
    do i = 1, 3
      qe_g(:,ig) = qe_g(:,ig) + igv_(i,ig)*bmat(:,i)
    enddo
    qe_gg(ig) =sum(qe_g(:,ig)**2)
 enddo
 !
 qe_g  = qe_g / qe_tpiba
 qe_gg = qe_gg / qe_tpiba**2
 !
 !
end subroutine qe_pseudo_init

