!
! License-Identifier: GPL
!
! Copyright (C) 2018 The Yambo Team
!
! Authors (see AUTHORS file for details): AF IM
!
subroutine qe_pseudo_init()
 !
 use pars,              ONLY: DP, pi_DP
 !
 ! data already stored by p2y
 use pw_data,           ONLY: nat_, tau_, ityp_, nsp_, psfile, n_spin_pw_, noncolin_, lspinorb_, &
                              alat_, a1_, a2_, a3_, ecutwfc_, ecutrho_, npwx_, ngm_, igv_, nr1_,nr2_,nr3_
 !
 use electrons,         ONLY: n_sp_pol
 !
 ! qe_pseudo internal modules
 use io_files,          ONLY: pseudo_dir, qe_psfile => psfile
 use ions_base,         ONLY: qe_nsp => nsp, qe_nat => nat, qe_tau => tau, qe_ityp => ityp
 use cell_base,         ONLY: qe_omega => omega, qe_tpiba => tpiba, qe_alat=>alat
 use lsda_mod,          ONLY: qe_nspin => nspin
 use noncollin_module,  ONLY: qe_noncolin => noncolin, qe_npol => npol, qe_nspin_mag=>nspin_mag
 use spin_orb,          ONLY: qe_lspinorb => lspinorb
 use control_flags,     ONLY: qe_gamma_only => gamma_only
 use wvfct,             ONLY: qe_npwx => npwx
 use gvect,             ONLY: qe_g => g, qe_gg => gg, qe_ngm=>ngm, qe_ecutrho=>ecutrho, &
                              qe_gcutm => gcutm, qe_mill=>mill, &
                              qe_eigts1 => eigts1, qe_eigts2 => eigts2, qe_eigts3 => eigts3
 use gvecw,             ONLY: qe_ecutwfc => ecutwfc, qe_gcutw => gcutw
 !
 use matrix_inversion,  ONLY: invmat
 !
#include<memory.h>
 !
 real(DP), parameter :: Ha2Ry=2.0_DP
 character(14)       :: subname="qe_pseudo_init"
 real(DP) :: amat(3,3),bmat(3,3)
 integer  :: i,ig,isp
 complex(DP), allocatable :: strf(:,:)

 qe_nsp = nsp_
 qe_nat = nat_
 if (.not. allocated(tau_))  call errore(subname,"tau_ not alloc",10)
 if (.not. allocated(ityp_)) call errore(subname,"ityp_ not alloc",10)
 allocate(qe_tau(3,qe_nat))
 allocate(qe_ityp(qe_nat))
 qe_tau = tau_/alat_
 qe_ityp = ityp_
 !
 pseudo_dir="./"
 do isp = 1, nsp_
   qe_psfile(isp) = trim(psfile(isp))
 enddo
 !
 qe_nspin = n_spin_pw_
 qe_nspin_mag = n_sp_pol
 qe_noncolin = noncolin_
 qe_lspinorb = lspinorb_
 !
 if (qe_noncolin) then
   qe_npol = 2
 else
   qe_npol = 1
 endif
 !
 qe_gamma_only = .false.
 ! 
 amat(:,1)=a1_
 amat(:,2)=a2_
 amat(:,3)=a3_
 call invmat(3,amat,bmat,qe_omega)
 bmat=transpose(bmat)
 bmat=bmat*2.0_DP * pi_DP
 !
 qe_alat = alat_
 qe_tpiba = 2.0_DP * pi_DP / alat_
 !
 qe_npwx = npwx_
 qe_ngm = ngm_
 qe_ecutwfc = ecutwfc_ * Ha2Ry
 qe_gcutw = qe_ecutwfc / qe_tpiba**2
 qe_ecutrho = ecutrho_ * Ha2Ry
 qe_gcutm = qe_ecutrho /qe_tpiba**2
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
 allocate(qe_eigts1(-nr1_:nr1_,nat_))
 allocate(qe_eigts2(-nr2_:nr2_,nat_))
 allocate(qe_eigts3(-nr3_:nr3_,nat_))
 allocate(strf(ngm_,nsp_))
 !
 bmat=bmat/qe_tpiba
 !
 call struc_fact(qe_nat,qe_tau,qe_nsp,qe_ityp,qe_ngm,qe_g,bmat,nr1_,nr2_,nr3_, &
&                .false.,strf,.true.,qe_eigts1,qe_eigts2,qe_eigts3)
 deallocate(strf)
 !
end subroutine qe_pseudo_init

