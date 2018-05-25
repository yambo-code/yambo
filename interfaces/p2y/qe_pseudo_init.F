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
 use pw_data,           ONLY: nat_, tau_, ityp_, nsp_, psfile, nspin_, noncolin_, &
                              alat_, a1_, a2_, a3_, npwx_
 !
 ! qe_pseudo internal modules
 use io_files,          ONLY: pseudo_dir, qe_psfile => psfile
 use ions_base,         ONLY: qe_nsp => nsp, qe_nat => nat, qe_tau => tau, qe_ityp => ityp
 use cell_base,         ONLY: qe_omega => omega, qe_tpiba => tpiba
 use lsda_mod,          ONLY: qe_nspin => nspin
 use noncollin_module,  ONLY: qe_noncolin => noncolin, qe_npol => npol 
 use control_flags,     ONLY: qe_gamma_only => gamma_only
 use wvfct,             ONLY: qe_npwx => npwx
 !
 use matrix_inversion,  ONLY: invmat
 !
#include<memory.h>
 !
 character(14) :: subname="qe_pseudo_init"
 real(DP):: amat(3,3),a_inv(3,3)
 integer :: isp

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
 !
 qe_gamma_only = .false.
 ! 
 amat(:,1)=a1_
 amat(:,2)=a2_
 amat(:,3)=a3_
 call invmat(3,amat,a_inv,qe_omega)
 qe_tpiba = 2.0_DP * pi_DP / alat_
 !
 qe_npwx = npwx_
 !
end subroutine qe_pseudo_init

