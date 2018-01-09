!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine X_Double_Grid_setup(iq)
 !
 ! Define which kpts I will use in a consistent way between q=0 and q/=0
 ! For q/=0 this change makes the code much faster as it avoids the use of all
 ! double-grid kpts in the two spheres (k and kp) which would smear out too much
 ! the absorption.
 ! For q==0 the approach can be used to properly describe the q->0 limit
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:use_X_DbGd,X_DbGd_nkpt_at_q,X_DbGd_use_kpt,X_DbGd_q_size
 use electrons,     ONLY:BZ_DbGd_nkpt,BZ_DbGd_kpts,BZ_DbGd_table
 use R_lattice,     ONLY:nXkbz
 use R_lattice,     ONLY:q_pt,nqibz,qindx_X
 use vec_operate,   ONLY:iku_v_norm
 use parallel_m,    ONLY:PAR_IND_Xk_bz,PAR_Xk_nbz,PAR_IND_Xk_bz,PAR_Xk_bz_index
 use BS_solvers,    ONLY:BSS_uses_DbGd
 !
#include<memory.h>
 !
 integer :: iq
 ! 
 ! Work Space
 !
 integer   :: NK,iqbz,ik,ip,ikbz,ikbz_rand,ikpbz_rand,ikpbz,ikbz_mem
 real(SP)  :: q_pt_shift(3),q_norm_ref
 !
 if (.not.use_X_DbGd.and..not.BSS_uses_DbGd) return
 !
 NK=maxval(BZ_DbGd_nkpt)
 !
 YAMBO_ALLOC(X_DbGd_nkpt_at_q,(PAR_Xk_nbz,NK))
 YAMBO_ALLOC(X_DbGd_use_kpt,(PAR_Xk_nbz,NK,NK))
 !
 X_DbGd_nkpt_at_q=0
 X_DbGd_use_kpt  =.FALSE.
 !
 q_norm_ref=100000._SP
 do iqbz=2,nqibz
   q_norm_ref=min(q_norm_ref,iku_v_norm(q_pt(iqbz,:)))
 enddo
 !
 q_norm_ref=q_norm_ref*X_DbGd_q_size
 !
 do ikbz=1,nXkbz
   !
   if (.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
   !
   ikbz_mem=PAR_Xk_bz_index(ikbz)
   ikpbz   =qindx_X(iq,ikbz,1) 
   !
   do ik=1,BZ_DbGd_nkpt(ikbz)
     ikbz_rand = BZ_DbGd_table(ikbz,ik,2)
     do ip=1,BZ_DbGd_nkpt(ikpbz)
       !
       ikpbz_rand = BZ_DbGd_table(ikpbz,ip,2)
       q_pt_shift(:)=BZ_DbGd_kpts(ikbz_rand,:)-BZ_DbGd_kpts(ikpbz_rand,:)-q_pt(iq,:)
       !
       if ( iku_v_norm(q_pt_shift) > q_norm_ref ) cycle
       !
       X_DbGd_nkpt_at_q(ikbz_mem,ik)=X_DbGd_nkpt_at_q(ikbz_mem,ik)+1
       X_DbGd_use_kpt(ikbz_mem,ik,ip)=.true.
       !
     enddo
   enddo
 enddo
 !
end subroutine
