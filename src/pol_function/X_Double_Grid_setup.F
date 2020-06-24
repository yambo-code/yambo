!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine X_Double_Grid_setup(iq,k)
 !
 ! Define which kpts I will use in a consistent way between q=0 and q/=0
 ! For q/=0 this change makes the code much faster as it avoids the use of all
 ! double-grid kpts in the two spheres (k and kp) which would smear out too much
 ! the absorption.
 ! For q==0 the approach can be used to properly describe the q->0 limit
 !
 use pars,          ONLY:SP
 use X_m,           ONLY:use_X_DbGd,X_DbGd_nkpt_at_q,X_DbGd_percentual,X_DbGd_nkpt_at_q_table
 use com,           ONLY:msg
 use stderr,        ONLY:intc
 use electrons,     ONLY:BZ_DbGd_nkpt,BZ_DbGd_kpts,BZ_DbGd_table
 use R_lattice,     ONLY:nXkbz,bz_samp
 use R_lattice,     ONLY:q_pt,nqibz,qindx_X
 use vec_operate,   ONLY:v_norm,iku_v_norm
 use parallel_m,    ONLY:PAR_Xk_nbz,PP_redux_wait,PAR_IND_Xk_bz,PAR_Xk_bz_index,PAR_COM_Xk_bz_INDEX
 use BS_solvers,    ONLY:BSS_uses_DbGd
 !
#include<memory.h>
 !
 integer       :: iq
 type(bz_samp) :: k
 ! 
 ! Work Space
 !
 integer   :: iq_,ik,ip,ikbz,ikbz_rand,ipbz_rand,ipbz,ikbz_mem,N_all_pairs
 real(SP)  :: central_q(3),running_q(3),q_radius,PREC(1)
 logical   :: FILL_it
 !
 if (.not.use_X_DbGd.and..not.BSS_uses_DbGd) return
 !
 YAMBO_ALLOC(X_DbGd_nkpt_at_q,(PAR_Xk_nbz))
 !
 q_radius=100000._SP
 do iq_=2,nqibz
   q_radius=min(q_radius,iku_v_norm(q_pt(iq_,:)))
 enddo
 q_radius=q_radius/100.
 !
 PREC=0.
 FILL_it=.FALSE.
 !
 N_all_pairs=0
 do ikbz=1,nXkbz
   ipbz    =qindx_X(iq,ikbz,1) 
   N_all_pairs=N_all_pairs+BZ_DbGd_nkpt(ikbz)*BZ_DbGd_nkpt(ipbz)
 enddo
 !
 do while(PREC(1)<X_DbGd_percentual.or.FILL_it)
   !
   if (FILL_it) then
     YAMBO_ALLOC(X_DbGd_nkpt_at_q_table,(PAR_Xk_nbz, maxval(X_DbGd_nkpt_at_q) ,2))
   else
     q_radius=q_radius*1.5
   endif
   !
   X_DbGd_nkpt_at_q=0
   !
   do ikbz=1,nXkbz
     !
     if (.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
     !
     ikbz_mem=PAR_Xk_bz_index(ikbz)
     ipbz    =qindx_X(iq,ikbz,1) 
     !
     central_q=BZ_DbGd_kpts(BZ_DbGd_table(ipbz,BZ_DbGd_nkpt(ipbz),2),:)-&
&              BZ_DbGd_kpts(BZ_DbGd_table(ikbz,BZ_DbGd_nkpt(ikbz),2),:)
     !
     do ik=1,BZ_DbGd_nkpt(ikbz)
       !
       ikbz_rand = BZ_DbGd_table(ikbz,ik,2)
       !
       do ip=1,BZ_DbGd_nkpt(ipbz)
         !
         ipbz_rand = BZ_DbGd_table(ipbz,ip,2)
         !
         running_q(:)=BZ_DbGd_kpts(ipbz_rand,:)-BZ_DbGd_kpts(ikbz_rand,:)-central_q
         !
         if (iku_v_norm( running_q ) > q_radius) cycle
         !
         X_DbGd_nkpt_at_q(ikbz_mem)=X_DbGd_nkpt_at_q(ikbz_mem)+1
         if (FILL_it) then
           X_DbGd_nkpt_at_q_table(ikbz_mem, X_DbGd_nkpt_at_q(ikbz_mem) ,:)=(/ik,ip/)
         endif
         !
       enddo
     enddo
     !
   enddo
   !
   PREC=real(sum(X_DbGd_nkpt_at_q))/real(N_all_pairs)*100.
   !
   call PP_redux_wait(PREC,COMM=PAR_COM_Xk_bz_INDEX%COMM)
   !
   if (FILL_it) exit
   !
   if (PREC(1)>=X_DbGd_percentual) FILL_it=.TRUE.
   !
 enddo 
 !
 call msg('nrsn','DbGd@q['//trim(intc(iq))//'] k->k-q transitions included [%]:',PREC)
 !
end subroutine
