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
 use R_lattice,     ONLY:nXkbz,bz_samp
 use R_lattice,     ONLY:qindx_X,smallest_q_norm
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
 integer   :: ikbz,ikpbz,ikbz_mem,ikbz_FineGd,ikpbz_FineGd,N_all_pairs
 real(SP)  :: central_q(3),running_q(3),k_pt_loc(3),kp_pt_loc(3),q_radius,PERC(1)
 logical   :: FILL_it
 !
 if (.not.use_X_DbGd.and..not.BSS_uses_DbGd) return
 !
 YAMBO_ALLOC(X_DbGd_nkpt_at_q,(PAR_Xk_nbz))
 !
 if(.not.allocated(k%ptbz)) call k_ibz2bz(k,'i',.true.)
 !
 q_radius=smallest_q_norm/100._SP
 !
 PERC=0._SP
 FILL_it=.FALSE.
 !
 N_all_pairs=0
 do ikbz=1,nXkbz
   ikpbz      =qindx_X(iq,ikbz,1) 
   N_all_pairs=N_all_pairs+ (k%FGbz%k_range(ikbz,2)-k%FGbz%k_range(ikbz,1)+1) &
&                          *(k%FGbz%k_range(ikpbz,2)-k%FGbz%k_range(ikpbz,1)+1)
 enddo
 !
 do while(PERC(1)<X_DbGd_percentual.or.FILL_it)
   !
   if (FILL_it) then
     YAMBO_ALLOC(X_DbGd_nkpt_at_q_table,(PAR_Xk_nbz, maxval(X_DbGd_nkpt_at_q) ,2))
   else
     q_radius=q_radius*1.5_SP
   endif
   !
   X_DbGd_nkpt_at_q=0
   !
   do ikbz=1,nXkbz
     !
     if (.not.PAR_IND_Xk_bz%element_1D(ikbz)) cycle
     !
     ikbz_mem=PAR_Xk_bz_index(ikbz)
     !
     ikpbz   =qindx_X(iq,ikbz,1) 
     !
     central_q=k%ptbz(ikbz,:)-k%ptbz(ikpbz,:)
     !
     ! Coarse grid points are included for ikbz_FineGd= k%bz_DbGd_map(1,ikbz)
     ! Fine   grid points are included for ikbz_FineGd>k%bz_DbGd_map(1,ikbz)
     !
     do ikbz_FineGd=k%FGbz%k_range(ikbz,1),  k%FGbz%k_range(ikbz,2)
       if(ikbz_FineGd==k%FGbz%k_range(ikbz,1)) k_pt_loc=k%ptbz(ikbz,:)
       if(ikbz_FineGd> k%FGbz%k_range(ikbz,1)) k_pt_loc=k%FGbz%pt(ikbz_FineGd,:)
       do ikpbz_FineGd=k%FGbz%k_range(ikpbz,1),k%FGbz%k_range(ikpbz,2)
         if(ikpbz_FineGd==k%FGbz%k_range(ikpbz,1)) kp_pt_loc=k%ptbz(ikpbz,:)
         if(ikpbz_FineGd> k%FGbz%k_range(ikpbz,1)) kp_pt_loc=k%FGbz%pt(ikpbz_FineGd,:)
         !
         running_q(:)=(k_pt_loc-kp_pt_loc)
         !
         if (iku_v_norm( running_q - central_q ) > q_radius) cycle
         !
         X_DbGd_nkpt_at_q(ikbz_mem)=X_DbGd_nkpt_at_q(ikbz_mem)+1
         if (FILL_it) then
           X_DbGd_nkpt_at_q_table(ikbz_mem, X_DbGd_nkpt_at_q(ikbz_mem) ,:)=(/ikbz_FineGd,ikpbz_FineGd/)
         endif
         !
       enddo
     enddo
     !
   enddo
   !
   PERC=real(sum(X_DbGd_nkpt_at_q))/real(N_all_pairs)*100._SP
   !
   call PP_redux_wait(PERC,COMM=PAR_COM_Xk_bz_INDEX%COMM)
   !
   if (FILL_it) exit
   !
   if (PERC(1)>=X_DbGd_percentual) FILL_it=.TRUE.
   !
 enddo 
 !
 call msg('nrsn','DbGd@q['//trim(intc(iq))//'] k->k-q (trans %, q-rad %)]',(/PERC,q_radius/smallest_q_norm*100._SP/))
 !
end subroutine
