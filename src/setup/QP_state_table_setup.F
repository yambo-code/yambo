!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine QP_state_table_setup(en)
 !
 ! There are 2 objects that command the QP corrections
 ! indexes: QP_state & QP_table.
 !
 ! QP_table gives the band/k indexes for any QP state
 ! QP_state is T if that band/k is a QP state
 !
 ! QP_state is read from QP based DBs (xxvxc/QP...)
 !
 ! IN :  either input_file or QP_state
 ! Out:  QP_table(1:QP_n_states,:) = (n,n`,ikibz)
 !       QP_state
 !
 use pars,          ONLY:SP
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_sc_run,l_real_time,l_eval_collisions
 use QP_m,          ONLY:QP_nk,QP_nb,QP_state,QP_n_states,QP_table,SC_bands_mixed,SC_band_mixing 
 use electrons,     ONLY:levels,n_sp_pol
 use parser_m,      ONLY:parser
#if defined _SC || defined _RT
 use hamiltonian,   ONLY:H_ref_bands,H_potential
 use drivers,       ONLY:l_elel_scatt
#endif
 !
#include<memory.h>
 type(levels)::en
 !
 ! Work Space
 !
 integer :: e_rng_lines,k_rng_lines,icheck,ik,ib,i1,i2,i3,&
&           QP_k_nk,QP_e_nk,QP_k_nb,QP_e_nb,i_sp,bands_mixed,v(2)
 real(SP):: rcheck
 logical :: Mixing
 integer,    allocatable:: QP_i_limits(:,:)
 real(SP)   ,allocatable:: QP_r_limits(:,:)
 !
 if (.not.allocated(QP_state)) then
   !
   !Input File -> QP_table
   !
   ! %QPkrange fields
   !
   k_rng_lines=1
   icheck=1
   do while(icheck/=0)
     YAMBO_FREE(QP_i_limits)
     YAMBO_ALLOC(QP_i_limits,(k_rng_lines,4))
     QP_i_limits=0
     call parser('QPkrange',QP_i_limits)
     icheck=sum(QP_i_limits(k_rng_lines,:))
     if (any(QP_i_limits(k_rng_lines,:)<0)) icheck=0
     k_rng_lines=k_rng_lines+1
   enddo
   k_rng_lines=k_rng_lines-2
   do i1=1,k_rng_lines,1
     call i_check(QP_i_limits(i1,:2),en%nk)
     call i_check(QP_i_limits(i1,3:),en%nb)
   enddo
   !
   ! %QPerange fields
   !
   e_rng_lines=1
   rcheck=1._SP
   do while(rcheck>0._SP)
     YAMBO_FREE(QP_r_limits)
     YAMBO_ALLOC(QP_r_limits,(e_rng_lines,4))
     QP_r_limits(e_rng_lines,3:4)=(/0._SP,-1._SP/)
     call parser('QPerange',QP_r_limits)
     rcheck=QP_r_limits(e_rng_lines,4)-QP_r_limits(e_rng_lines,3)
     e_rng_lines=e_rng_lines+1
   enddo
   QP_r_limits(:,3:4)=QP_r_limits(:,3:4)/HA2EV
   e_rng_lines=e_rng_lines-2
   do i1=1,e_rng_lines,1
     v=int(QP_r_limits(i1,:2))
     call i_check(v,en%nk)
     QP_r_limits(i1,:2)=real(v,SP)
   enddo
   !
   ! Non null %QPerange/%QPkrange fields
   !
   if (.not.all((/k_rng_lines==0,e_rng_lines==0/))) then
     !
     ! QP_nk setup
     !
     QP_k_nk=0
     QP_e_nk=0
     if (k_rng_lines>0) QP_k_nk=    maxval(QP_i_limits(:,1:2))
     if (e_rng_lines>0) QP_e_nk=int(maxval(QP_r_limits(:,1:2)))
     if (max(QP_k_nk,QP_e_nk)>0) QP_nk=max(QP_k_nk,QP_e_nk)
     !
     ! QP_nb setup
     !
     QP_k_nb=0
     QP_e_nb=0
     if (k_rng_lines>0) QP_k_nb=maxval(QP_i_limits(:,3:4))
     if (allocated(en%E)) then
       do i1=1,e_rng_lines
         do ik=int(QP_r_limits(i1,1)),int(QP_r_limits(i1,2)),1
           do ib=1,en%nb
             if (en%E(ib,ik,1)<QP_r_limits(i1,4)) QP_e_nb=max(QP_e_nb,ib)
           enddo
         enddo
       enddo
     endif
     if ( max(QP_k_nb,QP_e_nb)>0) QP_nb=max(QP_k_nb,QP_e_nb)
     !
     ! QP_state setup
     !
     YAMBO_ALLOC(QP_state,(QP_nb,QP_nk))
     QP_state(:,:)=.false.
     do i1=1,k_rng_lines
       QP_state(QP_i_limits(i1,3):QP_i_limits(i1,4),QP_i_limits(i1,1):QP_i_limits(i1,2))=.true.
     enddo
     if (allocated(en%E)) then
       do i1=1,e_rng_lines
         do ik=int(QP_r_limits(i1,1)),int(QP_r_limits(i1,2)),1
           do ib=1,en%nb
             if (all((/QP_r_limits(i1,3)<=en%E(ib,ik,1),&
&                      en%E(ib,ik,1)<=QP_r_limits(i1,4)/))) QP_state(ib,ik)=.true.
           enddo
         enddo
       enddo
     endif
     !
   else
     !
     if (.not.allocated(en%E)) return
     YAMBO_ALLOC(QP_state,(QP_nb,QP_nk))
     QP_state(:,:)=.true.
     !
   endif
   !
   YAMBO_FREE(QP_i_limits)
   YAMBO_FREE(QP_r_limits)
   !
 endif
 !
 ! Band Mixing
 !=============
 !
 ! Note: only upper triangle of the table is filled. 
 !       This corresponds to assume symmetric/hermitian 
 !       matrixes
 !
 ! Note (2) : a non zero mixing is allowed only in a SC/RT calculation
 !
 bands_mixed   =1
 SC_bands_mixed=1
 !
 Mixing=.FALSE.
#if defined _SC || defined _RT
 Mixing=l_sc_run.or.l_real_time.or.l_eval_collisions
 if (l_eval_collisions) Mixing=l_elel_scatt.or.trim(H_potential)=="HARTREE+FOCK".or.trim(H_potential)=="COHSEX"
#endif
 !
 if (Mixing) then
   !
#if !defined _ELPH
   if (any(.not.QP_state(1,:)).or.any(.not.QP_state(QP_nb,:))) SC_band_mixing=0._SP
#endif
   !
   if (SC_band_mixing<0._SP.or.SC_band_mixing>100._SP) SC_band_mixing=0._SP
   if (SC_band_mixing>0._SP) SC_bands_mixed=nint(QP_nb*SC_band_mixing/100._SP)
   if (SC_bands_mixed==0) SC_bands_mixed=1
   !
   ! Recalculated SC/RT band mixing
   !
   SC_band_mixing=float(SC_bands_mixed)*100._SP/float(QP_nb)
   bands_mixed=SC_bands_mixed
   !
 endif
 !
 ! QP_table allocation
 !
 QP_n_states=product(shape(pack(QP_state,QP_state)))*(2*bands_mixed-1)
 QP_n_states=QP_n_states*n_sp_pol
 !
 if (.not.allocated(QP_table)) then
   YAMBO_ALLOC(QP_table,(QP_n_states,3+n_sp_pol-1))
   QP_table=0
 endif
 !
 QP_n_states=0
 !
 do i1=1,QP_nk
   do i2=1,QP_nb
     !
     if (.not.QP_state(i2,i1)) cycle
     !
     do i_sp=1,n_sp_pol
       do i3=0,bands_mixed-1
         !
         if (i2+i3>QP_nb.or.i2+i3<1) cycle
         !
#if defined _SC || defined _RT
         if ((l_sc_run.or.l_real_time) .and. (i2<H_ref_bands(1)) ) cycle
#endif
         !
         QP_n_states=QP_n_states+1
         QP_table(QP_n_states,1)=i2
         QP_table(QP_n_states,2)=i2+i3
         QP_table(QP_n_states,3)=i1
         if (n_sp_pol==2) QP_table(QP_n_states,4)=i_sp
         !
         !
       enddo
     enddo
   enddo
 enddo
 !
 contains
   !
   subroutine i_check(k_i,ilim)
     integer :: k_i(2),ilim
     if (k_i(1)>ilim.or.k_i(1)<1) k_i(1)=1
     if (k_i(2)>ilim.or.k_i(2)<1) k_i(2)=ilim
     if (k_i(1)>k_i(2)) k_i(1)=1
   end subroutine
   !
end subroutine
