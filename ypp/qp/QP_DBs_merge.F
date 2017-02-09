!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine QP_DBs_merge(en,k)
 !
 use YPP,           ONLY:n_QP_dbs,QP_db,QP_db_what,QP_db_name
#if defined _YPP_RT
 use YPP_real_time, ONLY:Z_input
#endif
 use pars,          ONLY:SP,cI,schlen
 use stderr,        ONLY:intc
 use com,           ONLY:msg
 use electrons,     ONLY:levels
 use IO_m,          ONLY:io_control,OP_WR_CL,REP
 use R_lattice,     ONLY:bz_samp,k_the_nearest
 use QP_m,          ONLY:max_qp_descs
 implicit none
 type(levels), intent(in) :: en
 type(bz_samp),intent(in) :: k
 !
 ! Work Space
 !
 integer :: i_qp,i_d,i_qp_ref,nk_max,nb_to_merge,i_s,i_b,i_k,i_k_near,&
&           i_err,i_sp,i1,ID
 real(SP):: k_qp(3),W_to_add,dE_to_add,COHSEX_eq,dE_Neq,Z_local
 character(schlen) :: new_descriptions(max_qp_descs)
 integer, external :: io_QP_and_GF
 !
 call section("=",'QP databases mergering')
 !=========================================
 !
 call msg('s','Files & Actions ...')
 !
 i_qp_ref=-1
 nk_max=-1
 nb_to_merge=1000
 !
 do i_qp=1,n_QP_dbs
   !
   call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   call msg('s','[DB #'//trim(intc(i_qp))//']'//trim(QP_db_what(i_qp))//" < "//trim(QP_db_name(i_qp)))
   call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   !
   do i_d=1,QP_db(i_qp)%n_descs
     call msg('s','[DB #'//trim(intc(i_qp))//']'//trim(QP_db(i_qp)%description(i_d)))
   enddo
   !
   nk_max=max(nk_max,QP_db(i_qp)%nk)
   !
   if (nk_max==QP_db(i_qp)%nk) i_qp_ref=i_qp
   !
   if (i_qp==n_QP_dbs) call msg('s',repeat("-",len_trim(QP_db_name(i_qp))+13))
   !
   nb_to_merge=min(nb_to_merge,QP_db(i_qp)%nb)
   !
 enddo
 !
 call msg('s','Reference Database is: '//trim(QP_db_name(i_qp_ref)))
 call msg('s','Bands mergered       : ',nb_to_merge)
 !
 call msg('s','Building up new QP properties... ')
 !================================================
 !
 do i_s=1,QP_db(i_qp_ref)%n_states
   !
   i_b    =QP_db(i_qp_ref)%table(i_s,1)
   i_k    =QP_db(i_qp_ref)%table(i_s,3)
   k_qp(:)=QP_db(i_qp_ref)%k(i_k,:)
   !
   if (i_b>nb_to_merge) cycle
   !
   db_loop: do i_qp=1,n_QP_dbs
     !
     if (i_qp==i_qp_ref) cycle
     !
     i_k_near=k_the_nearest(k_qp,QP_db(i_qp)%k(:,:),QP_db(i_qp)%nk,ROTATE_k_ref=.TRUE.,i_err=i_err)
     !
     do i_sp=1,QP_db(i_qp)%n_states
       if (QP_db(i_qp)%table(i_sp,3)==i_k_near.and.QP_db(i_qp)%table(i_sp,1)==i_b) then
         W_to_add=aimag(QP_db(i_qp)%E(i_sp))
         dE_to_add= real(QP_db(i_qp)%E(i_sp)-QP_db(i_qp)%E_bare(i_sp))
         if (index(trim(QP_db_what(i_qp)),"W")==0) W_to_add =0._SP
         if (index(trim(QP_db_what(i_qp)),"E")==0) dE_to_add=0._SP
         QP_db(i_qp_ref)%E(i_s)=QP_db(i_qp_ref)%E(i_s)+dE_to_add+cI*W_to_add
         !
         ! GPL_EXCLUDE_START
         !
#if defined _YPP_RT
         !
         COHSEX_eq= real(QP_db(i_qp)%E(i_sp)-QP_db(i_qp)%E_bare(i_sp))
         dE_Neq   = real(QP_db(i_qp)%E(i_sp)-QP_db(i_qp)%E_bare(i_sp))
         if (index(trim(QP_db_what(i_qp)),"C")==0) COHSEX_eq=0._SP
         if (index(trim(QP_db_what(i_qp)),"N")==0) dE_Neq   =0._SP
         if (Z_input<=0._SP) then
           if(index(trim(QP_db_what(i_qp)),"Z")==0) then
             Z_local=real(QP_db(i_qp_ref)%E(i_sp)-QP_db(i_qp_ref)%E_bare(i_sp))/ &
  &                  real(QP_db(1)%E(i_sp)-QP_db(1)%E_bare(i_sp))
           else
             Z_local=QP_db(i_qp_ref)%Z(i_sp)
           endif
         else
           Z_local=Z_input
         endif
         QP_db(i_qp_ref)%E(i_s)=QP_db(i_qp_ref)%E(i_s)+Z_local*(dE_Neq-COHSEX_eq)
         !
#endif
         !
         ! GPL_EXCLUDE_END
         !
         cycle db_loop
       endif
     enddo
     !
   enddo db_loop
   !
 enddo
 !
 call msg('l','done')
 !
 call msg('s','Building up new descriptions... ')
 !===============================================
 !
 i1=0
 !
 do i_qp=1,n_QP_dbs
   i1=i1+1
   new_descriptions(i1)=repeat("-",len_trim(QP_db_name(i_qp))+13)
   i1=i1+1
   new_descriptions(i1)='[DB #'//trim(intc(i_qp))//']'//trim(QP_db_what(i_qp))//"<"//trim(QP_db_name(i_qp))
   i1=i1+1
   new_descriptions(i1)=repeat("-",len_trim(QP_db_name(i_qp))+13)
   do i_d=1,QP_db(i_qp)%n_descs
     i1=i1+1
     new_descriptions(i1)='[DB #'//trim(intc(i_qp))//']'//trim(QP_db(i_qp)%description(i_d))
   enddo
 enddo
 !
 QP_db(i_qp_ref)%n_descs=i1
 QP_db(i_qp_ref)%description(:i1)=new_descriptions(:i1)
 !
 call msg('l','done')
 !
 call msg('s','I/O...')
 !=====================
 call io_control(ACTION=OP_WR_CL,COM=REP,SEC=(/1,2,3/),ID=ID)
 i_err=io_QP_and_GF('QP',QP_db(i_qp_ref),ID)
 call msg('l','done')
 !
end subroutine
