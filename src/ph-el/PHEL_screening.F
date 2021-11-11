!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine PHEL_screening(E,q,DONE)
 !
 use pars,           ONLY:SP,cZERO,schlen,pi,cI
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,k_the_nearest,nqbz,bare_qpg
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_mat,X_t,X_alloc
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:DUMP,NONE,REP,RD_CL_IF_END,manage_action,OP_RD,VERIFY,OP_RD_CL
 use LIVE_t,         ONLY:live_timing
 use ELPH,           ONLY:PH_qpt
 use PHEL,           ONLY:PH_Q
 use parallel_int,   ONLY:PARALLEL_index,PP_redux_wait
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_indexes,PP_indexes_reset,myid
 use vec_operate,    ONLY:iku_v_norm
 use stderr,         ONLY:intc,real2ch
 use OUTPUT_simple,  ONLY:OUTPUT_add_column
 use linear_algebra, ONLY:MAT_MUL
 use interfaces,     ONLY:LINEAR_ALGEBRA_driver
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: q
 logical                  :: DONE
 !
 ! Work Space
 !
 !... map
 integer      ::Q_local_map(PH_Q(2)),i_f
 real(SP)     ::q_dist(nqbz),min_q_modulus
 !
 !... functions
 integer , external ::io_X
 character(schlen)  ::Q_map_report
 !
 ! ...X's
 type(X_t)    ::X(2) ! ! 1:Xo 2:em1d 
 type(w_samp) ::Xw(2)
 integer      ::io_err(2),i_db,ID(2),iq,iq_ph,IO_ACT
 complex(SP), allocatable :: EM1D(:,:,:),Xo(:,:,:),WK(:,:),WK_ss_L(:,:),WK_ss_R(:,:),ss_WK_ss(:,:)
 !
 ! ...Output and naming
 integer           ::ig1,ig2,iw
 real(SP)          ::data(5)
 character(schlen) ::fname,desc(5)
 character(2)      ::prefix
 type(PP_indexes)  ::px
 complex(SP), allocatable :: F_test(:,:,:)
 !
 call section("=","Static-Screening test&correct")
 !=================================================
 !
 ! Correspondance PH_q <-> q%
 !----------------------------
 call k_ibz2bz(q,'i',.false.)
 min_q_modulus=10000.
 do iq=2,q%nbz
   if (iku_v_norm(q%ptbz(iq,:))<min_q_modulus) min_q_modulus=iku_v_norm(q%ptbz(iq,:))
 enddo
 do iq=PH_Q(1),PH_Q(2)
   Q_local_map(iq)=k_the_nearest(PH_qpt(iq,:),q%ptbz,q%nbz,i_err=io_err(1),k_dist=q_dist)
   Q_map_report='PH_Q ['//trim(intc(iq))//']: '//trim(intc(q%sstar(Q_local_map(iq),1)))//' (IBZ) '//&
&               trim(real2ch(q_dist(Q_local_map(iq))/min_q_modulus))//' (distance/minimum Q modulus)'
   call msg('r',trim(Q_map_report))
 enddo
 call k_ibz2bz(q,'d',.false.)
 !
 ! DB's check 
 !-------------
 X(:)%whoami=1
 X(1)%KERNEL="IP"
 do i_db=1,2
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID(i_db))
   io_err(i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
 enddo
 !
 if (any(io_err<0)) then
   call warning("X and/or Xo databases not found. Static screening validation procedure skipped.")
   DONE=.FALSE.
   return
 endif
 !
 ! Alloc
 !-------
 call X_alloc('X',(/X(1)%ng,X(1)%ng,Xw(1)%n_freqs/))
 YAMBO_ALLOC(EM1D,(X(1)%ng,X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(Xo,(X(1)%ng,X(1)%ng,Xw(1)%n_freqs))
 YAMBO_ALLOC(WK,(X(1)%ng,X(1)%ng))
 YAMBO_ALLOC(WK_ss_L,(X(1)%ng,X(1)%ng))
 YAMBO_ALLOC(WK_ss_R,(X(1)%ng,X(1)%ng))
 YAMBO_ALLOC(ss_WK_ss,(X(1)%ng,X(1)%ng))
 YAMBO_ALLOC(F_test,(PH_Q(1):PH_Q(2),Xw(1)%n_freqs,5))
 F_test=cZERO
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/X(1)%ng,X(1)%ng/))
 call live_timing('Screening Validation Function',px%n_of_elements(myid+1)*Xw(1)%n_freqs*(PH_Q(2)-PH_Q(1)+1))
 !
 do iq_ph=PH_Q(1),PH_Q(2)
   !
   iq=iq_ph!q%sstar(Q_local_map(iq_Ph),1)
   !
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,PH_Q(2))
   do i_db=1,2
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/2*iq,2*iq+1/),MODE=VERIFY,ID=ID(i_db))
     io_err(i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
     if (io_err(i_db)<0) call error(trim(intc(iq))//" Section of database missing")
     if (i_db==1) Xo(:,:,:)=X_mat
     if (i_db==2) then
       do ig1=1,X(1)%ng
         EM1D(ig1,:,:)=4.*pi/bare_qpg(iq,ig1)**2*X_mat(ig1,:,:)
         EM1D(ig1,ig1,:)=EM1D(ig1,ig1,:)+1.
       enddo
     endif
   enddo
   !
   do iw=1,Xw(1)%n_freqs
     !
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=Xo(:,:,iw),B=EM1D(:,:,iw),C=WK)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=Xo(:,:,iw),B=EM1D(:,:,1) ,C=WK_ss_R)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=EM1D(:,:,1),B=Xo(:,:,iw),C=WK_ss_L)
     call LINEAR_ALGEBRA_driver(MAT_MUL,M=EM1D(:,:,1),B=WK_ss_R,C=ss_WK_ss)
     !
     do ig1=1,X(1)%ng
       do ig2=1,X(1)%ng
         if (.not.px%element_2D(ig1,ig2)) cycle
         F_test(iq_ph,iw,1)= F_test(iq_ph,iw,1)+WK(ig1,ig2)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)*4.*pi
         F_test(iq_ph,iw,2)= F_test(iq_ph,iw,2)+(WK_ss_L(ig1,ig2)+WK_ss_R(ig1,ig2))/2./bare_qpg(iq,ig1)/bare_qpg(iq,ig2)*4.*pi
         F_test(iq_ph,iw,3)= F_test(iq_ph,iw,3)+ss_WK_ss(ig1,ig2)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)*4.*pi
         call live_timing(steps=1)
       enddo
     enddo 
     F_test(iq_ph,iw,4)= real(F_test(iq_ph,iw,1))/real(F_test(iq_ph,iw,2))+&
&                     cI*aimag(F_test(iq_ph,iw,1))/aimag(F_test(iq_ph,iw,2))
     F_test(iq_ph,iw,5)= real(F_test(iq_ph,iw,1))/real(F_test(iq_ph,iw,3))+&
&                     cI*aimag(F_test(iq_ph,iw,1))/aimag(F_test(iq_ph,iw,3))
   enddo
   !
 enddo 
 !
 call live_timing( )
 !
 call PP_redux_wait(F_test)
 !
 ! Output
 !
 call live_timing('Output',Xw(1)%n_freqs*(PH_Q(2)-PH_Q(1)+1)*2)
 !
 do iq_ph=PH_Q(1),PH_Q(2)
   do i_f=1,2
     !
     if (i_f==1) prefix="RE"
     if (i_f==2) prefix="IM"
     !
     fname="static_screening-"//prefix//"_q"//trim(intc(iq_ph))
     call OUTPUT_add_column(trim(fname),action="open")
     !
     do iw=1,Xw(1)%n_freqs
       call OUTPUT_add_column(trim(fname),TITLES=(/"Energy"/),R_VALUES=(/real(Xw(1)%p(iw))/),UNIT="eV")
       if (i_f==1) data=real(F_test(iq_ph,iw,:))
       if (i_f==2) data=aimag(F_test(iq_ph,iw,:))
       desc(1)=prefix//"[X]"
       desc(2)=prefix//"[Xo.SS]"
       desc(3)=prefix//"[SS.Xo.SS]"
       desc(4)=prefix//"[Xo.SS]/"//prefix//"[X]"
       desc(5)=prefix//"[SS.Xo.SS]/"//prefix//"[X]"
       call OUTPUT_add_column(trim(fname),TITLES=desc,R_VALUES=data)
       call OUTPUT_add_column(trim(fname),action="write")
       call live_timing( steps=1 )
     enddo
     call OUTPUT_add_column(trim(fname),action="close")
     !
   enddo
   !
 enddo
 !
 call live_timing( )
 !
 ! Free
 call PP_indexes_reset(px)
 call X_alloc('X')
 YAMBO_FREE(EM1D)
 YAMBO_FREE(Xo)
 YAMBO_FREE(WK)
 YAMBO_FREE(WK_ss_L)
 YAMBO_FREE(WK_ss_R)
 YAMBO_FREE(ss_WK_ss)
 !
 DONE=.TRUE.
 !
end subroutine PHEL_screening
