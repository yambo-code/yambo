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
subroutine PHEL_screening(E,q)
 !
 use pars,           ONLY:SP,cZERO,schlen
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,k_the_nearest,nqbz
 use frequency,      ONLY:w_samp
 use X_m,            ONLY:X_mat,X_t,X_alloc
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:DUMP,NONE,REP,RD_CL_IF_END,manage_action,OP_RD
 use LIVE_t,         ONLY:live_timing
 use ELPH,           ONLY:PH_qpt
 use PHEL,           ONLY:PH_Q
 use parallel_int,   ONLY:PARALLEL_index
 use com,            ONLY:msg
 use parallel_m,     ONLY:PP_indexes,PP_indexes_reset,myid
 use vec_operate,    ONLY:rlu_iku_v_norm
 use stderr,         ONLY:intc,real2ch
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 !... map
 integer      ::Q_local_map(PH_Q(2))
 real(SP)     ::q_dist(nqbz),min_q_modulus
 !
 !... functions
 integer , external ::io_X
 character(schlen)  ::Q_map_report
 !
 ! ...X's
 type(X_t)    ::X(2) ! ! 1:Xo 2:em1d 
 type(w_samp) ::Xw(2)
 integer      ::io_err(2),i_db,ID(2),iq,IO_ACT
 complex(SP), allocatable :: X_loc_mat(:,:,:,:)
 !
 ! ...Output and naming
 !integer      ::Xo_ID,EM1D_ID,io_err(2),iq,SEC(2),IO_ACT,ig1,ig2,iw,ig3
 complex(SP), allocatable :: F_test(:,:,:)
 type(PP_indexes)  ::px
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
 do i_db=1,2
   if (i_db==1) X(i_db)%whoami=1
   if (i_db==2) X(i_db)%whoami=3
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=ID(i_db))
   io_err(i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
 enddo
 !
 if (any(io_err<0)) return
 !
 ! Alloc
 !-------
 call X_alloc('X',(/X(1)%ng,X(1)%ng,Xw(1)%n_freqs/))
 YAMBO_ALLOC(X_loc_mat,(X(1)%ng,X(1)%ng,Xw(1)%n_freqs,2))
 YAMBO_ALLOC(F_test,(PH_Q(1):PH_Q(2),Xw(1)%n_freqs,2))
 F_test=cZERO
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/X(1)%ng**2/))
 call live_timing('Screening Test-Function',px%n_of_elements(myid+1))
 !
 do iq=PH_Q(1),PH_Q(2)
!   !
!   if (iq==1) bare_qpg(iq,1)=1._SP
!   !
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,PH_Q(2))
   do i_db=1,2
     if (i_db==1) X(i_db)%whoami=1
     if (i_db==2) X(i_db)%whoami=3
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iq,2*iq+1/),MODE=DUMP,ID=ID(i_db))
     io_err(i_db)=io_X(X(i_db),Xw(i_db),ID(i_db))
     X_loc_mat(:,:,:,i_db)=X_mat
   enddo
   write (*,*) iq,io_err
!   !
!   if (.not.any(px%element_2D(iq,:))) cycle
!   !
!   if (any(io_err/=0)) then
!     call live_timing(steps=Xow%n_freqs)
!     cycle
!   endif
!   !
!   do iw=1,Xow%n_freqs
!     if (.not.px%element_2D(iq,iw)) cycle 
!     call live_timing(steps=1)
!     do ig1=2,Xo%ng
!       do ig2=1,Xo%ng
!         do ig3=2,Xo%ng
!           F_test(iq,iw,1)= F_test(iq,iw,1)+Xo_mat(ig1,ig3,iw)*X_mat(ig3,ig2,iw)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
!           F_test(iq,iw,2)= F_test(iq,iw,2)+Xo_mat(ig1,ig3,iw)*X_mat(ig3,ig2,1)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
!         enddo
!       enddo
!     enddo 
!   enddo
!   !
 enddo 
 !
 ! Free
 call X_alloc('X')
 YAMBO_FREE(X_loc_mat)
 !
! !
! call PP_redux_wait(F_test)
! !
! call OUTPUT_add_column("phel_screening_diff",action="reset")
! call OUTPUT_add_column("phel_screening_diff",action="open")
! !
! do iq=1,nqibz
!   !
!   INT_SF(iq,1)=RIntegrate(aimag(F_test(iq,:,1)),real(Xow%p),Xow%n_freqs)
!   INT_SF(iq,2)=RIntegrate(aimag(F_test(iq,:,2)),real(Xow%p),Xow%n_freqs)
!   !
!   fname="phel_screening_q"//trim(intc(iq))
!   call OUTPUT_add_column(trim(fname),action="reset")
!   call OUTPUT_add_column(trim(fname),action="open")
!   do iw=1,Xow%n_freqs
!     !
!     call OUTPUT_add_column(trim(fname),TITLES=(/"Energy"/),R_VALUES=(/real(Xow%p(iw))/),UNIT="eV")
!     call OUTPUT_add_column(trim(fname),&
!&         TITLES=(/"Im[Xo x Em1D]","Re[Xo x Em1D]"/),R_VALUES=(/aimag(F_test(iq,iw,1)),real(F_test(iq,iw,1))/))
!     call OUTPUT_add_column(trim(fname),&
!&         TITLES=(/"Im[Xo x Em1S]","Re[Xo x Em1S]"/),R_VALUES=(/aimag(F_test(iq,iw,2)),real(F_test(iq,iw,2))/))
!     call OUTPUT_add_column(trim(fname),action="write")
!   enddo
!   call OUTPUT_add_column(trim(fname),action="close")
!   !
!   call OUTPUT_add_column("phel_screening_diff",TITLES=(/"Q-point"/),I_VALUES=(/iq/))
!   call OUTPUT_add_column("phel_screening_diff",TITLES=(/"dInt[%]     ","Int(Im[DYN])"/),&
!&                         R_VALUES=(/(INT_SF(iq,1)-INT_SF(iq,2))/INT_SF(iq,1)*100.,INT_SF(iq,1)/))
!   call OUTPUT_add_column("phel_screening_diff",action="write")
!   !
! enddo
! call OUTPUT_add_column("phel_screening_diff",action="close")
 !
 call live_timing( )
 !
end subroutine PHEL_screening
