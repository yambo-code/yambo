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
 use pars,          ONLY:SP,cZERO,schlen
 use electrons,     ONLY:levels
 use R_lattice,     ONLY:bz_samp,nqibz,bare_qpg
 use frequency,     ONLY:w_samp
 use X_m,           ONLY:X_mat,X_t
 use IO_int,        ONLY:io_control
 use IO_m,          ONLY:DUMP,NONE,REP,OP_RD_CL,RD_CL_IF_END,manage_action,OP_RD
 use LIVE_t,        ONLY:live_timing
 use parallel_int,  ONLY:PP_redux_wait,PARALLEL_index
 use parallel_m,    ONLY:PP_indexes,PP_indexes_reset,myid
 use OUTPUT_simple, ONLY:OUTPUT_add_column
 use stderr,        ONLY:intc
 !
#include<memory.h>
 !
 type(levels) ,intent(in) :: E
 type(bz_samp),intent(in) :: q
 !
 ! Work Space
 !
 integer      ::Xo_ID,EM1D_ID,io_err(2),iq,SEC(2),IO_ACT,ig1,ig2,iw,ig3
 real(SP)     ::INT_SF(nqibz,2)
 type(w_samp) ::Xow,EM1Dw
 type(X_t)    ::Xo,EM1D
 complex(SP), allocatable :: Xo_mat(:,:,:)
 complex(SP), allocatable :: F_test(:,:,:)
 type(PP_indexes)  ::px
 character(schlen) ::fname
 !
 integer , external ::io_X
 real(SP), external ::RIntegrate
 !
 Xo%whoami=1
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=Xo_ID)
 io_err(1)=io_X(Xo,Xow,Xo_id)
 !
 YAMBO_ALLOC(Xo_mat,(Xo%ng,Xo%ng,Xow%n_freqs))
 YAMBO_ALLOC(F_test,(nqibz,Xow%n_freqs,2))
 F_test=cZERO
 !
 EM1D%whoami=3
 call io_control(ACTION=OP_RD,COM=REP,SEC=(/1,2/),MODE=DUMP,ID=EM1D_ID)
 io_err(2)=io_X(EM1D,EM1Dw,EM1D_id)
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/nqibz,Xow%n_freqs/))
 call live_timing('Screening test',px%n_of_elements(myid+1))
 !
 do iq=1,nqibz
   !
   if (iq==1) bare_qpg(iq,1)=1._SP
   !
   SEC=(/2*iq,2*iq+1/)
   IO_ACT=manage_action(RD_CL_IF_END,iq,1,nqibz)
   call io_control(ACTION=IO_ACT,COM=NONE,SEC=SEC,MODE=DUMP,ID=Xo_ID)
   io_err(1)=io_X(Xo,Xow,Xo_id)
   Xo_mat=X_mat
   call io_control(ACTION=IO_ACT,COM=NONE,SEC=SEC,MODE=DUMP,ID=EM1D_ID)
   io_err(2)=io_X(EM1D,EM1Dw,EM1D_id)
   !
   if (.not.any(px%element_2D(iq,:))) cycle
   !
   if (any(io_err/=0)) then
     call live_timing(steps=Xow%n_freqs)
     cycle
   endif
   !
   do iw=1,Xow%n_freqs
     if (.not.px%element_2D(iq,iw)) cycle 
     call live_timing(steps=1)
     do ig1=1,Xo%ng
       do ig2=1,Xo%ng
         do ig3=1,Xo%ng
           F_test(iq,iw,1)= F_test(iq,iw,1)+Xo_mat(ig1,ig3,iw)*X_mat(ig3,ig2,iw)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
           F_test(iq,iw,2)= F_test(iq,iw,2)+Xo_mat(ig1,ig3,iw)*X_mat(ig3,ig2,1)/bare_qpg(iq,ig1)/bare_qpg(iq,ig2)
         enddo
       enddo
     enddo 
   enddo
   !
 enddo 
 !
 call PP_redux_wait(F_test)
 !
 call OUTPUT_add_column("phel_screening_diff",action="reset")
 call OUTPUT_add_column("phel_screening_diff",action="open")
 !
 do iq=1,nqibz
   !
   INT_SF(iq,1)=RIntegrate(aimag(F_test(iq,:,1)),real(Xow%p),Xow%n_freqs)
   INT_SF(iq,2)=RIntegrate(aimag(F_test(iq,:,2)),real(Xow%p),Xow%n_freqs)
   !
   fname="phel_screening_q"//trim(intc(iq))
   call OUTPUT_add_column(trim(fname),action="reset")
   call OUTPUT_add_column(trim(fname),action="open")
   do iw=1,Xow%n_freqs
     !
     call OUTPUT_add_column(trim(fname),TITLES=(/"Energy"/),R_VALUES=(/real(Xow%p(iw))/),UNIT="eV")
     call OUTPUT_add_column(trim(fname),&
&         TITLES=(/"Im[Xo x Em1D]","Re[Xo x Em1D]"/),R_VALUES=(/aimag(F_test(iq,iw,1)),real(F_test(iq,iw,1))/))
     call OUTPUT_add_column(trim(fname),&
&         TITLES=(/"Im[Xo x Em1S]","Re[Xo x Em1S]"/),R_VALUES=(/aimag(F_test(iq,iw,2)),real(F_test(iq,iw,2))/))
     call OUTPUT_add_column(trim(fname),action="write")
   enddo
   call OUTPUT_add_column(trim(fname),action="close")
   !
   call OUTPUT_add_column("phel_screening_diff",TITLES=(/"Q-point"/),I_VALUES=(/iq/))
   call OUTPUT_add_column("phel_screening_diff",TITLES=(/"dInt[%]     ","Int(Im[DYN])"/),&
&                         R_VALUES=(/(INT_SF(iq,1)-INT_SF(iq,2))/INT_SF(iq,1)*100.,INT_SF(iq,1)/))
   call OUTPUT_add_column("phel_screening_diff",action="write")
   !
 enddo
 call OUTPUT_add_column("phel_screening_diff",action="close")
 !
 call live_timing( )
 !
 YAMBO_FREE(F_test)
 YAMBO_FREE(Xo_mat)
 !
end subroutine PHEL_screening
