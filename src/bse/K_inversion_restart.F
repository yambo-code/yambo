!
! License-Identifier: GPL
!
! Copyright (C) 2016 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine K_inversion_restart(iq,W)
 !
 use pars,           ONLY:SP,cZERO
 use X_m,            ONLY:Resp_ii
 use BS_solvers,     ONLY:BSS_dr,BSS_er,BSS_n_freqs,K_Inversion_data,K_INV_EPS
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:NONE,OP_RD_CL,REP,VERIFY
 use parallel_m,     ONLY:PAR_COM_SLK_INDEX_local
 use com,            ONLY:msg
 use frequency,      ONLY:w_samp,W_reset
 use pars,           ONLY:rZERO
 use BS,             ONLY:l_BS_photolum
 use BS_solvers,     ONLY:K_INV_PL
 use PHOTOLUM,       ONLY:PL
 !
#include<memory.h>
 !
 integer,             intent(inout) :: iq
 type(w_samp)        ,intent(inout) :: W
 !
 ! Work Space
 !
 integer                  :: ID,iw,i_err,iw_tab
 integer,        external :: io_BSS_invert
 integer,     allocatable :: freq_table(:)
 type(w_samp)             :: W_db
 !
 ! Restart DB I/O
 !================
 !
 call W_reset(W_db)
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 i_err=io_BSS_invert(iq,W_db,ID)
 !
 ! When the damping range is not uniform then changing energy borders means changing damping :(
 !
 if (W_db%dr(1)/=W_db%dr(2).or.W%dr(1)/=W%dr(2)) then
   if (W%er(1)/=W_db%er(1).or.W%er(2)/=W_db%er(2)) i_err=-1
 endif
 !
 if (i_err==0) then
   !
   W_db%n_freqs=K_INV_EPS%n_freqs
   !
   YAMBO_ALLOC(freq_table,(W_db%n_freqs+W%n_freqs))
   !
   ! Here I create a new energy array that merges the db to the current energies. 
   !
   call FREQUENCIES_merge(W_db,W,freq_table)
   !
   BSS_n_freqs=W%n_freqs
   BSS_er=W%er
   BSS_dr=W%dr
   !
   call elemental_restart(K_INV_EPS)
   if(l_BS_photolum) call elemental_restart(K_INV_PL)
   !
   YAMBO_FREE(freq_table)
   !
 else
   !
   allocate(K_INV_EPS%err(W%n_freqs),K_INV_EPS%spectra(W%n_freqs,3))
   K_INV_EPS%err    =-1
   K_INV_EPS%spectra=cZERO
   K_INV_EPS%spectra(:,2:3)=Resp_ii(:,3:4)
   Resp_ii     =cZERO
   Resp_ii(:,1)=W%p(:)
   if(l_BS_photolum) then
     allocate(K_INV_PL%err(W%n_freqs),K_INV_PL%spectra(W%n_freqs,2))
     K_INV_PL%err    =-1
     K_INV_PL%spectra=cZERO
     K_INV_PL%spectra(:,2)=PL(:,3)
     PL     =rZERO
     PL(:,1)=W%p(:)
   endif
   !
 endif
 !
 allocate(K_INV_EPS%iw_full(W%n_freqs))
 K_INV_EPS%iw_full=0
 if(l_BS_photolum) then
   allocate(K_INV_PL%iw_full(W%n_freqs))
   K_INV_PL%iw_full=0
 endif
 !
 contains
   !
   subroutine elemental_restart(TYP)
     !
     type(K_Inversion_data) :: TYP
     !
     integer,     allocatable :: inv_err_db(:)
     complex(SP), allocatable :: DB_spectra(:,:)
     !
     allocate(TYP%err(W_db%n_freqs))
     allocate(inv_err_db(W_db%n_freqs))
     if (TYP%what=="PLS") then
       allocate(DB_spectra(W_db%n_freqs,2),TYP%spectra(W_db%n_freqs,2))
     else
       allocate(DB_spectra(W_db%n_freqs,3),TYP%spectra(W_db%n_freqs,3))
     endif
     !
     if (TYP%what=="EPS") call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,3/),ID=ID)
     if (TYP%what=="PLS") call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1,4/),ID=ID)
     i_err=io_BSS_invert(iq,W_db,ID)
     !
     ! Shadows and re-allocation
     !
     inv_err_db=TYP%err
     DB_spectra=TYP%spectra
     !
     deallocate(TYP%err,TYP%spectra)
     !
     allocate(TYP%err(W%n_freqs))
     if (TYP%what=="EPS") allocate(TYP%spectra(W%n_freqs,3))
     if (TYP%what=="PLS") allocate(TYP%spectra(W%n_freqs,2))
     TYP%spectra=cZERO
     TYP%err    =-1
     !
     do iw=1,W%n_freqs
       !
       ! Load eps/pl only for converged frequencies
       !
       iw_tab=freq_table(iw)
       !
       if (iw_tab>0) then
         TYP%spectra(iw,2)=DB_spectra(iw_tab,2)
         if (TYP%what=="EPS") TYP%spectra(iw,3)=DB_spectra(iw_tab,3)
         TYP%err(iw)=inv_err_db(iw_tab)
         if (TYP%err(iw)==0) then
           TYP%spectra(iw,1)=DB_spectra(iw_tab,1)
         endif
       else
         TYP%spectra(iw,1)=cZERO
         if (TYP%what=="EPS") then
           TYP%spectra(iw,2:3)=Resp_ii(iabs(iw_tab),3:4)
         endif
         if(l_BS_photolum) then
           if (TYP%what=="PLS") TYP%spectra(iw,2)=PL(iabs(iw_tab),3)
         endif
         TYP%err(iw)=-1
       endif
     enddo
     !
     call msg('rs','[BSE INV@'//TYP%what//'] Frequencies read         ',count(inv_err_db==0))
     call msg('rs','[BSE INV@'//TYP%what//']                          ',real(count(inv_err_db==0))/real(W%n_freqs)*100._SP,"[o/o]")
     call msg('rs','[BSE INV@'//TYP%what//'] Frequencies used         ',W%n_freqs)
     !
     ! De/Allocate Resp_ii/PL
     !
     if (TYP%what=="EPS") then
       YAMBO_FREE(Resp_ii)
       YAMBO_ALLOC(Resp_ii,(BSS_n_freqs,4))
       Resp_ii(:,1)=W%p(:)
       Resp_ii(:,2)=TYP%spectra(:,1)/real(PAR_COM_SLK_INDEX_local%n_CPU,SP)
       Resp_ii(:,3)=TYP%spectra(:,2)
       Resp_ii(:,4)=TYP%spectra(:,3)
     endif
     if(l_BS_photolum) then
       if (TYP%what=="PLS") then
         YAMBO_FREE(PL)
         YAMBO_ALLOC(PL,(BSS_n_freqs,3))
         PL(:,1)=W%p(:)
         PL(:,2)=TYP%spectra(:,1)/real(PAR_COM_SLK_INDEX_local%n_CPU,SP)
         PL(:,3)=TYP%spectra(:,2)
       endif
     endif
     !
     deallocate(inv_err_db,DB_spectra)
     !
  end subroutine
  !
end subroutine K_inversion_restart
