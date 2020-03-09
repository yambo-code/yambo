!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine TDDFT_BSK_disk_2_FXC(iq,Xk,W,X_static)
 !
 use pars,          ONLY:SP,pi
 use frequency,     ONLY:w_samp
 use R_lattice,     ONLY:nqbz,bz_samp,nXkibz
 use BS,            ONLY:BS_res_K_exchange,BS_n_g_exch,&
&                        BS_bands,BS_nT_at_k,BS_K_dim
 use BS_solvers,    ONLY:BS_mat
 use matrix_operate,ONLY:UP_matrix_index
 use com,           ONLY:isec
 use LIVE_t,        ONLY:live_timing
 use parallel_m,    ONLY:PP_redux_wait
 use D_lattice,     ONLY:DL_vol
 use interfaces,    ONLY:WF_load,WF_free
 use wave_func,     ONLY:WF
 use X_m,           ONLY:X_t
 use IO_m,          ONLY:io_control,OP_RD,RD,NONE,RD_CL,VERIFY,OP_RD_CL,REP
 use TDDFT,         ONLY:FXC,FXC_n_g_corr,FXC_K_diagonal,io_BS_Fxc
#include<memory.h>
 type(bz_samp) ::Xk
 type(w_samp)  ::W
 integer       ::iq
 type(X_t)     ::X_static
 !
 ! WorkSpace 
 !
 integer    :: ik1,ik2,O_x_dim,icv1,icv2
 complex(SP):: Co,Hrx
#if defined _DOUBLE
 complex(SP):: zdotc
#else
 complex(SP):: cdotc
#endif
 complex(SP),allocatable::O1x(:,:),O2x(:,:)
 !
 ! I/O
 !
 integer           ::io_BS_err,ioFxc_err,ID,ACTION
 integer, external ::io_BS
 !
 if (isec(2)/=0) then
   call section('=','TDDFT `solver`')
 else if (isec(2)==0) then
   call section('+','TDDFT `solver`')
 endif
 !
 ! Fxc DB
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=VERIFY,SEC=(/1/),ID=ID)
 ioFxc_err=io_BS_Fxc(iq,W,ID,X=X_static)
 if (ioFxc_err==0) return
 !
 ! Constants & dimensions
 !
 Co=4.*pi/(DL_vol*real(nqbz))
 O_x_dim=maxval(BS_nT_at_k)
 !
 ! WF loading
 !
 call WF_load(WF,max(BS_n_g_exch,FXC_n_g_corr),1,BS_bands,(/1,nXkibz/),title='-BS/TDDFT')
 !
 ! Exchange oscillators
 !
 YAMBO_ALLOC(O1x,(max(BS_n_g_exch,FXC_n_g_corr),O_x_dim))
 YAMBO_ALLOC(O2x,(max(BS_n_g_exch,FXC_n_g_corr),O_x_dim))
 !
 ! Kernel 2 FXC
 !
 YAMBO_ALLOC(FXC_K_diagonal,(BS_K_dim))
 YAMBO_ALLOC(FXC,(FXC_n_g_corr,FXC_n_g_corr,W%n_freqs))
 O_x_dim=maxval(BS_nT_at_k)
 FXC_K_diagonal=0._SP
 FXC=(0._SP,0._SP)
 !
 ! Memory usage estimation  in BS K 2 Fxc procedure
 !
 !
 call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),ID=ID)
 io_BS_err=io_BS(iq,X_static,ID)
 !
 call live_timing('Fxc',(nXkibz**2-nXkibz)/2+nXkibz)
 do ik2=1,nXkibz
   !
   !call K_exchange(.true.,iq,ik2,Xk,O2x,O_x_dim)
   !
   do ik1=ik2,1,-1
     !
     !if (ik1/=ik2) call K_exchange(.true.,iq,ik1,Xk,O1x,O_x_dim)
     if (ik1==ik2) O1x=O2x
     !
     YAMBO_ALLOC(BS_mat,(BS_nT_at_k(ik1),BS_nT_at_k(ik2)))
     !
     ACTION=RD
     if (ik1==1.and.ik2==nXkibz) ACTION=RD_CL
     call io_control(ACTION=ACTION,COM=NONE,SEC=(/UP_matrix_index(ik1,ik2)/),ID=ID)
     io_BS_err=io_BS(iq,X_static,ID)
     !
     if (BS_res_K_exchange) then
       do icv1=1,BS_nT_at_k(ik1)
         do icv2=1,BS_nT_at_k(ik2)
           if (ik1==ik2.and.icv2<icv1) cycle
           !
#if defined _DOUBLE
           Hrx=zdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#else
           Hrx=cdotc(BS_n_g_exch,O2x(1,icv2),1,O1x(1,icv1),1)
#endif
           BS_mat(icv1,icv2)=BS_mat(icv1,icv2)-Hrx*2._SP*Co
           !
         enddo
       enddo
     endif
     !
     call TDDFT_BSK_2_FXC(iq,ik1,ik2,O1x(:FXC_n_g_corr,:),&
&                         O2x(:FXC_n_g_corr,:),O_x_dim,W,X_static)
     !
     call live_timing(steps=1)
     YAMBO_FREE(BS_mat)
   enddo
 enddo
 call live_timing
 !
 ! CLEAN
 !
 call PP_redux_wait
 YAMBO_FREE(O1x)
 YAMBO_FREE(O2x)
 call WF_free(WF)
 YAMBO_FREE(FXC_K_diagonal)
 YAMBO_FREE(FXC)
 !
end subroutine
