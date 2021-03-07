!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG
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
subroutine OEP_ApplySigmaCSX(X,Xw,E,k,q,U_csx)
 !
 !  Apply the NL local sX+cH operator to the valence wfs.
 !  
 !  OUTPUT: U_csx^{nk}(r) = \int dr' \Sigma_csx(r',r) \phi_{nk} 
 !
 !  Found as U_csx^{nk}(r) = U_sx^{nk}(r) +  U_ch^{nk}(r) 
 !
 !  U_sx^{nk}(r) = \sum_q \sum_m \phi_{m,k-q}(r)*u_rs_{n,m}(k,q,r)
 !
 !  where u_rs_{n,m}(k,q,r) = FT[\sum_G' (rho_{m,n}(k,q,G'))^*/|q+G||q+G'|\epsilon_{GG'}(q)] 
 !
 !  U_ch^{nk}(r)  = \sum_q \phi_{n,k}(r)*u_ch(q,r)
 !
 !  where u_ch(q,r) = FT[\sum_G' \epsilon_{GG'}(q)]/|q+G||q+G'|]
 !
 !      NO SPIN/PARALLEL EXECUTION NOT TESTED  
 ! 
 use pars,            ONLY:SP,DP,pi,schlen
 use D_lattice,       ONLY:i_time_rev,i_space_inv,sop_inv,nsym
 use electrons,       ONLY:levels,n_met_bands,spin_occ
 use R_lattice,       ONLY:bz_samp,g_rot,qindx_S,nkibz,G_m_G
 use frequency,       ONLY:w_samp
 use FFT_m,           ONLY:fft_size,fft_g_table,fft_dim,fftw_plan,fft_rot_r
 use collision_el,    ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc,elemental_collision_free
 use parallel_m,      ONLY:PP_indexes,myid,PP_indexes_reset
 use parallel_int,    ONLY:PP_redux_wait,PARALLEL_index 
 use wave_func,       ONLY:WF
 use LIVE_t,          ONLY:live_timing
 use stderr,          ONLY:intc
 use SC,              ONLY:it_now
 use IO_m,            ONLY:io_control,manage_action,OP_RD_CL,REP,VERIFY,NONE,OP_IF_START_RD_CL_IF_END
 use X_m,             ONLY:X_alloc,X_mat,X_t
 !
 ! I/O
 !
#include<memory.h>
 complex(SP),intent(inout)  :: U_csx(nkibz,n_met_bands,fft_size)
 type(levels), intent(in)   :: E
 type(bz_samp), intent(in)  :: k,q
 type(X_t),intent(in)       :: X  
 type(w_samp),intent(in)    :: Xw
 !
 ! Work space
 !
 type(elemental_collision) ::isc
 type(PP_indexes)          ::px
 integer          ::iv1,iv2,i1,i2,ik,iq,ifft,offt,os,is,iGo
 integer          ::io_err,ID,IO_ACT,iqs,iqibz
 character(schlen)::ch
 logical          ::lostr, PPaR_is_TR_rotated     
 complex(DP),allocatable  ::u(:),rhotw_rs(:),u_rs(:),local_u(:),Vcoh(:),Vcoh_rs(:)
 complex(SP), allocatable ::PPaR(:,:),PPaR_ws(:,:)
 integer, external::io_X
 !
 ! Resets
 !
 call elemental_collision_free(isc)
 call PP_indexes_reset(px)
 !
 isc%ngrho=maxval(G_m_G)
 isc%iqref=0
 !
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=io_X(X,Xw,ID)
 if (io_err<0) call error('Incomplete and/or broken static diel. fun. database')
 !
 call X_alloc('X',(/X%ng,X%ng,1/))
 YAMBO_ALLOC(PPaR_ws,(X%ng,X%ng))
 YAMBO_ALLOC(PPaR,(X%ng,X%ng))
 !
 call elemental_collision_alloc(isc,TITLE="OEP_SigmaCSX",NG=X%ng,NG_GAMP=(/X%ng,X%ng/))
 YAMBO_ALLOC(local_u,(fft_size))
 YAMBO_ALLOC(u,(X%ng))
 YAMBO_ALLOC(u_rs,(fft_size))
 YAMBO_ALLOC(rhotw_rs,(fft_size))
 YAMBO_ALLOC(Vcoh,(X%ng))
 !
 isc%qs=1
 !call scatterGamp(isc,'c') !???
 Vcoh =(0._DP,0._DP)
 call PARALLEL_index(px,(/q%nbz,E%nbm/))
 call PP_redux_wait
 ch="Sigma_csX|WF>@it"//trim(intc(it_now))
 call live_timing(trim(ch),px%n_of_elements(myid+1)*E%nbm*k%nibz)
 do iq=1,q%nbz
   isc%qs(2:)=(/q%sstar(iq,1),q%sstar(iq,2)/)
   iqibz=isc%qs(2)
   iqs  =isc%qs(3)
   if (iqibz/=isc%iqref) then
     call scatter_Gamp(isc,'c')
     !
     IO_ACT=manage_action(OP_IF_START_RD_CL_IF_END,iq,1,q%nbz)
     call io_control(ACTION=IO_ACT,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)
     io_err=io_X(X,Xw,ID)
     !
     forall(i1=1:X%ng,i2=1:X%ng) PPaR(i1,i2)=X_mat(i1,i2,1)*isc%gamp(i1,i2) ! Poles and Residuals
     !
     PPaR_is_TR_rotated=.false.
     !
   end if
   !
   if (iqs>nsym/(i_time_rev+1) .and. i_space_inv == 0 .and..not.PPaR_is_TR_rotated) then
     PPaR_is_TR_rotated=.true.
     forall(i1=1:X%ng,i2=1:X%ng) PPaR_ws(i2,i1)=PPaR(i1,i2)
     PPaR(:,:)=PPaR_ws(:,:)
   endif
   !
   ! COH potential in Reciprocal Space 
   !
!   do ig1 = 1,X%ng
!     do ig2 = 1,X%ng
!       if (G_m_G(ig1,ig2)==0) cycle
!       Vcoh(ig1) = Vcoh(ig1)+2._SP*pi*PPaR(ig2,G_m_G(ig1,ig2))
!     end do
!   end do
   !
   ! SX applied to the WF
   !
   do iv1 = 1 ,E%nbm
     if (.not.px%element_2D(iq,iv1)) cycle
     do ik = 1,k%nibz
       is = k%sstar(ik,2)
       do iv2 = 1 ,E%nbm
         u =(0._DP,0._DP)
         u_rs =(0._DP,0._DP) 
         local_u =(0._DP,0._DP)
         rhotw_rs =(0._DP,0._DP)  
         isc%is=(/iv1,ik,1,1/)
         isc%os=(/iv2,k%sstar(qindx_S(ik,iq,1),:),1/)
         isc%qs(1)=qindx_S(ik,iq,2)
         os=isc%os(3)
         lostr = os>nsym/(i_time_rev+1)
         call scatter_Bamp(isc)
#if defined _DOUBLE
         call zgemv('N', X%ng, X%ng, (1._SP,0._SP), PPaR, X%ng, conjg(isc%rhotw),1,(0._SP,0_SP), u,1)
#else 
         call cgemv('N', X%ng, X%ng, (1._SP,0._SP), PPaR, X%ng, conjg(isc%rhotw), 1, (0._SP,0_SP), u,1)
#endif
         iGo=g_rot(isc%qs(1),sop_inv(iqs))
         u_rs(fft_g_table(1:X%ng,iGo))=4._SP*pi*u(1:X%ng)
#if defined _FFTW
         fftw_plan=0
         call fft_3d(u_rs,fft_dim,1,fftw_plan)
#else
         call fft_3d(u_rs,fft_dim,1)
#endif      
         ifft=WF%index(iv1,ik,1)
         offt=WF%index(iv2,isc%os(2),1)
         rhotw_rs(:)=E%f(iv2,isc%os(2),1)/spin_occ
         if (lostr)      rhotw_rs(1:fft_size)=rhotw_rs(:)*conjg(WF%c(fft_rot_r(1:fft_size,os),1,offt))
         if (.not.lostr) rhotw_rs(1:fft_size)=rhotw_rs(:)*WF%c(fft_rot_r(1:fft_size,os),1,offt)
         local_u(:) =  u_rs(fft_rot_r(:,iqs))*rhotw_rs(:)
         U_csx(ik,iv1,:) = U_csx(ik,iv1,:)-local_u(:)
          call live_timing(steps=1)
         !
       end do
     end do
   end do
 end do
 YAMBO_FREE(local_u)
 YAMBO_FREE(u)
 YAMBO_FREE(u_rs)
 YAMBO_FREE(rhotw_rs)
 !
 ! COH applied to the WF: FFT,then multiply by WFs and add to SX
 !
 YAMBO_ALLOC(Vcoh_rs,(fft_size))
!  Vcoh_rs(fft_g_table(1:,1))=Vcoh(1:) 
! #if defined _FFTW
!  fftw_plan=0
!  call fft_3d(Vcoh_rs,fft_dim,1,fftw_plan)
! #else
!  call fft_3d(Vcoh_rs,fft_dim,1)
! #endif       
!  do iv1 = 1 ,E%nbm
!    do ik = 1,k%nibz
!      ifft=wf_state(iv1,ik,1)
!      U_csx(ik,iv1,:) = U_csx(ik,iv1,:)+Vcoh_rs(:)*wf(:,ifft)     
!    end do
!  end do
 !
 call live_timing()
 YAMBO_FREE(Vcoh)
 YAMBO_FREE(Vcoh_rs)
 call elemental_collision_free(isc)
 call PP_indexes_reset(px)
 !
end subroutine OEP_ApplySigmaCSX
