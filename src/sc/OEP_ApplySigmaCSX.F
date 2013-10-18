!
! Copyright (C) 2000-2013 M. Gruening and the YAMBO team
!              http://www.yambo-code.org
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
 use QP_m,            ONLY:QP_ng_Sx
 use collision,       ONLY:ggwinfo,collision_reset 
 use memory_m,        ONLY:mem_est
 use parallel_m,      ONLY:PP_redux_wait,PP_indexes,myid,PP_indexes_reset
 use interfaces,      ONLY:PARALLEL_index
 use wave_func,       ONLY:WF_load,WF
 use LIVE_t,          ONLY:live_timing
 use stderr,          ONLY:intc
 use SC,              ONLY:it_now
 use IO_m,            ONLY:io_control,OP_RD_CL,REP,VERIFY,NONE,RD_CL,OP_RD,RD_CL_IF_END,OP_WR_CL,IO_and_Messaging_switch
 use X_m,             ONLY:X_alloc,X_mat,X_t
 use com,             ONLY:error
 !
 ! I/O
 !
 implicit none
 complex(SP),intent(inout)  :: U_csx(nkibz,n_met_bands,fft_size)
 type(levels), intent(in)   :: E
 type(bz_samp), intent(in)  :: k,q
 type(X_t),intent(in)       :: X  
 type(w_samp),intent(in)    :: Xw
 !
 ! Work space
 !
 type(ggwinfo)    ::isc
 type(PP_indexes) ::px
 integer          ::iv1,iv2,ir,ig1,ig2,i1,i2,ikbz,ik,iq,ifft,offt,os,is,iGo
 integer          ::io_err,alloc_err,ID,iqs,iqibz
 character(schlen)::ch
 logical          ::lostr, listr, PPaR_is_TR_rotated     
 complex(DP),allocatable ::u(:),rhotw_rs(:),u_rs(:),local_u(:),Vcoh(:),Vcoh_rs(:)
 real(SP),    allocatable ::PPaP(:,:)
 complex(SP), allocatable ::PPaR(:,:),PPaR_ws(:,:)
 integer, external::ioX
 !
 ! Resets
 !
 call collision_reset(isc)
 call PP_indexes_reset(px)
 !
 isc%ngrho=maxval(G_m_G)
 isc%iqref=0
 !
 !
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_err=ioX(X,Xw,ID)
 if (io_err/=0) call error('Incomplete and/or broken static diel. fun. database')
 !
 call X_alloc('X',(/X%ng,X%ng,1/))
 allocate(PPaR_ws(X%ng,X%ng),PPaR(X%ng,X%ng),stat=alloc_err)
 call mem_est("PPaR",(/2*size(PPaR_ws)/),errors=(/alloc_err/))
 allocate(isc%gamp(X%ng,X%ng),isc%rhotw(isc%ngrho),local_u(fft_size),u(X%ng),&
      &u_rs(fft_size),rhotw_rs(fft_size),Vcoh(X%ng),stat=alloc_err)
 call mem_est("ISC-GAMP",(/X%ng**2+isc%ngrho/),errors=(/alloc_err/))
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
     call scatterGamp(isc,'c')
     !
     if (iq==1   ) call io_control(ACTION=OP_RD,COM=NONE,       SEC=(/1,2,2*iqibz+1/),ID=ID)    !
     if (q%nbz==1) call io_control(ACTION=OP_RD_CL,COM=NONE,    SEC=(/1,2,3/),ID=ID)            ! I/O 
     if (iq>1    ) call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/2*iqibz,2*iqibz+1/),ID=ID)!
     io_err=ioX(X,Xw,ID)
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
         call scatterBamp(isc)
#if defined _DOUBLE
         call zgemv('N', X%ng, X%ng, (1._SP,0._SP), PPaR, X%ng, conjg(isc%rhotw),1,(0._SP,0_SP), u,1)
#else 
         call cgemv('N', X%ng, X%ng, (1._SP,0._SP), PPaR, X%ng, conjg(isc%rhotw), 1, (0._SP,0_SP), u,1)
#endif
         iGo=g_rot(sop_inv(iqs),isc%qs(1))
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
         if (lostr)      rhotw_rs(1:fft_size)=rhotw_rs(:)*conjg(WF%c(fft_rot_r(os,1:fft_size),offt))
         if (.not.lostr) rhotw_rs(1:fft_size)=rhotw_rs(:)*WF%c(fft_rot_r(os,1:fft_size),offt)
         local_u(:) =  u_rs(fft_rot_r(iqs,:))*rhotw_rs(:)
         U_csx(ik,iv1,:) = U_csx(ik,iv1,:)-local_u(:)
          call live_timing(steps=1)
         !
       end do
     end do
   end do
 end do
 deallocate(isc%gamp,isc%rhotw,local_u,u,u_rs,rhotw_rs)
 !
 ! COH applied to the WF: FFT,then multiply by WFs and add to SX
 !
!  allocate(Vcoh_rs(fft_size))
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
! deallocate(Vcoh,Vcoh_rs)
 call collision_reset(isc)
 call PP_indexes_reset(px)
 !
end subroutine OEP_ApplySigmaCSX
