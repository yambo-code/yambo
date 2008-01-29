!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
subroutine tddft_BSK_2_FXC(iq,ik1,ik2,O1x,O2x,O_x_dim,W,X_static)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:spin_occ,spin
 use X_m,           ONLY:X_rhoq0,X_t
 use BS,            ONLY:BS_blk_dim,BS_eh_table,BS_mat,BS_eh_E
 use TDDFT,         ONLY:FXC_n_g_corr,FXC_K_diagonal,FXC,&
&                        FXC_is_causal,ioBS_Fxc
 use par_proc_m,    ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m, ONLY:par_indexes
 use D_lattice,     ONLY:DL_vol
 use R_lattice,     ONLY:bare_qpg,nXkbz
 use IO_m,          ONLY:io_control,OP_RD_CL,OP_RD,RD_CL,&
&                        NONE,OP_WR_CL,REP,VERIFY
 use frequency,     ONLY:w_samp
 implicit none
 type(w_samp) ::W
 integer      ::ik1,ik2,O_x_dim,iq
 complex(SP)  ::O1x(FXC_n_g_corr,O_x_dim),O2x(FXC_n_g_corr,O_x_dim)
 type(X_t)    ::X_static
 !
 ! WorkSpace 
 !
 integer      :: blk_pos(2),ik,ic,iv,i1,i2,iw,iw_blk,i_sp
 complex(SP)  :: Co
 complex(SP), allocatable :: wk1(:,:),wk2(:,:),FXC_temp(:,:)
 !
 ! Local Oscillators & Residuals
 !
 complex(SP), allocatable :: conjg_O1x_qpg(:,:),O2x_qpg(:,:)
 complex(SP), allocatable :: left_R(:,:),right_R(:,:)
 !
 ! Local Energies 
 !
 real(SP),    allocatable :: blk_Eo(:,:)
 real(SP),      parameter :: E_same_treshold=1.E-5
 !
 ! Fxc procedure
 !
 type(pp_indexes) :: p1,p2
 !
 ! I/O
 !
 integer           ::ioFxc_err,ID,COM_MODE
 !
 ! Constants and coordinates
 !
 blk_pos=(/sum(BS_blk_dim(:ik1-1)),sum(BS_blk_dim(:ik2-1))/)
 Co=spin_occ/(DL_vol*real(nXkbz))
 !
 ! [IO] READ PARS & FXC_K_diagonal 
 !
 call pp_redux_wait()
 call io_control(ACTION=OP_RD,COM=NONE,MODE=VERIFY,SEC=(/1,2/),ID=ID)
 ioFxc_err=ioBS_Fxc(iq,W,ID,X=X_static)
 !
 ! Exchange Oscillators Renormalization
 !
 if (iq==1) then
   do i1=blk_pos(1)+1,blk_pos(1)+BS_blk_dim(ik1)
     ik=BS_eh_table(i1,1)
     iv=BS_eh_table(i1,2)
     ic=BS_eh_table(i1,3)
     i_sp=spin(BS_eh_table(i1,:))
     O1x(1,i1-blk_pos(1))=-conjg(X_rhoq0(ic,iv,ik,i_sp))/bare_qpg(iq,1)
   enddo
   if (ik1/=ik2) then
     do i1=blk_pos(2)+1,blk_pos(2)+BS_blk_dim(ik2)
       ik=BS_eh_table(i1,1)
       iv=BS_eh_table(i1,2)
       ic=BS_eh_table(i1,3)
       i_sp=spin(BS_eh_table(i1,:))
       O2x(1,i1-blk_pos(2))=-conjg(X_rhoq0(ic,iv,ik,i_sp))/bare_qpg(iq,1)
     enddo
   else
     O2x=O1x
   endif
 endif
 !
 allocate(conjg_O1x_qpg(FXC_n_g_corr,BS_blk_dim(ik1)))
 allocate(O2x_qpg(BS_blk_dim(ik2),FXC_n_g_corr))
 allocate(blk_Eo(2,O_x_dim))
 !
 forall(i1=1:FXC_n_g_corr) conjg_O1x_qpg(i1,:)=conjg(O1x(i1,:))*bare_qpg(iq,i1)
 forall(i2=1:FXC_n_g_corr) O2x_qpg(:,i2)=O2x(i2,:)*bare_qpg(iq,i2)
 !
 ! Kernel diagonal and simmetrization
 !
 if (ik1==ik2) then
   forall(i1=1:BS_blk_dim(ik1)) FXC_K_diagonal(blk_pos(1)+i1)=real(BS_mat(i1,i1))
   forall(i1=1:BS_blk_dim(ik1)) BS_mat(i1,i1)=(0.,0.)
   do i1=1,BS_blk_dim(ik1)
     do i2=i1+1,BS_blk_dim(ik1)
       BS_mat(i2,i1)=conjg(BS_mat(i1,i2))
     enddo
   enddo
 endif
 forall(i1=1:BS_blk_dim(ik1)) blk_Eo(1,i1)=BS_eh_E(blk_pos(1)+i1)+&
&                                          FXC_K_diagonal(blk_pos(1)+i1)
 forall(i2=1:BS_blk_dim(ik2)) blk_Eo(2,i2)=BS_eh_E(blk_pos(2)+i2)+&
&                                          FXC_K_diagonal(blk_pos(2)+i2)
 !
 ! Projectors
 !
 forall(i1=1:BS_blk_dim(ik1),i2=1:BS_blk_dim(ik2)) &
&      BS_mat(i1,i2)=BS_mat(i1,i2)*Fb2x1(blk_Eo(1,i1),blk_Eo(2,i2),E_same_treshold)
 !
 ! Degenerations:
 !
 ! In contrast to the original implementation here degenerations are
 ! artifically removed shifting of E_same_treshold one of the two
 ! degenerate energies.
 ! As Fb2x1 mulitplies by E_same_treshold  in the limit of small 
 ! E_same_treshold the result is independent on E_same_treshold.
 !
 do i1=1,BS_blk_dim(ik1)
   do i2=1,BS_blk_dim(ik2)
     blk_Eo(1,i1)=blk_Eo(1,i1)+2.*Fb2x2(blk_Eo(1,i1),blk_Eo(2,i2),&
&                                       E_same_treshold)*E_same_treshold
   enddo
 enddo
 !
 ! Residuals
 !
 allocate(left_R(FXC_n_g_corr,BS_blk_dim(ik2)))
 allocate(right_R(BS_blk_dim(ik1),FXC_n_g_corr))
 left_R=(0._SP,0._SP)
 right_R=(0._SP,0._SP)
 !
 call pp_indexes_reset(p1)
 call pp_indexes_reset(p2)
 !
 call par_indexes(p1,(/BS_blk_dim(ik1)/))
 do i1=1,BS_blk_dim(ik1)
   if (.not.p1%i1p(i1)) cycle
   right_R(i1,:)=matmul(BS_mat(i1,:),O2x_qpg(:,:))
 enddo
 call pp_redux_wait(right_R)
 !
 call par_indexes(p2,(/BS_blk_dim(ik2)/))
 do i2=1,BS_blk_dim(ik2)
   if (.not.p2%i1p(i2)) cycle
   left_R(:,i2)=-matmul(conjg_O1x_qpg(:,:),BS_mat(:,i2))
 enddo
 call pp_redux_wait(left_R)
 !
 call pp_indexes_reset(p1)
 call pp_indexes_reset(p2)
 !
 ! Fxc
 !
 allocate(wk1(BS_blk_dim(ik1),FXC_n_g_corr),wk2(FXC_n_g_corr,BS_blk_dim(ik2)),&
&         FXC_temp(FXC_n_g_corr,FXC_n_g_corr))
 !
 COM_MODE=NONE
 if (ik1==1.and.ik2==1) COM_MODE=REP
 !
 call pp_indexes_reset(p1)
 call par_indexes(p1,(/W%n(2)/))
 !
 do iw_blk=1,W%n(1),W%n(2)
   !
   ! [IO] READ FXC % FXC_K_diagonal for this block of frequencies
   !
   if (iw_blk==1) then
     if (ioFxc_err==0) then
       call io_control(ACTION=RD_CL,COM=NONE,SEC=(/iw_blk+2/),ID=ID)
       ioFxc_err=ioBS_Fxc(iq,W,ID,X=X_static)
     endif
   else
     call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/iw_blk+2/),ID=ID)
     ioFxc_err=ioBS_Fxc(iq,W,ID,X=X_static)
   endif
   !
   if (ioFxc_err/=0) FXC=(0._SP,0._SP)
   !
   do iw=iw_blk,iw_blk+W%n(2)-1
     !
     if (.not.p1%i1p(iw-iw_blk+1)) then
       FXC(:,:,iw-iw_blk+1)=(0._SP,0._SP)
       cycle
     endif
     !
     ! Upper diagonal block contribution
     !
     if (FXC_is_causal) then 
       forall(i1=1:BS_blk_dim(ik1)) wk1(i1,:)=right_R(i1,:)*(&
&            1._SP/(W%p(iw)-blk_Eo(1,i1))+1._SP/(-W%p(iw)-blk_Eo(1,i1)) )
       forall(i2=1:BS_blk_dim(ik2)) wk2(:,i2)= left_R(:,i2)*(&
&            1._SP/(W%p(iw)-blk_Eo(2,i2))+1._SP/(-W%p(iw)-blk_Eo(2,i2)) )
     else
       forall(i1=1:BS_blk_dim(ik1)) wk1(i1,:)=right_R(i1,:)/(W%p(iw)-blk_Eo(1,i1))
       forall(i2=1:BS_blk_dim(ik2)) wk2(:,i2)= left_R(:,i2)/(W%p(iw)-blk_Eo(2,i2))
     endif
     !
     FXC(:,:,iw-iw_blk+1)=FXC(:,:,iw-iw_blk+1)+&
&                         Co*(matmul(conjg_O1x_qpg,wk1)+matmul(wk2,O2x_qpg))
     !
     ! Lower diagonal block contribution
     !
     if (ik1/=ik2) then
       if (FXC_is_causal) then 
         forall(i1=1:BS_blk_dim(ik1)) wk1(i1,:)=right_R(i1,:)*(&
&            1._SP/(-conjg(W%p(iw))-blk_Eo(1,i1))+1._SP/(conjg(W%p(iw))-blk_Eo(1,i1)) )
         forall(i2=1:BS_blk_dim(ik2)) wk2(:,i2)= left_R(:,i2)*(&
&            1._SP/(-conjg(W%p(iw))-blk_Eo(2,i2))+1._SP/(conjg(W%p(iw))-blk_Eo(2,i2)) )
       else
         forall(i1=1:BS_blk_dim(ik1)) wk1(i1,:)=right_R(i1,:)/(conjg(W%p(iw))-blk_Eo(1,i1))
         forall(i2=1:BS_blk_dim(ik2)) wk2(:,i2)= left_R(:,i2)/(conjg(W%p(iw))-blk_Eo(2,i2))
       endif
       !
       FXC_temp=Co*(matmul(conjg_O1x_qpg,wk1)+matmul(wk2,O2x_qpg))
       !
       forall(i1=1:FXC_n_g_corr,i2=1:FXC_n_g_corr) &
&            FXC(i1,i2,iw-iw_blk+1)=FXC(i1,i2,iw-iw_blk+1)+conjg(FXC_temp(i2,i1))
     endif
     !
   enddo
   !
   do iw=1,W%n(2)
     call pp_redux_wait(FXC(:,:,iw))
   enddo
   !
   ! [IO] WRITE FXC & FXC_K_diagonal for this block of frequencies
   !
   if (iw_blk==1) call io_control(ACTION=OP_WR_CL,COM=COM_MODE,SEC=(/1,2,iw_blk+2/),ID=ID)
   if (iw_blk> 1) call io_control(ACTION=OP_WR_CL,COM=COM_MODE,SEC=(/iw_blk+2/),ID=ID)
   ioFxc_err=ioBS_Fxc(iq,W,ID,X=X_static)
   !
 enddo
 !
 call pp_indexes_reset(p1)
 !
 ! CLEAN
 !
 deallocate(conjg_O1x_qpg,O2x_qpg,blk_Eo,left_R,right_R,wk1,wk2,FXC_temp)
 ! 
 contains
   !
   pure function Fb2x1(E1,E2,T)
     implicit none
     real(SP), intent(in):: E1,E2,T
     real(SP)            :: Fb2x1
     if (abs(E1-E2)<T) Fb2x1=1./2./T
     if (abs(E1-E2)>T) Fb2x1=1./(E1-E2)
   end function
   !
   pure function Fb2x2(E1,E2,T)
     implicit none
     real(SP), intent(in):: E1,E2,T
     real(SP)            :: Fb2x2
     Fb2x2=0.
     if (abs(E1-E2)<T) Fb2x2=1.
   end function
   !
end subroutine
