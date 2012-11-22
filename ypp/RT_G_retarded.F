! 
! Copyright (C) 2000-2012 A. Marini and the YAMBO team
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
subroutine RT_G_retarded(E,k)
 !
 use pars,           ONLY:SP,DP,pi,cI
 use units,          ONLY:FS2AUT,HA2EV
 use YPP,            ONLY:Gr_QP_width,GF_T_step,Gr_E_step
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window,Gr_E_window,Gr_E_steps
 use QP_CTL_m,       ONLY:QP_apply
 use ELPH,           ONLY:W_debye
 use timing,         ONLY:live_timing
 use com,            ONLY:msg
 use IO_m,           ONLY:OP_WR_CL,io_control,LOG
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer  :: i_b,i_k,i_w,io_err,ID
 real(SP) :: E_cb_min,E_vb_max,W_diff,Area
 real(SP),    allocatable :: A_t(:,:,:)
 complex(SP), allocatable :: W(:),Sigma(:)
 complex(DP), allocatable :: A_w_DP(:),A_t_DP(:)
 integer,     external    :: io_G_retarded
 real(SP),    external    :: RIntegrate
 !
 call section("=",'Retarded Green`s functions integral (CCA)')
 !============================================================
 !
 Gr_E_steps =Gr_E_window/Gr_E_step
 Gr_T_window=2.*pi*real(Gr_E_steps)/Gr_E_window*.5 ! 50% of the T period
 Gr_T_steps =Gr_T_window/GF_T_step
 !
 Gr_T_window   =GF_T_step*Gr_T_steps
 Gr_E_window=Gr_E_step*Gr_E_steps
 ! 
 call msg("s",'Energy window [eV]:',Gr_E_window*HA2EV)
 call msg("s",'Time window   [fs]:',Gr_T_window/FS2AUT)
 call msg("s",'Time steps        :',Gr_T_steps)
 !
 allocate(Gr(k%nibz,Gr_bands(2),Gr_T_steps))
 allocate(W(Gr_E_steps),A_w_DP(Gr_E_steps),A_t_DP(Gr_T_steps),Sigma(Gr_E_steps))
 !
 ! QP corrections
 !================
 call QP_apply(Gr_bands,E,k,'G',msg_fmt='l')
 !
 E_vb_max=maxval( E%E(:E%nbf,:,:) )
 E_cb_min=minval( E%E(E%nbf+1:,:,:) )
 !
 W_debye=0.06/HA2EV
 ! 
 call live_timing('Fourier Transformation',k%nibz*(Gr_bands(2)-Gr_bands(1)+1))
 !
 do i_k=1,k%nibz
   !
   do i_b=Gr_bands(1),Gr_bands(2)
     !
     A_w_DP=(0._DP,0._DP)
     A_t_DP=(0._DP,0._DP)
     Sigma =(0._SP,0._SP)
     !
     do i_w=1,Gr_E_steps
       !
       W(i_w)=E%E(i_b,i_k,1)+Gr_E_window*(-1./2.+1./real(Gr_E_steps)*i_w)
       !
       select case (trim(Gr_kind))
         !
         case ("QP")
           Sigma(i_w)=cI*Gr_QP_width
           !
         case ("QP-corrected")
           !
           if (i_b<=E%nbf) then
             !
             ! Filled bands
             !==============
             !
             W_diff=real(W(i_w))-E_vb_max
             !
             if (W_diff>0.) cycle
             !
             ! El-Ph part
             !
             if (abs(W_diff)<W_debye) then
               Sigma(i_w)=ci*abs(W_diff)**3*Gr_QP_width/abs(W_debye-E_vb_max)**3
             else
               Sigma(i_w)=cI*Gr_QP_width
             endif
             !
           else
             !
             ! Empty bands
             !=============
             !
             W_diff=real(W(i_w))-E_cb_min
             !
             if (W_diff<0.) cycle
             !
             ! El-Ph part
             !
             if (abs(W_diff)<W_debye) then
               Sigma(i_w)=ci*abs(W_diff)**3*Gr_QP_width/abs(W_debye-E_vb_max)**3
             else
               Sigma(i_w)=cI*Gr_QP_width
             endif
             !
           endif
       end select
       !
       A_w_DP(i_w)=1./pi*aimag(Sigma(i_w))/( ( W(i_w)-E%E(i_b,i_k,1)-real(Sigma(i_w)))**2 + ( aimag(Sigma(i_w)) )**2)
       !
     enddo
     !
     Area=RIntegrate(real(A_w_DP,SP),real(W),Gr_E_steps)
     !
     A_w_DP=A_w_DP/Area
     !
     call RT_1D_Fourier_Transform("W2T"," ",W,A_w_DP,Gr_E_steps,A_t_DP,Gr_T_steps,Gr_T_window/Gr_T_steps,0.,.FALSE.,0)
     Gr(i_k,i_b,:)=A_t_DP
     !
     call live_timing(steps=1)
     !
   enddo
   !
 enddo
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1,2/),COM=LOG,ID=ID)
 io_err=io_G_retarded(ID,"G_retarded")
 !
 deallocate(Gr,W,A_w_DP,A_t_DP,Sigma)
 !
end subroutine RT_G_retarded
