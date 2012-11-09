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
subroutine RT_retarded_GFs_for_the_CCA(E,k)
 !
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:FS2AUT,HA2EV
 use YPP,            ONLY:GF_QP_width,GF_T_step,GF_E_step,GF_E_window
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window
 use QP_CTL_m,       ONLY:QP_apply
 use timing,         ONLY:live_timing
 use com,            ONLY:msg
 use IO_m,           ONLY:OP_WR,WR_CL_IF_END,io_control,NONE,LOG
 use global_XC,      ONLY:G_E_xc_string
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer  :: i_b,i_k,i_w,E_steps,io_err,ID
 real(SP),    allocatable :: A_w(:),A_t(:,:,:)
 complex(SP), allocatable :: W(:)
 complex(DP), allocatable :: A_w_DP(:),A_t_DP(:)
 integer,     external    :: io_Green_Functions
 real(SP),    external    :: RIntegrate
 !
 call section("=",'Retarded Green`s functions integral (CCA)')
 !============================================================
 !
 E_steps=GF_E_window/GF_E_step
 Gr_T_window=2.*pi*real(E_steps)/GF_E_window*.5 ! 50% of the T period
 Gr_T_steps =Gr_T_window/GF_T_step
 !
 Gr_T_window   =GF_T_step*Gr_T_steps
 GF_E_window=GF_E_step*E_steps
 ! 
 call msg("s",'Energy window [eV]:',GF_E_window*HA2EV)
 call msg("s",'Time window   [fs]:',Gr_T_window/FS2AUT)
 !
 allocate(A_w(E_steps),Gr(k%nibz,Gr_bands(2),Gr_T_steps),W(E_steps),A_w_DP(E_steps),A_t_DP(Gr_T_steps))
 !
 call QP_apply(Gr_bands,E,k,'G',msg_fmt='l')
 !
 call io_control(ACTION=OP_WR,SEC=(/1/),COM=LOG,ID=ID)
 io_err=io_Green_Functions(ID)
 ! 
 call live_timing('Fourier Transformation',k%nibz*(Gr_bands(2)-Gr_bands(1)+1))
 !
 do i_k=1,k%nibz
   !
   do i_b=Gr_bands(1),Gr_bands(2)
     !
     select case (trim(Gr_kind))
       !
       case ("QP")
         !
         do i_w=1,E_steps
           W(i_w)=E%E(i_b,i_k,1)+GF_E_window*(-1./2.+1./real(E_steps)*i_w)
           A_w(i_w)=GF_QP_width/pi/((W(i_w)-E%E(i_b,i_k,1))**2+GF_QP_width**2)
         enddo
         !
         A_w_DP=A_w
         call RT_1D_Fourier_Transform("W2T"," ",W,A_w_DP,E_steps,A_t_DP,Gr_T_steps,Gr_T_window/Gr_T_steps,0.,.FALSE.,0)
         Gr(i_k,i_b,:)=A_t_DP
         !
         ! DEBUG
         !do i_t=1,Gr_T_steps
         !  TIME= Gr_T_window/Gr_T_steps*i_t
         !  write (100,'(5f20.10)') TIME/FS2AUT,A_t(i_k,i_b,i_t),cos(E%E(i_b,i_k,1)*TIME)*exp(-GF_QP_width*TIME)
         !enddo
         !stop
         !
     end select
     !
     call live_timing(steps=1)
     !
   enddo
   !
   call io_control(ACTION=WR_CL_IF_END,SEC=(/i_k+1/),COM=NONE,ID=ID)
   io_err=io_Green_Functions(ID)
   !
 enddo
 !
 deallocate(A_w,Gr,W,A_w_DP,A_t_DP)
 !
end subroutine RT_retarded_GFs_for_the_CCA
