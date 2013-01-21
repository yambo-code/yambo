! 
! Copyright (C) 2000-2013 A. Marini and the YAMBO team
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
 use pars,           ONLY:SP,DP,pi
 use units,          ONLY:FS2AUT,HA2EV
 use YPP,            ONLY:GF_T_step,Gr_E_step
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use real_time,      ONLY:Gr,Gr_kind,Gr_bands,Gr_T_steps,Gr_T_window,Gr_E_window,Gr_E_steps,&
&                         Gr_peak_pos,Gr_E_ranges,Gr_T_step,Gr_QP_width
 use QP_CTL_m,       ONLY:QP_apply
 use QP_m,           ONLY:QP_ctl_Wc,QP_ctl_Wv
 use ELPH,           ONLY:W_debye
 use timing,         ONLY:live_timing
 use com,            ONLY:msg,warning
 use IO_m,           ONLY:OP_WR_CL,OP_RD_CL,io_control,LOG,DUMP,NONE,VERIFY
 implicit none
 !
 type(bz_samp) ::k
 type(levels)  ::E
 !
 ! Work Space
 !
 integer  :: i_b,i_k,i_w,io_err,ID,i_t
 real(SP) :: Area,Time
 complex(SP), allocatable :: W(:)
 complex(DP), allocatable :: A_w_DP(:),A_t_DP(:)
 integer,     external    :: io_G_retarded,ioELPH
 real(SP),    external    :: RIntegrate,RT_G_retarded_frequency
 complex(SP), external    :: RT_G_retarded_time
 !
 call section("=",'Retarded Green`s functions')
 !=============================================
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err=ioELPH(ID,'gkkp')
 if (io_err/=0) then
   call io_control(ACTION=OP_RD_CL,COM=NONE,MODE=DUMP,SEC=(/1/),ID=ID)
   io_err=ioELPH(ID,'gkkp_expanded')
 else
   W_debye=0.
 endif
 !
 if (io_err/=0.and.trim(Gr_kind)/="HC".and.trim(Gr_kind)/="QP") then
   call warning("Missing ELPH database. Unknown Debye energy. Switching to plain QP.")
   Gr_kind="QP"
 endif
 !
 Gr_QP_width=QP_ctl_Wc(3,1,1)/HA2EV
 !
 if (trim(Gr_kind)=="QP-cut".or.trim(Gr_kind)=="BOX") Gr_E_window=4*Gr_QP_width
 !
 Gr_E_steps =Gr_E_window/Gr_E_step
 Gr_T_window=2.*pi*real(Gr_E_steps)/Gr_E_window*.5 ! 50% of the T period
 Gr_T_steps =Gr_T_window/GF_T_step
 !
 Gr_T_window=GF_T_step*Gr_T_steps
 Gr_E_window=Gr_E_step*Gr_E_steps
 !
 Gr_T_step=GF_T_step
 !
 if (.not.trim(Gr_kind)=="HC") call msg("s",'Energy window [eV]:',Gr_E_window*HA2EV)
 !
 call msg("s",'Time window   [fs]:',Gr_T_window/FS2AUT)
 call msg("s",'Time step     [fs]:',Gr_T_step/FS2AUT)
 call msg("s",'Time steps        :',Gr_T_steps)
 !
 call io_control(ACTION=OP_RD_CL,SEC=(/1,2/),MODE=VERIFY,COM=LOG,ID=ID)
 io_err=io_G_retarded(ID,"G_retarded")
 !
 if (io_err==0) return
 !
 allocate(Gr(k%nibz,Gr_bands(2),Gr_T_steps))
 allocate(Gr_peak_pos(k%nibz,Gr_bands(2)))
 allocate(Gr_E_ranges(k%nibz,Gr_bands(2),2))
 allocate(W(Gr_E_steps),A_w_DP(Gr_E_steps),A_t_DP(Gr_T_steps))
 !
 Gr_E_ranges=0._SP
 Gr_peak_pos=0._SP
 !
 ! QP corrections
 !================
 call QP_apply(Gr_bands,E,k,'G',msg_fmt='l')
 !
 call live_timing('Fourier Transformation',k%nibz*(Gr_bands(2)-Gr_bands(1)+1))
 !
 do i_w=1,Gr_E_steps
   !
   W(i_w)=Gr_E_window*(-1./2.+1./real(Gr_E_steps)*i_w)
   !
 enddo
 !
 do i_k=1,k%nibz
   !
   do i_b=Gr_bands(1),Gr_bands(2)
     !
     A_w_DP=(0._DP,0._DP)
     A_t_DP=(0._DP,0._DP)
     !
     Gr_peak_pos(i_k,i_b)=E%E(i_b,i_k,1)
     !
     Gr_E_ranges(i_k,i_b,:)=Gr_QP_width
     !
     if (trim(Gr_kind)=="HC") then
       !
       do i_t=1,Gr_T_steps
         !
         Time= Gr_T_window/Gr_T_steps*i_t
         Gr(i_k,i_b,i_t)=RT_G_retarded_time("HC",E,i_b,i_k,Time,0.)
         A_t_DP(i_t)=Gr(i_k,i_b,i_t)
         !
       enddo
       !
     else
       !
       do i_w=1,Gr_E_steps
         !
         A_w_DP(i_w)=RT_G_retarded_frequency(trim(Gr_kind),E,i_b,i_k,real(W(i_w)),Gr_QP_width)
         !
       enddo
       !
       Area=RIntegrate(real(A_w_DP,SP),real(W),Gr_E_steps)
       A_w_DP=A_w_DP/Area
       !
       call RT_1D_Fourier_Transform("W2T"," ",W,A_w_DP,Gr_E_steps,A_t_DP,Gr_T_steps,Gr_T_window/Gr_T_steps,0.,.FALSE.,0)
       Gr(i_k,i_b,:)=A_t_DP
       !
     endif
     !
     !DEBUG>
     !do i_w=1,Gr_E_steps
     !  write (100,*) real(W(i_w))*HA2EV,real(A_w_DP(i_w))
     !enddo
     !do i_t=1,Gr_T_steps
     !  Time= Gr_T_window/Gr_T_steps*i_t
     !  write (200,*) Time/FS2AUT,real(A_t_DP(i_t))
     !enddo
     !call RT_1D_Fourier_Transform("T2W"," ",W,A_w_DP,Gr_E_steps,A_t_DP,Gr_T_steps,Gr_T_window/Gr_T_steps,0.,.FALSE.,1)
     !do i_w=1,Gr_E_steps
     !  write (300,*) real(W(i_w))*HA2EV,real(A_w_DP(i_w))/pi
     !enddo
     !stop
     !DEBUG<
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
 deallocate(Gr,W,A_w_DP,A_t_DP,Gr_peak_pos,Gr_E_ranges)
 !
end subroutine RT_G_retarded
