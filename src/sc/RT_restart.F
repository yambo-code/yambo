!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_restart(Xen)
 !
 use com,            ONLY:msg,warning
 use memory_m,       ONLY:mem_est
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels,n_sp_pol
 use real_time,      ONLY:G_lesser,ID,io_J,io_G,ID,Tot_steps,&
&                         G_lesser_previous,G_lesser_reference,Thermal_steps, &
&                         LifeTime
 use SC,             ONLY:SC_bands,SC_R,E_convergence,it_now,&
&                        rho_convergence,H_rotate
 use IO_m,           ONLY:OP_RD,OP_RD_CL,RD_CL_IF_END,io_control,REP,VERIFY,NONE
 implicit none
 !
 type(levels)      :: Xen
 integer, external :: io_SC_components
 !
 ! Work Space
 !
 integer :: ik,it
 integer :: io_E,io_V,io_WF
 logical :: RESTART
 logical :: RESTART_FROM_SC
 !
 ! Starting point 
 !
 it_now=1
 !
 ! Perform a first I/O to check which DBs are present. 
 !
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(1)) ! Energies
 io_E=io_SC_components('E' ,Xen,ID(1))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2)) ! WFs
 io_WF=io_SC_components('WF',Xen,ID(2))
 call io_control(ACTION=OP_RD_CL,COM=REP ,SEC=(/1/),MODE=VERIFY,ID=ID(3)) ! Local potential
 io_V=io_SC_components('V' ,Xen,ID(3))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(4)) ! Current response
 io_J=io_SC_components('J' ,Xen,ID(4))
 call io_control(ACTION=OP_RD_CL,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5)) ! G_lesser
 io_G=io_SC_components('G' ,Xen,ID(5))
 !
 ! To restart from RT only the G_lesser is necessary
 !
 RESTART        = io_G ==0
 !
 RESTART_FROM_SC=all((/io_E==0,io_WF==0,io_V==0,io_G/=0/))  
 !
 if(RESTART_FROM_SC) then
   !
   !  I only need SC_R than I can reconstruct G_lesser(:,:,:)
   !  rotating the non-integrating G_lesser
   !
   allocate(SC_R(SC_bands,SC_bands,QP_nk,n_sp_pol))
   call mem_est("SC_R ",(/size(SC_R)/))
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(2))
   io_WF=io_SC_components('WF',Xen,ID(2))
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,SEC=(/ik+1/),ID=ID(2))
     io_WF=io_SC_components('WF',Xen,ID(2))
   enddo
   !
   call msg('snr','[RT] Restart from a Self-Consistent Calculation ')
   call msg('nr' ,'[RT] Found   accuracy on density      :',rho_convergence)
   call msg('rn' ,'[RT]                  on energies [ev]:',E_convergence*HARTREE)
   !
   if ( E_convergence>0.01.and.rho_convergence>1e-4) &
&    call warning(" Restart from a non-converged Self-Consistent calculation! ")
   !
   if ( Thermal_steps/=0) &
&    call warning(" Restart from a SC does not work with thermalization! ")
   !
   ! Now I build the G_lesser rotating the non interaction one
   ! G_lesser(i1,i1,ik)=(0.,1)*en%f(i1,ik)
   !
   do ik=1,QP_nk
     call H_rotate(SC_R(:,:,ik,1),G_lesser(:,:,ik),SC_bands,-1)
   enddo
   !
   ! Starting index for the real-time dynamics
   !
   it_now=1
   !
 endif
 !
 if(RESTART) then
   !
   !  I use G_lesser(:,:,:) to reconstruct everything
   !  and I read it_now from the databases
   !
   call io_control(ACTION=OP_RD,COM=NONE,SEC=(/1/),MODE=VERIFY,ID=ID(5))
   io_G=io_SC_components('G',Xen,ID(5))
   !
   do ik=1,QP_nk
     call io_control(ACTION=RD_CL_IF_END,COM=NONE,SEC=(/ik+1/),MODE=VERIFY,ID=ID(5))
     io_G=io_SC_components('G',Xen,ID(5))
   enddo
   !
   if(io_G==0.and.it_now<Tot_steps+Thermal_steps) then
     !
     call msg('snr','[RT] Restart from a Real-Time Calculation ')
     call msg('snr','[RT] Number of steps done         :',it_now)
     !
   endif
   !
   it_now=it_now+1
   !
 else
   !
   it_now=1
   !
 endif
 !
 ! Initialize G_lesser_previous and G_lesser_reference
 !
 G_lesser_previous =G_lesser   
 if(LifeTime/=0.0) G_lesser_reference=G_lesser   
 !
end subroutine RT_restart
