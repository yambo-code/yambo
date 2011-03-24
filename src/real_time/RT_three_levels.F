!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
subroutine RT_three_levels(G_lesser,it)
 !
 use pars,           ONLY:SP,pi,lchlen,schlen,cI
 use com,            ONLY:msg,of_open_close
 use units,          ONLY:FS2AUT
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use units,          ONLY:HARTREE
 use com,            ONLY:msg
 use X_m,            ONLY:DIP_iR,DIP_P
 use real_time,      ONLY:it_start,RT_step,Thermal_steps,NE_steps,Wo,W1,&
&                         W_pump,Rabi_freq_pump,Phase_LifeTime,tau_offdiag 
 use fields,         ONLY:Efield
 !
 implicit none
 !
 integer,     intent(in)   :: it
 complex(SP), intent(in)   :: G_lesser(SC_bands,SC_bands,QP_nk) 
 !
 ! work space
 !
 integer, parameter       :: n_files=2
 integer, save            :: n_headings(n_files)
 real(SP)                 :: data_to_dump(10),Time
 real(SP)                 :: rabi12,rabi13,rabi23
 complex(SP)              :: G12,G13,G23
 character(schlen)        :: headings(n_files,10)
 character(lchlen), save  :: file_name(n_files)
 integer                  :: i_f,i_Pump,i_Probe
 !
 i_Probe=2
 i_Pump =1
 if (Efield(2)%ef_name/="none") then
   i_Pump=2
   i_Probe=1
 endif
 !
 if(it==it_start) then
   !
   n_headings(1)=7
   headings(1,1)="Time[fs]"
   headings(1,2:7)=(/'G_12/Im','G_12/Re','G_13/Im','G_13/Re','G_23/Im','G_23/Re'/)
   file_name(1)="G_theo"
   !
   n_headings(2)=7
   headings(2,1)="Time[fs]"
   headings(2,2:7)=(/'G_12/Im','G_12/Re','G_13/Im','G_13/Re','G_23/Im','G_23/Re'/)
   file_name(2)="G_num"
   !
   do i_f=1,n_files
     call of_open_close(file_name(i_f),'ot')
     call msg('o '//trim(file_name(i_f)),'#')
     call msg('o '//trim(file_name(i_f)),'#',headings(i_f,1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(file_name(i_f)),'#')
   enddo
   !
   return
   !
 elseif (it==Thermal_steps+NE_steps) then
   !
   do i_f=1,n_files
     call of_open_close(file_name(i_f))
   enddo
   !
   return
   !
 endif
 !
 Time           =(it-Thermal_steps-1)*RT_step
 data_to_dump(1)=Time/FS2AUT
 !
 ! Rabi freq 2._SP*Efied(1)%amplitude*DIP_P/W_pump
 !
 rabi12=cI*Efield(i_Pump)%amplitude*sum(Efield(i_Pump)%versor(:)*DIP_P(:,2,1,1,1))/W_pump
 rabi13=cI*Efield(i_Pump)%amplitude*sum(Efield(i_Pump)%versor(:)*DIP_P(:,3,1,1,1))/W_pump
 rabi23=cI*Efield(i_Pump)%amplitude*sum(Efield(i_Pump)%versor(:)*DIP_P(:,3,2,1,1))/W_pump
 !
 G12=-2._SP*cI*rabi12/(W_pump+Wo+cI/tau_offdiag(1))*(exp(-cI*W_pump*Time)-exp(cI*(Wo+cI/tau_offdiag(1))*Time))
 G13=-2._SP*cI*rabi13/(W_pump+W1+cI/tau_offdiag(2))*(exp(-cI*W_pump*Time)-exp(cI*(W1+cI/tau_offdiag(2))*Time))
 !
 if(trim(Efield(i_Pump)%ef_name)=="SIN") then
   !
   W_pump=-W_pump
   !
   G12=G12-2._SP*cI*rabi12 &
&      /(W_pump+Wo+cI/tau_offdiag(1))*(exp(-cI*W_pump*Time)-exp(cI*(Wo+cI/tau_offdiag(1))*Time))
   G13=G13-2._SP*cI*rabi13 &
&      /(W_pump+W1+cI/tau_offdiag(2))*(exp(-cI*W_pump*Time)-exp(cI*(W1+cI/tau_offdiag(2))*Time))
   !
   G12=G12/(0._SP,2._SP)
   G13=G13/(0._SP,2._SP)
   !
   W_pump=-W_pump
   !
 endif
 !
 i_f=1
 data_to_dump(2)= real(G12)
 data_to_dump(3)=aimag(G12)
 data_to_dump(4)= real(G13)
 data_to_dump(5)=aimag(G13)
 data_to_dump(6)= real(G23)
 data_to_dump(7)=aimag(G23)
 call msg('o '//trim(file_name(i_f)),'',data_to_dump(1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
 !
 i_f=2
 data_to_dump(2:7)=0._SP
 data_to_dump(2)= real(G_lesser(1,2,1))
 data_to_dump(3)=aimag(G_lesser(1,2,1))
 data_to_dump(4)= real(G_lesser(1,3,1))
 data_to_dump(5)=aimag(G_lesser(1,3,1))
 data_to_dump(6)= real(G_lesser(2,3,1))
 data_to_dump(7)=aimag(G_lesser(2,3,1))
 call msg('o '//trim(file_name(i_f)),'',data_to_dump(1:n_headings(i_f)),INDENT=0,USE_TABS=.TRUE.)
 !
 contains
   !
   complex(SP) function G12_LR(Time,Rabi,W)
     !
     implicit none
     !
     real(SP)    :: Time,W
     complex(SP) :: Rabi,G12_LRR
     !
     G12_LRR=-cI*Rabi/(Wo+cI/Phase_LifeTime-W)*exp(cI*W*Time)
     !
   end function
   !
end subroutine RT_three_levels
