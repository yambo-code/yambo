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
!
subroutine RT_eps_analyze(j_current,A_t)
 !
 use pars,      ONLY:SP,DP,schlen
 use com,       ONLY:msg,of_open_close
 use real_time, ONLY:EF_q0,tot_steps,Ext_Field_name,Ext_Field_Int,RT_step
 !
 implicit none
 real(DP),    intent(in) :: j_current(tot_steps,3)
 complex(DP), intent(in) :: A_t(tot_steps)
 character(10)           :: headings(10) 
 !
 ! Work Space
 !
 complex(DP) :: chi(tot_steps/2+1,3)
 complex(DP), allocatable :: A_w(:)
 real(SP)    :: w_eps(7),omega
 integer     :: i1,i2,fft_sign
 integer(8)  :: fftw_plan
 character(schlen):: file_name
 !
 fftw_plan=0
 fft_sign=-1
 !
 do i1=1,3
#if defined _FFTW
   call fft_1d_real(j_current(1,i1),chi(1,i1),tot_steps,fft_sign,fftw_plan)
#else
   call fft_1d_real(j_current(1,i1),chi(1,i1),tot_steps,fft_sign)
#endif
 enddo
 !
 if(Ext_Field_Name/="DELTA") then
   !        
   allocate(A_w(tot_steps))
   A_w=A_t
   fftw_plan=0
   !
#if defined _FFTW  
   call fft_1d(A_w,tot_steps,fft_sign,fftw_plan)
#else
   call fft_1d(A_w,tot_steps,fft_sign)
#endif
   !
   forall(i1=1:tot_steps/2+1)
     chi(i1,:)=chi(i1,:)/((omega*dble(i1))**2*A_w(i1))
   end forall
   !
   deallocate(A_w)
   !
 else
   !      
   forall(i1=1:tot_steps/2+1)
     chi(i1,:)=chi(i1,:)/(omega*dble(i1)*Ext_Field_Int)
   end forall
   !
 endif
 !
 omega=1._SP/RT_step   ! Frequency convertion factor to 1/fs to eV
 !
 !
 file_name='eps_q001_rt'
 call of_open_close(file_name,'ot')
 !
 headings(1)  ='E/ev[1]'
 headings(2:3)=(/'e/Im[2]','e/Re[3]'/)
 headings(4:5)=(/'e_od/Im[4]','e_od/Re[5]'/)
 headings(6:7)=(/'e_od/Im[6]','e_od/Re[7]'/)
 call msg('o '//file_name,'#',headings(1:7),INDENT=0,USE_TABS=.true.)
 !
 do i1=1,tot_steps/2+1
   !      
   w_eps(1)   = omega*dble(i1)
   !
   do i2=1,3
     w_eps(2*i2+1)     = real((1._SP,0._SP)+chi(i1,i2))
     w_eps(2*i2)       = aimag((1._SP,0._SP)+chi(i1,i2))
   enddo
   !
   call msg('o '//file_name,'',w_eps(1:7),INDENT=-2,USE_TABS=.TRUE.)
   !
 enddo
 !
 call of_open_close(file_name)  
 !
end subroutine RT_eps_analyze

