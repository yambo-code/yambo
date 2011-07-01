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
subroutine RT_split_Polarization(nh,nt,W,X,P,what)
 !
 use pars,           ONLY:SP,cI,pi,schlen
 use YPP,            ONLY:i_t_start
 use units,          ONLY:FS2AUT,HARTREE,HBAR_eVfs
 use real_time,      ONLY:RT_step,RT_P,RT_output
 use stderr,         ONLY:intc
 use timing,         ONLY:live_timing
 implicit none
 !
 integer,     intent(in)   :: nt,nh
 real(SP),    intent(inout):: P(nt,3),W
 complex(SP), intent(in)   :: X(nh,3)
 character(*),intent(in)   :: what
 !
 ! Work Space
 !
 integer   :: i_n,i_t,i_c,nh_min
 real(SP)  :: P_num(nh,nt,3),T_i
 character(schlen) :: P_file_name
 !
 nh_min=1
 if (what=="subtract") nh_min=nh
 if (what=="dump") call live_timing('Partial P(t):',nt*nh)
 !
 do i_n=nh_min,nh
   !
   if (what=="dump") then
     P_file_name="dP_order_"//trim(intc(i_n-1))
     call RT_output("open "//trim(P_file_name) )
   endif
   !
   do i_t=1,nt
     !
     T_i = (i_t-1)*RT_step
     P_num(i_n,i_t,:)=X(i_n,:)*exp((i_n-1)*cI*W*T_i)+conjg(X(i_n,:))*exp(-(i_n-1)*cI*W*T_i)
     !
     if (what=="dump") then
       do i_c=1,3
         RT_P(i_c) = P(i_t,i_c)-sum( P_num(:i_n,i_t,i_c) )
       enddo
       call RT_output(trim(P_file_name),TIME=(i_t-1)*RT_step/FS2AUT)
     endif
     !
     if (what=="subtract") P(i_t,:)=P(i_t,:)-P_num(i_n,i_t,:) 
     if (what=="dump") call live_timing(steps=1)
     !
   enddo
   !
   if (what=="dump") call RT_output("close "//trim(P_file_name))
   !
 enddo
 !
 if (what=="dump") call live_timing()
 !
end subroutine RT_split_Polarization
