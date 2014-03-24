!
! Copyright (C) 2000-2014 A. Marini and the YAMBO team 
!            http://www.yambo-code.org
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
subroutine RT_apply_RWA(M,ik,E,Time,MODE)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:RT_ibz_coo,SC_bands,RT_all2ibz
 use electrons,      ONLY:levels
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM
 !
 implicit none
 !
 complex(SP),  intent(inout)  :: M(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,      intent(in)     :: ik
 type(levels), intent(in)     :: E
 real(SP),     intent(in)     :: Time
 character(*), intent(in)     :: MODE
 !
 ! Work space
 !
 integer  :: i_n,i_np,ik_RIM,ik_ibz
 real(SP) :: deltaE,pre_factor
 !
 if (MODE=="APPLY" ) pre_factor= 1._SP
 if (MODE=="REMOVE") pre_factor=-1._SP
 !
 ik_ibz=RT_all2ibz(ik)
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=SC_bands(1),SC_bands(2)
     if (l_RT_uses_E_RIM) then
       ik_RIM=ik-RT_ibz_coo(ik_ibz,1)+RIM_ibz_coo(ik_ibz,1)
       deltaE=E%E_RIM(i_n,ik_RIM,1)-E%E_RIM(i_np,ik_RIM,1)
       M(i_n,i_np)=M(i_n,i_np)*exp(pre_factor*cI*deltaE*Time)
     else
       deltaE=E%E(i_n,ik,1)-E%E(i_np,ik,1)
       M(i_n,i_np)=M(i_n,i_np)*exp(pre_factor*cI*deltaE*Time)
     endif
   enddo
 enddo
end subroutine RT_apply_RWA
