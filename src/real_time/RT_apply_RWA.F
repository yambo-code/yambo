!
!        Copyright (C) 2000-2014 the YAMBO team
!            http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine RT_apply_RWA(ik,E,Time,dT,MODE,M1,M2,M3)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:RT_ibz_coo,SC_bands,RT_all2ibz
 use electrons,      ONLY:levels
 use real_time,      ONLY:RIM_ibz_coo,l_RT_uses_E_RIM
 !
 implicit none
 !
 integer,      intent(in)     :: ik
 type(levels), intent(in)     :: E
 real(SP),     intent(in)     :: Time,dT
 character(*), intent(in)     :: MODE
 !
 complex(SP),  intent(inout)            :: M1(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP),  intent(inout), optional  :: M2(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP),  intent(inout), optional  :: M3(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 !
 ! Work space
 !
 integer  :: i_n,i_np,ik_RIM,ik_ibz
 real(SP) :: deltaE,pre_factor,alpha,alpha1
 complex(SP) :: phase,phase1
 !
 if (MODE=="APPLY" ) pre_factor= 1._SP
 if (MODE=="REMOVE") pre_factor=-1._SP
 !
 ik_ibz=RT_all2ibz(ik)
 ik_RIM=ik-RT_ibz_coo(ik_ibz,1)+RIM_ibz_coo(ik_ibz,1)
 !
 do i_n=SC_bands(1),SC_bands(2)
   do i_np=i_n+1,SC_bands(2)
     if (     l_RT_uses_E_RIM) deltaE=E%E_RIM(i_n,ik_RIM,1)-E%E_RIM(i_np,ik_RIM,1)
     if (.not.l_RT_uses_E_RIM) deltaE=E%E(i_n,ik,1)-E%E(i_np,ik,1)
     alpha=pre_factor*deltaE*Time
     phase=cmplx(cos(alpha),sin(alpha))
     M1(i_n,i_np)=M1(i_n,i_np)*      phase
     M1(i_np,i_n)=M1(i_np,i_n)*conjg(phase)
     if(present(M2)) then
       M2(i_n,i_np)=M2(i_n,i_np)*      phase
       M2(i_np,i_n)=M2(i_np,i_n)*conjg(phase)
     endif
     if(present(M3)) then
       alpha1=pre_factor*deltaE*dT
       !phase1=1._SP +cI*alpha1 -alpha1**2._SP/2._SP -cI*alpha1**3._SP/6._SP
       phase1=cmplx(cos(alpha1),sin(alpha1))
       M3(i_n,i_np)=M3(i_n,i_np)*      phase *phase1
       M3(i_np,i_n)=M3(i_np,i_n)*conjg(phase *phase1)
     endif
   enddo
 enddo
 !
end subroutine RT_apply_RWA
