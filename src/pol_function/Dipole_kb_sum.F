!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
!
! Copyright (C) 1992-2005, Lucia Reining, Valerio Olevano,
!   Francesco Sottile, Stefan Albrecht, Giovanni Onida,
!                    Fabien Bruneval
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
subroutine Dipole_kb_sum(ivfft,icfft,rho,kbv)
 !
 use pars,          ONLY:SP,DP
 use wrapper,       ONLY:Vstar_dot_V,V_dot_V,V_by_V_plus_V
 use wave_func,     ONLY:wf,wf_ng
 use pseudo,        ONLY:pp_kbv_dim,kbv_spin
 implicit none
 integer    :: ivfft,icfft
 complex(SP):: rho(3),kbv(wf_ng,pp_kbv_dim,7)
 ! 
 ! Work Space
 !
 integer     :: i1,i2
 complex(SP) :: Cv,Cc,Kc(wf_ng,3),Kv(wf_ng,3),drho(3)
 !
 Kc=(0.,0.)
 Kv=(0.,0.)
 !
 do i1=1,pp_kbv_dim
   !
   if(kbv_spin(i1)==2) cycle
   !
   Cv=V_dot_V(wf_ng,wf(:,ivfft),kbv(:,i1,1))
   Cc=V_dot_V(wf_ng,wf(:,icfft),kbv(:,i1,1))
   !
   if (Cc==(0.,0.)) cycle
   do i2=1,3
     call V_by_V_plus_V(wf_ng,Cc,conjg(kbv(:,i1,1+i2)),Kc(:,i2))
     call V_by_V_plus_V(wf_ng,conjg(Cv),kbv(:,i1,1+i2),Kv(:,i2))
   enddo
 enddo
 !
 do i1=1,3
   drho(i1)=Vstar_dot_V(wf_ng,wf(:,ivfft),Kc(1,i1))+V_dot_V(wf_ng,wf(:,icfft),Kv(1,i1))
 enddo
 !
 rho=rho+drho
 !
end subroutine
