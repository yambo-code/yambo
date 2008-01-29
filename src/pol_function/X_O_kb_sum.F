!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
!
! Copyright (C) 1999-2004 ABINIT group
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
subroutine X_O_kb_sum(ivfft,icfft,rho,kbv)
 !
 use pars,          ONLY:SP,DP
 use wave_func,     ONLY:wf,wf_ng
 use D_lattice,     ONLY:pp_kbv_dim
 implicit none
 integer    :: ivfft,icfft
 complex(SP):: rho(3),kbv(wf_ng,pp_kbv_dim,4)
 ! 
 ! Work Space
 !
 integer :: i1,i2
#if defined _DOUBLE
 complex(DP):: zdotc,zdotu
#else 
 complex(SP):: cdotc,cdotu
#endif
 complex(SP) :: Cv,Cc,Kc(wf_ng,3),Kv(wf_ng,3),drho(3)
 !
 Kc=(0.,0.)
 Kv=(0.,0.)
 !
 do i1=1,pp_kbv_dim
#if defined _DOUBLE
   Cv=zdotu(wf_ng,wf(:,ivfft),1,kbv(:,i1,1),1)
   Cc=zdotu(wf_ng,wf(:,icfft),1,kbv(:,i1,1),1)
#else
   Cv=cdotu(wf_ng,wf(:,ivfft),1,kbv(:,i1,1),1)
   Cc=cdotu(wf_ng,wf(:,icfft),1,kbv(:,i1,1),1)
#endif
   if (Cc==(0.,0.)) cycle
   do i2=1,3
#if defined _DOUBLE
     call zaxpy(wf_ng,Cc,conjg(kbv(:,i1,1+i2)),1,Kc(:,i2),1)
     call zaxpy(wf_ng,conjg(Cv),kbv(:,i1,1+i2),1,Kv(:,i2),1)
#else
     call caxpy(wf_ng,Cc,conjg(kbv(:,i1,1+i2)),1,Kc(:,i2),1)
     call caxpy(wf_ng,conjg(Cv),kbv(:,i1,1+i2),1,Kv(:,i2),1)
#endif
   enddo
 enddo
 !
 do i1=1,3
#if defined _DOUBLE
   drho(i1)=zdotc(wf_ng,wf(:,ivfft),1,Kc(1,i1),1)+&
&           zdotu(wf_ng,wf(:,icfft),1,Kv(1,i1),1)
#else
   drho(i1)=cdotc(wf_ng,wf(:,ivfft),1,Kc(1,i1),1)+&
&           cdotu(wf_ng,wf(:,icfft),1,Kv(1,i1),1)
#endif
 enddo
 !
 rho=rho+drho
 !
end subroutine
