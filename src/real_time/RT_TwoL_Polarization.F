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
subroutine RT_TwoL_Polarization(E,k)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nkibz,bz_samp 
 use D_lattice,      ONLY:DL_vol
 use X_m,            ONLY:DIP_P,DIP_iR
 use fields,         ONLY:Efield,i_Pump,i_Probe,Rabi_Factor
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:RT_P_2levels,P_2levels_is_done,Poles_2levels_save
 use memory_m,       ONLY:mem_est
 !
 implicit none
 type(levels),            intent(in) :: E
 type(bz_samp),           intent(in) :: k
 ! 
 ! Work Space
 !
 integer      :: iv,ic,ik
 complex(SP)  :: Rabi,G_lesser,D(3)
 !
 if (i_Pump/=i_Probe) return
 !
 if (.not.allocated(Poles_2levels_save)) then
   allocate(Poles_2levels_save(SC_bands(2),SC_bands(2),nkibz,4))
   call mem_est("Poles_2levels_save",(/size(Poles_2levels_save)/),(/2*SP/))
 endif
 !
 P_2levels_is_done=.TRUE.
 !
 RT_P_2levels(:)=(0.,0.)
 !
 do ik=1,nkibz
   do iv=1,E%nbm
     do ic=E%nbf+1,SC_bands(2)
       !
       if (abs(E%E(iv,ik,1)-E%E(ic,ik,1))<=1.E-5_SP) cycle
       !
       Rabi=Rabi_Factor(Efield(i_Pump), DIP_P(:,ic,iv,ik,1),-cI*DIP_iR(:,ic,iv,ik,1))
       !
       call RT_TwoL_Glesser(Poles_2levels_save(iv,ic,ik,:),E%E(ic,ik,1)-E%E(iv,ik,1),Rabi,G_lesser)
       D(:)=cI*DIP_iR(:,ic,iv,ik,1)
       RT_P_2levels(:)=RT_P_2levels(:)-cI*(-D(:)*conjg(G_lesser)+conjg(D(:))*G_lesser)/DL_vol*k%weights(ik)
       !
     enddo
   enddo
 enddo
 !
end subroutine RT_TwoL_Polarization
