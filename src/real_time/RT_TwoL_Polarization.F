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
subroutine RT_TwoL_Polarization(E,eval_P,k)
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:HARTREE
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:nkibz,bz_samp 
 use D_lattice,      ONLY:DL_vol
 use X_m,            ONLY:DIP_P,DIP_iR
 use fields,         ONLY:Efield,i_Pump,i_Probe
 use SC,             ONLY:SC_bands
 use com,            ONLY:msg
 use real_time,      ONLY:RT_P_2levels,P_2levels_is_done,Poles_2levels_save
 use memory_m,       ONLY:mem_est
 !
 implicit none
 type(levels),            intent(in) :: E
 logical     ,            intent(in) :: eval_P
 type(bz_samp), optional, intent(in) :: k
 ! 
 ! Work Space
 !
 integer      :: iv,ic,ik,i_ref(3)
 complex(SP)  :: Rabi,G_lesser,D(3),Rabi_prefactor
 real(SP)     :: Rabi_max,E_field(3)
 !
 if (i_Pump/=i_Probe) return
 !
 if (.not.allocated(Poles_2levels_save)) then
   allocate(Poles_2levels_save(SC_bands,SC_bands,nkibz,4))
   call mem_est("Poles_2levels_save",(/size(Poles_2levels_save)/),(/2*SP/))
 endif
 !
 P_2levels_is_done=.TRUE.
 !
 E_field(:) = Efield(i_Pump)%versor(:)* Efield(i_Pump)%amplitude
 !
 Rabi_max=-1.
 Rabi_prefactor=(1.,0.)
 !
 select case(trim(Efield(i_Pump)%ef_name))
   case('ANTIRES')
     Rabi_prefactor=1./Efield(i_Pump)%frequency(1)/cI
   case('RES')
     Rabi_prefactor=-1./Efield(i_Pump)%frequency(1)/cI
 end select
 !
 if (eval_P) RT_P_2levels(:)=(0.,0.)
 !
 do ik=1,nkibz
   do iv=1,E%nbm
     do ic=E%nbf+1,SC_bands
       !
       if (abs(E%E(iv,ik,1)-E%E(ic,ik,1))<=1.E-5_SP) cycle
       !
       Rabi =  2. * sum(DIP_P(:,ic,iv,ik,1) * E_field(:)) *Rabi_prefactor
       !
       if (abs(Rabi) > Rabi_max) then
         Rabi_max=abs(Rabi)
         i_ref(:)=(/iv,ic,ik/)
       endif
       !
       if (eval_P) then
         call RT_TwoL_Glesser(Poles_2levels_save(iv,ic,ik,:),E%E(ic,ik,1)-E%E(iv,ik,1),Rabi,G_lesser)
         D(:)=cI*DIP_iR(:,ic,iv,ik,1)
         RT_P_2levels(:)=RT_P_2levels(:)-cI*(-D(:)*conjg(G_lesser)+conjg(D(:))*G_lesser)/DL_vol*k%weights(ik)
       endif
       !
     enddo
   enddo
 enddo
 !
 if (.not.eval_P) then
   call msg( 'r', '[RT] Max Rabi frequency            [eV]:',Rabi_max*HARTREE)
   return
 endif
 !
end subroutine RT_TwoL_Polarization
