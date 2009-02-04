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
subroutine RT_Integrator(ik,it,en,Int_Type)
 !
 use pars,           ONLY:SP,schlen
 use SC,             ONLY:SC_potential,SC_bands
 use real_time,      ONLY:G_lesser,G_l_old,Go_Phases,I1_lesser,I2_lesser,RT_step&
&                        ,n_poles
 use electrons,      ONLY:levels
 use X_m,            ONLY:X_poles,X_poles_tab 
 implicit none
 integer, intent(in)      :: ik,it
 type(levels), intent(in) :: en
 character(schlen)        :: Int_Type
 ! 
 ! Work Space
 !
 complex(SP) :: P_Factor(SC_bands,SC_bands)
 integer     :: i1,i2,i,j,i_pole,iv,ic
 !
 if(trim(int_type)=='diagonal') then
   !        

#if defined _DOUBLE
   call zcopy(SC_bands*SC_bands,G_lesser(:,:,ik), 1, G_l_old(:,:,ik), 1)
   P_Factor=(0._SP,0._SP)
   call zgerc(SC_bands,SC_bands,(1._SP,0._SP),Go_Phases(:,ik),1,Go_Phases(:,ik),1,P_Factor,SC_bands)
#else
   call ccopy(SC_bands*SC_bands,G_lesser(:,:,ik), 1, G_l_old(:,:,ik), 1)
   P_Factor=(0._SP,0._SP)
   call cgerc(SC_bands,SC_bands,(1._SP,0._SP),Go_Phases(:,ik),1,Go_Phases(:,ik),1,P_Factor,SC_bands)
#endif
   !
   ! Gf_{n,m}(T+dt) = G_{n,m}(T) * exp(-dt(E_{n}-E_{n'}))
   !
   forall(i=1:SC_bands,j=1:SC_bands) &
&    G_lesser(i,j,ik)=G_l_old(i,j,ik)*P_Factor(i,j)
   !
   ! Put the correct limit for E_eh --> 0
   !
   P_Factor(:,:)=(0._SP,1._SP)*RT_step
   !
   do i_pole=1,n_poles
     !     
     if(X_poles_tab(i_pole,1)/=ik) cycle
     !
     iv=X_poles_tab(i_pole,2)
     ic=X_poles_tab(i_pole,3)
     P_factor(iv,ic)=(1.0-P_Factor(iv,ic))/X_poles(i_pole)
     P_factor(ic,iv)=(1.0-P_Factor(ic,iv))/-X_poles(i_pole)
     !
   enddo
   !
   ! G(T+dt) = G(T+dt) - (1 - exp(-i dt (E_n-E_m))/(E_n-E_m) * (I1(T) - I2(T))
   !
   forall(i=1:SC_bands,j=1:SC_bands) &
&    G_lesser(i,j,ik)=G_lesser(i,j,ik)-P_factor(i,j)*(I1_lesser(i,j)-I2_lesser(i,j))
   !                                                            
 elseif(trim(int_type)=='left')  then
 elseif(trim(int_type)=='right') then
 endif
end subroutine RT_Integrator

