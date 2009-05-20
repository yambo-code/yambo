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
subroutine RT_Ext_Field(ik,Sigma_s,time)
 !
 use pars,           ONLY:SP,DP
 use com,            ONLY:error
 use real_time,      ONLY:EF_q0
 use X_m,            ONLY:DIP_iR_or_P
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:Ext_Field_freq,Ext_Field_Int,Ext_Field_name,&
&                         RT_resonant,RT_no_intraband,T_initial,&
&                         A_norm,I_time,A_vec_pot
 implicit none
 integer,     intent(in)    :: ik
 real(SP),    intent(in)    :: time
 complex(SP), intent(inout) :: Sigma_s(SC_bands,SC_bands)
 ! 
 ! Work Space
 !
 integer     :: i1,i2
 complex(DP) :: A_dot_P
 !
 select case(trim(Ext_Field_name))
   case('SINUSOIDAL')
     if(RT_resonant) then
       A_norm(I_time)=-Ext_Field_Int*exp(-(0._SP,1._SP)*Ext_Field_freq*time)/((0._SP,2._SP)*Ext_Field_freq)
     else
       A_norm(I_time)=-Ext_Field_Int*sin(Ext_Field_freq*time)/Ext_Field_freq
     endif
   case('DELTA')
     if(time>=T_initial) then
       A_norm(I_time)=Ext_Field_Int
     else
       A_norm(I_time)=(0.,0.)
     endif
   case('STEP')
     if(time>=T_initial) then
       A_norm(I_time)=Ext_Field_Int*time
     else
       A_norm(I_time)=(0.,0.) 
     endif
   case default
     call error(" External Field "//trim(Ext_Field_name)//" not implemented ") 
 end select
 !
 A_vec_pot(I_time,:)=EF_q0(:)*A_norm(I_time)
 !
 !
 ! Add the External Field to the singular part of the Self-Energy
 ! in case of complex Efield puts the correct Hermitian matrix
 ! and the intraband optical matrix elements have been set to zero
 !
 if(.not.RT_no_intraband.or..not.RT_resonant) then
   do i1=1,SC_bands
     Sigma_s(i1,i1)=Sigma_s(i1,i1)+sum(A_vec_pot(I_time,:)*DIP_iR_or_P(:,i1,i1,ik,1))
   enddo
 endif
 !
 do i1=1,SC_bands
   do i2=i1+1,SC_bands
      !     
      A_dot_P=sum(A_vec_pot(I_time,:)*DIP_iR_or_P(:,i1,i2,ik,1))
      Sigma_s(i1,i2)=Sigma_s(i1,i2)+A_dot_P
      Sigma_s(i2,i1)=Sigma_s(i2,i1)+conjg(A_dot_P)
      !
   enddo
 enddo
 !
end subroutine RT_Ext_Field
