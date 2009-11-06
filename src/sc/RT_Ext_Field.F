!
! Copyright (C) 2000-2009 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_Ext_Field(ik,H_nl_sc,G_lesser,time,Efield,A_norm)
 !
 use pars,           ONLY:SP,DP
 use com,            ONLY:error
 use real_time,      ONLY:l_no_diamagnetic,ext_field
 use electrons,      ONLY:n_sp_pol
 use X_m,            ONLY:DIP_iR_or_P
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 !
 implicit none
 integer,     intent(in)     :: ik
 real(SP),    intent(in)     :: time
 complex(SP), intent(out)    :: A_norm
 type(ext_field), intent(in) :: Efield
 complex(SP), intent(inout) :: H_nl_sc(SC_bands,SC_bands,n_sp_pol)
 complex(SP), intent(in)    :: G_lesser(SC_bands,SC_bands,QP_nk)
 ! 
 ! Work Space
 !
 integer     :: i1,i2,is
 complex(DP) :: A_dot_P,A_vec_pot(3)
 !
 if(time<Efield%t_initial) then
   A_norm=(0.,0.)
   return
 endif
 !
 select case(trim(Efield%ef_name))
   case('SINUSOIDAL')
     A_norm=-Efield%amplitude*sin(Efield%frequency*time)/Efield%frequency
   case('RESONANT')
     A_norm=-Efield%amplitude*exp(-(0._SP,1._SP)*Efield%frequency*time)/((0._SP,2._SP)*Efield%frequency)
   case('DELTA')
     A_norm=Efield%amplitude
   case('STEP')
     A_norm=Efield%amplitude*time
   case default
     call error(" External Field "//trim(Efield%ef_name)//" not implemented ") 
 end select
 !
 A_vec_pot(:)=Efield%versor(:)*A_norm
 !
 ! Add the External Field to the singular part of the Self-Energy
 ! in case of complex Efield puts the correct Hermitian matrix
 ! and the intraband optical matrix elements have been set to zero
 !
 if(trim(Efield%ef_name)/="RESONANT".or.Efield%no_intraband) then
   do i1=1,SC_bands
     H_nl_sc(i1,i1,1)=H_nl_sc(i1,i1,1)+real(sum(A_vec_pot(:)*DIP_iR_or_P(:,i1,i1,ik,1)))
   enddo
 endif
 !
 do i1=1,SC_bands
   do i2=i1+1,SC_bands
      !     
      A_dot_P=sum(A_vec_pot(:)*DIP_iR_or_P(:,i1,i2,ik,1))
      H_nl_sc(i1,i2,1)=H_nl_sc(i1,i2,1)+A_dot_P
      H_nl_sc(i2,i1,1)=H_nl_sc(i2,i1,1)+conjg(A_dot_P)
      !
   enddo
 enddo
 !
 if(.not.l_no_diamagnetic) then
   forall(i1=1:SC_bands,is=1:n_sp_pol) 
     H_nl_sc(i1,i1,is)=H_nl_sc(i1,i1,is)+0.5*A_norm**2*(0.,-1.)*G_lesser(i1,i1,ik)
   end forall
 endif
 !
end subroutine RT_Ext_Field
