!
! Copyright (C) 2000-2010 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_apply_field(ik,H_nl_sc,A_input)
 !
 ! This subroutine add the vector potential to the Hamiltonian
 !
 use pars,           ONLY:SP,cI
 use units,          ONLY:SPEED_OF_LIGHT
 use real_time,      ONLY:l_RT_no_diamagnetic
 use fields,         ONLY:gauge_field
 use electrons,      ONLY:n_sp_pol
 use fields,         ONLY:global_gauge
 use X_m,            ONLY:DIP_iR,DIP_P
 use SC,             ONLY:SC_bands
 !
 implicit none
 integer,     intent(in)       :: ik
 complex(SP), intent(inout)    :: H_nl_sc(SC_bands,SC_bands,n_sp_pol)
 type(gauge_field), intent(in) :: A_input
 ! 
 ! Work Space
 !
 integer     :: i1,i2,is
 complex(SP) :: A_dot_P,E_dot_r,A_vec_pot(3),E_vec_pot(3)
 !
 if(trim(global_gauge)=='velocity') then
   !
   ! Velocity gauge
   !================
   !
   A_vec_pot=A_input%vecpot/SPEED_OF_LIGHT ! Gaussian units
   !
   ! Add the vector potential to the singular part of the Self-Energy
   ! in case of complex Efield puts the correct Hermitian matrix
   ! and the intraband optical matrix elements have been set to zero
   !
   do i1=1,SC_bands
     H_nl_sc(i1,i1,1)=H_nl_sc(i1,i1,1)-real(sum(A_vec_pot(1:3)*DIP_P(1:3,i1,i1,ik,1)),SP)
   enddo
   !
   do i1=1,SC_bands
     do i2=i1+1,SC_bands
        !     
        A_dot_P=-sum(A_vec_pot(1:3)*DIP_P(1:3,i1,i2,ik,1))
        H_nl_sc(i1,i2,1)=H_nl_sc(i1,i2,1)+A_dot_P
        H_nl_sc(i2,i1,1)=H_nl_sc(i2,i1,1)+conjg(A_dot_P)
        !
     enddo
   enddo
   !
   if(.not.l_RT_no_diamagnetic) then
     forall(i1=1:SC_bands,is=1:n_sp_pol) 
       H_nl_sc(i1,i1,is)=H_nl_sc(i1,i1,is)+0.5_SP*sum(A_vec_pot(1:3)**2)
     end forall
   endif
   !
 else  
   !
   ! Length gauge
   !===============
   !
   E_vec_pot=-A_input%vecpot_vel/SPEED_OF_LIGHT ! Gaussian units
   !
   do i1=1,SC_bands
     do i2=i1+1,SC_bands
       !     
       E_dot_r=cI*sum(E_vec_pot(1:3)*DIP_iR(1:3,i1,i2,ik,1))
       H_nl_sc(i1,i2,1)=H_nl_sc(i1,i2,1)+E_dot_r
       H_nl_sc(i2,i1,1)=H_nl_sc(i2,i1,1)+conjg(E_dot_r)
       !
     enddo
   enddo
   !
 endif
 !
end subroutine RT_apply_field
