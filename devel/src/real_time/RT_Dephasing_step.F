!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS,AM
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
subroutine RT_Dephasing_step(G_new_k,G_old_k,ik,deltaT)
 !
 use pars,           ONLY:SP,cI
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:RT_is_dephased,RT_Deph_Matrix,RT_all2ibz,l_RT_exp
 !
 implicit none
 !
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 complex(SP), intent(in)  :: G_old_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 !
 real(SP) :: GAMMA_e,GAMMA_h,dGAMMA_e,dGAMMA_h,GAMMA_TOT,f_inf,f0
 integer  :: ib1,ib2
 !
 ! dG = dG - i dT Sigma
 !======================
 if (.not.RT_is_dephased) return
 !
 if(l_RT_exp) then
   !
   do ib1=SC_bands(1),SC_bands(2)
     do ib2=ib1+1,SC_bands(2)
       !
       G_new_k(ib1,ib2) = G_new_k(ib1,ib2)*exp(-RT_Deph_Matrix(ib1,ib2,RT_all2ibz(ik))*deltaT)
       G_new_k(ib2,ib1) = G_new_k(ib2,ib1)*exp(-RT_Deph_Matrix(ib2,ib1,RT_all2ibz(ik))*deltaT)
       !
     enddo
     !
   enddo
   !
 else
   !
   do ib1=SC_bands(1),SC_bands(2)
     do ib2=ib1+1,SC_bands(2)
       !
       G_new_k(ib1,ib2) = ( G_new_k(ib1,ib2) + G_old_k(ib1,ib2) ) * (1._SP - deltaT*RT_Deph_Matrix(ib1,ib2,RT_all2ibz(ik))) 
       G_new_k(ib2,ib1) = ( G_new_k(ib2,ib1) + G_old_k(ib2,ib1) ) * (1._SP - deltaT*RT_Deph_Matrix(ib2,ib1,RT_all2ibz(ik)))
       !
     enddo
     !
     G_new_k(ib1,ib1) = G_new_k(ib1,ib1) + G_old_k(ib1,ib1)
     !
   enddo
   !
 endif
 !
end subroutine RT_Dephasing_step
