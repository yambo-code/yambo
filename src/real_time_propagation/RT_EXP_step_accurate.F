!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS
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
subroutine RT_EXP_step_accurate(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 ! The EXP integrator is better than the EULER integrator if Ho_plus_Sigma
 ! changes in time more slowly that G
 !
 ! e^-A B e^A = B + [A,B] + 1/2! [A,[A,B]] + 1/3! [A,[A,[A,B]]] + ...
 !
 use pars,           ONLY:SP,cI,cZERO
 use wrapper_omp,    ONLY:M_by_M_omp
 use real_time,      ONLY:Ho_plus_Sigma,RT_bands,RT_nk,RT_nbands,&
&                         integrator_exp_order,G_lesser_reference,I1_matrix
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: dG_inp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Work space
 !
 integer     :: i_order
 complex(SP) :: U1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: U2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP) :: G_comm(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 dG_out=cZERO
 !
 G_comm=dG_inp+G_lesser_reference(:,:,ik)
 !
 do i_order=1,Integrator_exp_order
   !
   call M_by_M_omp('n','n',RT_nbands,-cI*deltaT*Ho_plus_Sigma(:,:,ik,1)/real(i_order,SP),G_comm,U1_k)
   call M_by_M_omp('n','n',RT_nbands,G_comm,+cI*deltaT*Ho_plus_Sigma(:,:,ik,1)/real(i_order,SP),U2_k)
   !
   ! dG_out = G(T0+dt) -G(T0)=  U1(T0)+U2(T0) 
   !
   G_comm=U1_k+U2_k
   !
   dG_out=dG_out+G_comm
   !
 enddo
 !
end subroutine RT_EXP_step_accurate
