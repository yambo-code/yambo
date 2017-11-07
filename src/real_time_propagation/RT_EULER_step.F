!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine RT_EULER_step(dG_old,dG_inp,dG_out,ik,deltaT)
 !
 use pars,           ONLY:SP,cZERO,cI
 use real_time,      ONLY:l_RT_PERT
 use wrapper_omp,    ONLY:M_by_M_omp
 use real_time,      ONLY:Ho_plus_Sigma,RT_bands,RT_nbands,RT_nk, &
&                         G_lesser_reference
 !
 implicit none
 !
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: deltaT
 complex(SP), intent(in)  :: dG_old(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(in)  :: dG_inp(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP), intent(out) :: dG_out(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! Workspace
 !
 complex(SP)  :: I1_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: I2_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 complex(SP)  :: G_k(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 integer      :: ib
 !
 ! I1 = Ho_plus_Sigma*G   
 ! I2 = G*Ho_plus_Sigma
 !
 if (.not.l_RT_PERT) G_k=dG_inp+G_lesser_reference(:,:,ik)
 if (     l_RT_PERT) G_k=       G_lesser_reference(:,:,ik)
 !
 call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),G_k,I1_k)
 call M_by_M_omp('n','n',RT_nbands,G_k,Ho_plus_Sigma(:,:,ik,1),I2_k)
 !
 ! dG_out = G(T0+dt) -G(T0)= -i dt * ( I1(T0)-I2(T0) )
 !
 dG_out=-cI*deltaT*(I1_k-I2_k)
 !
 if (.not.l_RT_PERT) return
 !
 I1_k=cZERO
 I2_k=cZERO
 G_k=dG_inp+G_lesser_reference(:,:,ik)
 !
 call M_by_M_omp('n','n',RT_nbands,Ho_plus_Sigma(:,:,ik,1),G_k,I1_k)
 call M_by_M_omp('n','n',RT_nbands,G_k,Ho_plus_Sigma(:,:,ik,1),I2_k)
 !
 do ib=RT_bands(1),RT_bands(2)
   dG_out(ib,ib)=-cI*deltaT*(I1_k(ib,ib)-I2_k(ib,ib))
 enddo
 !
end subroutine RT_EULER_step
