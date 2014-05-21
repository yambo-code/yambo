!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine RT_DEPHASING_step(G_new_k,ik,T,deltaT)
 !
 use pars,           ONLY:SP,cI
 use electrons,      ONLY:spin_occ
 use SC,             ONLY:SC_bands
 use real_time,      ONLY:I_relax_static,I_relax_dyn,Life_MEM_steps,SAVED_times
 !
 implicit none
 !
 complex(SP), intent(out) :: G_new_k(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2))
 integer,     intent(in)  :: ik
 real(SP),    intent(in)  :: T,deltaT
 !
 ! Work Space
 !
 integer :: ib,i_n
 !
 ! Dephasing is composed of a static and a dynamical part
 !=======================================================
 !
 ! STATIC: dG = dG - i dT Sigma
 !------------------------------
 !
 if (allocated(I_relax_static)) G_new_k = G_new_k - cI*deltaT*I_relax_static(:,:,ik)
 !
 ! DYNAMICAL: only on the diagonal
 !---------------------------------
 !
 if (allocated(I_relax_dyn)) then
   do ib=SC_bands(1),SC_bands(2)
     do i_n=1,Life_MEM_steps
       G_new_k(ib,ib) = G_new_k(ib,ib) - cI*(I_relax_dyn(ib,ik,1,i_n)+I_relax_dyn(ib,ik,2,i_n))*&
&                       ((T+deltaT-SAVED_times(1))**i_n-(T-SAVED_times(1))**i_n)/real(i_n)
     enddo
   enddo
 endif
 !
end subroutine RT_DEPHASING_step
