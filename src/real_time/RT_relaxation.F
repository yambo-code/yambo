!
! Copyright (C) 2000-2013 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_relaxation(G_input,E,k,q,Time)
 !
 ! This subroutine calculate relaxation term coming from off-diagonal part
 ! of the self-energy in time, from the electron-phonon coupling or simple 
 ! radiative and dephasing term in the kinetic equation 
 !
 ! Output: Sigma_relax
 !
 use pars,           ONLY:SP,cI,cZERO
 use electrons,      ONLY:levels
 use drivers,        ONLY:l_ph_corr,l_carrier_dynamics 
 use R_lattice,      ONLY:bz_samp
 use SC,             ONLY:SC_bands
 use QP_m,           ONLY:QP_nk
 use parallel_m,     ONLY:PAR_IND_Xk_ibz,PAR_COM_Xk_ibz_INDEX,PP_redux_wait,master_cpu
 use real_time,      ONLY:Sigma_relax,G_lesser_reference,RT_is_dumped,NE_dynamics,RT_Deph_Matrix
 !
 implicit none
 !
 type(levels),  intent(in)    :: E
 type(bz_samp), intent(in)    :: k,q
 real(SP),      intent(in)    :: Time
 complex(SP),   intent(in)    :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 !
 ! Work Space
 !
 integer          :: i1,i2,ik
 !
 ! Damping
 !=========
 !
 ! Fictitious lifetime equivalent to the broadening
 ! in linear optics
 !   
 ! This term kill the induced polarization, namely off-diagonal term in the
 ! Green function
 !
 ! This term send the electron distribution into the initial one
 ! G_lesser_refence(:,:,:) 
 !
 if(.not.RT_is_dumped.and..not.l_ph_corr.and..not.l_carrier_dynamics) return 
 !
 Sigma_relax=cZERO
 !
 ! El-El
 !=======
 if (l_carrier_dynamics) then
   !
   do ik=1,QP_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_Coulomb_relaxation(ik,E,k,q) 
     !
   enddo
   !
 endif
 !
#if defined _ELPH
 !
 ! El-Ph
 !=======
 if (l_ph_corr) then
   !
   do ik=1,QP_nk
     !
     if (.not.PAR_IND_Xk_ibz%element_1D(ik)) cycle
     !
     call RT_ELPH_total_Kernel(ik,E,k,q,Time) 
     !
   enddo
   !
 endif
 !
#endif
 !
 ! PARALLEL Mergering
 !====================
 !
 ! I make copies of the k-resolved matrix. In the future a completely independet
 ! k loop could be implemented
 !
 if (l_ph_corr.or.l_carrier_dynamics) then
   do ik=1,QP_nk
     call PP_redux_wait(Sigma_relax(:,:,ik),COMM=PAR_COM_Xk_ibz_INDEX%COMM)
   enddo
 endif
 !
 !DEBUG>
 !write (*,*)
 !if (master_cpu) then
 !do i1=SC_bands(1),SC_bands(2)
 !  write  (*,'(i3,10f10.5)') i1,real(Sigma_relax(i1,i1,:10))*1000.
 !enddo
 !endif
 !stop
 !DEBUG<
 !
 ! Dephasing Matrix
 !==================
 !
 if(RT_is_dumped.and.NE_dynamics) then
   !
   forall(i1=SC_bands(1):SC_bands(2),i2=SC_bands(1):SC_bands(2)) 
     Sigma_relax(i1,i2,:)=Sigma_relax(i1,i2,:)-cI*RT_Deph_Matrix(i1,i2,:)*(G_input(i1,i2,:)-G_lesser_reference(i1,i2,:))
   end forall
   !
 endif
 !
end subroutine RT_relaxation

