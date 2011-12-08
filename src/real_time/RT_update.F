!
! Copyright (C) 2000-2011 A. Marini, C. Attaccalite and the YAMBO team 
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
subroutine RT_update(G_input,SC_R,it,Xen,k,Xk,X,up_Gref,up_DIP,an_QP)
 !
 ! This subroutine takes as input the rotation matrix SC_R,
 ! its eigenvalues Xen%E and occupations Xen%f
 ! to update G_lesser_reference,Dipoles and anlize quasi-paricles
 !
 use pars,       ONLY:SP
 use units,      ONLY:HA2EV 
 use com,        ONLY:msg
 use stderr,     ONLY:set_real_printed_length
 use SC,         ONLY:SC_bands,E_bare,H_rotate,SC_nbands
 use R_lattice,  ONLY:bz_samp
 use QP_m,       ONLY:QP_nk
 use electrons,  ONLY:levels,n_sp_pol
 use real_time,  ONLY:RT_diagonalization_steps,Occ_natural_orb,Thermal_steps,rho_reference,&
&                     full_rho 
 use wrapper,    ONLY:Vstar_dot_V,M_by_V
 use X_m,        ONLY:X_t
 !
 implicit none
 !
 type(X_t),      intent(inout)        :: X
 complex(SP),    intent(inout)        :: G_input(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),QP_nk)
 complex(SP) ,   intent(in)           :: SC_R(SC_bands(2),SC_bands(2),QP_nk,n_sp_pol)
 type(bz_samp),  intent(in)           :: k,Xk
 integer,        intent(in)           :: it
 type(levels),   intent(inout)        :: Xen
 logical,        optional, intent(in) :: up_Gref,up_DIP,an_QP
 !
 ! Work Space
 !
 integer :: ik,ib
 complex(SP) :: tmp_v(SC_bands(2))
 !
 ! Update Dipoles
 !
 if(present(up_DIP)) call SC_update_dipoles(SC_R,Xen,Xk,X)
 !
 ! Anlize quasi-particles
 ! 
 if(present(an_QP).and.(mod(it,RT_diagonalization_steps)==0.or.it==Thermal_steps)) then
   !
   call set_real_printed_length()
   !
   do ik=1,QP_nk
     !
     ! natural orbitals
     !
     do ib=1,SC_bands(1)-1
       Occ_natural_orb(ib,ik)=cmplx(2._SP,0._SP)
     enddo
     do ib=SC_bands(1),SC_bands(2)
       call M_by_V('N',SC_nbands,SC_nbands,(0._SP,-1._SP),G_input(:,:,ik),SC_nbands,SC_R(SC_bands(1):SC_bands(2),ib,ik,1), &
&                  1,(0._SP,0._SP),tmp_v,1)
       Occ_natural_orb(ib,ik)=real(Vstar_dot_V(SC_nbands,SC_R(SC_bands(1):SC_bands(2),ib,ik,1),tmp_v))
     enddo
     !
   enddo
   !
   call OCCUPATIONS_Fermi(Xen,k,0)
   Xen%E(:SC_bands(2),:,:)=Xen%E(:SC_bands(2),:,:)-Xen%Efermi(1)
   !
   if(it==Thermal_steps) then
     call msg('r',' ')
     call msg('r','Natural (O)ccupations, (E)nergies and (d_E) E-Eo.')
     do ik=1,QP_nk
       call REPORT_Energies(Xen%E(:SC_bands(2),:,:),k,Xk,SC_bands(2),(/ik,ik/),'E  ',.TRUE.)
       call REPORT_Energies(E_bare,k,Xk,SC_bands(2),(/ik,ik/),'E_o  ',.FALSE.)
       call REPORT_Energies(Xen%E(:SC_bands(2),:,:)-E_bare,k,Xk,SC_bands(2),(/ik,ik/),'d_E',.FALSE.)
       call REPORT_Energies(Occ_natural_orb(:SC_bands(2),:)/HA2EV,k,Xk,SC_bands(2),(/ik,ik/),'O  ',.FALSE.)
     enddo
   endif
   !
   call set_real_printed_length(f_length=15,g_length=15)
   !
   ! History
   !
   call  SC_History(it,Xen)    
   !
 endif
 !
 ! Update G_lesser and rho_reference
 !
 if(present(up_Gref)) then
   !
   G_input=(0._SP,0._SP)
   !
   do ik=1,Xen%nk
     !
     forall(ib=SC_bands(1):SC_bands(2))
       G_input(ib,ib,ik)=(0._SP,1._SP)*Xen%f(ib,ik,1)
     end forall   
     !
     call H_rotate(SC_R(SC_bands(1):SC_bands(2),SC_bands(1):SC_bands(2),ik,1),G_input(:,:,ik),SC_nbands,-1)
    !
  enddo
  !
  ! Update the full density and the one between SC_lw_band:SC_bands(2)
  !
  if(allocated(rho_reference)) call el_density(G_input,Xen,Xk,rho_reference,SC_bands(1),.false.)  
  if(allocated(full_rho))      call el_density(G_input,Xen,Xk,full_rho     ,1         ,.false.)  
  !
 endif 
 !
end subroutine RT_update
