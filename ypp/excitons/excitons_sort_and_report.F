!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DV
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
subroutine excitons_sort_and_report(E,BS_R,BS_E,BS_E_SOC_corr,S_z,S_sq)
 !
 use pars,          ONLY:SP,schlen,pi
 use units,         ONLY:HA2EV
 use electrons,     ONLY:spin_occ,levels,spin,E_SOC_nbands
 use stderr,        ONLY:intc
 use BS,            ONLY:BS_H_dim
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,BSS_n_eig
 use com,           ONLY:msg,of_open_close
 use R_lattice,     ONLY:d3k_factor,q0_def_norm
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,atom_pos,Z_species
 use vec_operate,   ONLY:sort
 !
 implicit none 
 !
 type(levels)       :: E
 complex(SP)        :: BS_R(BSS_n_eig),BS_E(BSS_n_eig)
 real(SP),pointer   :: BS_E_SOC_corr(:,:)
 real(SP),optional  :: S_z(BSS_n_eig)
 real(SP),optional  :: S_sq(BSS_n_eig)
 !
 ! Work Space
 !
 integer            :: j1,j2,ia,is,i_mode,ic,n_elements
 integer            :: S_indx(BSS_n_eig)
 real(SP)           :: Residuals(BSS_n_eig),v2sort(BSS_n_eig),Rmax,rv(10)
 character(schlen)  :: titles(7),ch_dummy(2)
 logical            :: write_widths,write_spin,write_soc_corr
 !
 ! Excitonc amplitude
 !
 character(3), parameter:: R_normalize="yes"
 !
 write_spin    = present(S_sq)
 write_widths  = any(abs(aimag(BS_E))>1.E-5)
 write_soc_corr= (E_SOC_nbands/=0)
 !
 if (     write_widths) call section('=','Reporting sorted Energies, Strengths and widths')
 if (.not.write_widths) call section('=','Reporting sorted Energies and Strengths')
 !
 do i_mode=1,2
   !
   if (i_mode==1) ch_dummy(1)='exc_E_sorted'
   if (i_mode==2) ch_dummy(1)='exc_I_sorted'
   if (write_spin) then
     if (i_mode==1) ch_dummy(1)='exc_E+spin_sorted'
     if (i_mode==2) ch_dummy(1)='exc_I+spin_sorted'
   endif
   !
   call of_open_close(ch_dummy(1),'ot')
   !
   do j1=1,BSS_n_descs
     call msg('o sort',"#",trim(BSS_description(j1)),INDENT=0)
   enddo
   call msg('o sort',"#")
   do is=1,n_atomic_species
     do ia=1,n_atoms_species(is)
       call msg('o sort',"# Atom "//trim(intc(ia))//&
&                        " with Z "//trim(intc(Z_species(is)))//" [cc]:",&
&               atom_pos(:,ia,is),INDENT=0)
     enddo
   enddo
   call msg('o sort','#','',INDENT=0) 
   !
   n_elements=3
   titles(1)='E [ev]'
   titles(2)='Strength'
   titles(3)='Index'
   if (write_widths) then
     n_elements=n_elements+1
     titles(4)='W [meV]'
   endif
   if (write_spin) then
     n_elements=n_elements+2
     titles(n_elements-1)='S_z'
     titles(n_elements  )='S_square'
   endif
   if (write_soc_corr) then
     n_elements=n_elements+2
     titles(n_elements-1)='dE(1)[meV]'
     titles(n_elements  )='dE(2)[meV]'
   endif
   call msg('o sort','#',titles(:n_elements),INDENT=0,USE_TABS=.true.)
   call msg('o sort','#','',INDENT=0)
   !
   Residuals(:) = BS_R(:)*conjg(BS_R(:))
   if (write_widths)  Residuals(:) = abs(BS_R(:))
   Residuals(:) = Residuals(:)*real(spin_occ,SP)/(2._SP*pi)**3*&
&         d3k_factor*4._SP*pi/q0_def_norm**2*HA2EV
   Rmax = maxval(Residuals)
   !
   if (trim(R_normalize)=='yes') Residuals=Residuals/Rmax
   !
   if (i_mode==1) v2sort=real(BS_E,SP)
   if (i_mode==2) v2sort=Residuals
   !
   call sort(arrin=v2sort,indx=S_indx)
   !
   do j1=BSS_n_eig,1,-1
     !
     if (i_mode == 1) j2=S_indx(BSS_n_eig-j1+1)
     if (i_mode == 2) j2=S_indx(j1)
     !
     rv(:3)=(/real(BS_E(j2),SP)*HA2EV,Residuals(j2),real(j2,SP)/)
     !
     n_elements=3
     if (write_widths) then
       n_elements=n_elements+1
       rv(n_elements)=abs(aimag(BS_E(j2)))*HA2EV*1000._SP
     endif
     if (write_spin) then
       n_elements=n_elements+2
       rv(n_elements-1:n_elements)=(/S_z(j2),S_sq(j2)/)
     endif
     if (write_soc_corr) then
       n_elements=n_elements+2
       rv(n_elements-1:n_elements)=(/BS_E_SOC_corr(1,j2),BS_E_SOC_corr(2,j2)/)*HA2EV*1000._SP
     endif
     !
     call msg('o sort','',rv(:n_elements),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   call of_open_close(ch_dummy(1))
   !
 enddo
 !
end subroutine
