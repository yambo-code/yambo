!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine excitons_sort_and_report(BS_R,BS_E,BS_E_SOC_corr,EXC_spin)
 !
 use pars,          ONLY:SP,schlen,pi
 use units,         ONLY:HA2EV
 use electrons,     ONLY:spin_occ,spin,E_SOC_nbands,n_sp_pol,n_spinor
 use stderr,        ONLY:intc,set_real_printed_length
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,BSS_n_eig
 use com,           ONLY:msg,of_open_close
 use R_lattice,     ONLY:d3k_factor,q0_def_norm
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,atom_pos,Z_species
 use vec_operate,   ONLY:sort
 use YPPm,          ONLY:EXCITONS_user_indexes,EXCITON_spin
 !
 implicit none 
 !
 complex(SP)                  :: BS_R(BSS_n_eig),BS_E(BSS_n_eig)
 real(SP),pointer             :: BS_E_SOC_corr(:,:)
 type(EXCITON_spin),optional  :: EXC_spin
 !
 ! Work Space
 !
 integer            :: j1,j2,ia,is,i_mod,ic,n_elements,i_S,i_M,S_indx(BSS_n_eig)
 real(SP)           :: Residuals(BSS_n_eig),v2sort(BSS_n_eig),Rmax,rv(20)
 character(schlen)  :: titles(20),ch_dummy,dump_ch
 logical            :: write_widths,write_soc_corr,write_spin,E_mode,I_mode
 !
 ! Excitonc amplitude
 !
 character(3), parameter:: R_normalize="yes"
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 write_spin    = present(EXC_spin)
 write_widths  = any(abs(aimag(BS_E))>1.E-5)
 write_soc_corr= (E_SOC_nbands/=0)
 !
 dump_ch=" "
 if (write_spin)     dump_ch=trim(dump_ch)//" Spin"
 if (write_widths)   dump_ch=trim(dump_ch)//" Im[E_lambda]"
 if (write_soc_corr) dump_ch=trim(dump_ch)//" SOC-corr"
 if (len_trim(dump_ch)>0) dump_ch="vs "//trim(dump_ch)
 !
 call section('=','Reporting sorted Energies and Strengths '//trim(dump_ch))
 !
 do i_mod=1,2
   !
   E_mode=i_mod==1
   I_mode=i_mod==2
   !
   if (E_mode) then
     ch_dummy='exc_E_sorted'
     if (write_spin) ch_dummy='exc_E+spin_sorted'
   else if (I_mode) then
     ch_dummy='exc_I_sorted'
     if (write_spin) ch_dummy='exc_I+spin_sorted'
   endif
   !
   call of_open_close(ch_dummy,'ot')
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
     n_elements=n_elements+1
     titles(n_elements)='<S>'
     if (n_spinor==2) then
       n_elements=n_elements+1
       titles(n_elements)='<S_z>'
     endif
     do i_S=0,1
       do i_M=-i_S,i_S
         if (n_sp_pol==2.and.i_M/=0) cycle
         n_elements=n_elements+1
         titles(n_elements)='|S='//trim(intc(i_S))//' M='//trim(intc(i_M))//'>'
       enddo
     enddo
   endif
   if (write_soc_corr) then
     n_elements=n_elements+2
     titles(n_elements-1)='dE(1)[meV]'
     titles(n_elements  )='dE(2)[meV]'
   endif
   !
   Residuals(:) = BS_R(:)*conjg(BS_R(:))
   if (write_widths)  Residuals(:) = abs(BS_R(:))
   Residuals(:) = Residuals(:)*real(spin_occ,SP)/(2._SP*pi)**3*d3k_factor*4._SP*pi/q0_def_norm**2*HA2EV
   Rmax = maxval(Residuals)
   !
   if (trim(R_normalize)=='yes') Residuals=Residuals/Rmax
   !
   if (E_mode) v2sort=real(BS_E,SP)
   if (I_mode) v2sort=Residuals
   !
   call sort(arrin=v2sort,indx=S_indx)
   !
   write(dump_ch,'(a,1e10.5)') " Maximum Residual Value = ",Rmax   
   call msg('o sort','#',dump_ch,USE_TABS=.TRUE.)
   call msg('o sort','#','',USE_TABS=.TRUE.)
   !
   call msg('o sort','#',titles(:n_elements),INDENT=0,USE_TABS=.true.)
   call msg('o sort','#','',INDENT=0)
   !
   do j1=BSS_n_eig,1,-1
     !
     if (E_mode) j2=S_indx(BSS_n_eig-j1+1)
     if (I_mode) j2=S_indx(j1)
     !
     rv(:3)=(/real(BS_E(j2),SP)*HA2EV,Residuals(j2),real(j2,SP)/)
     !
     n_elements=3
     if (write_widths) then
       n_elements=n_elements+1
       rv(n_elements)=abs(aimag(BS_E(j2)))*HA2EV*1000._SP
     endif
     if (write_spin) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+1
       rv(n_elements)=(-1.+sqrt(1.+4*EXC_spin%S_sq(j2)))/2.
       if (n_spinor==2) then
         n_elements=n_elements+1
         rv(n_elements)=EXC_spin%S_z(j2)
       endif
       do i_S=0,1
         do i_M=-i_S,i_S
           if (n_sp_pol==2.and.i_M/=0) cycle
           n_elements=n_elements+1
           rv(n_elements)=EXC_spin%N_SM(j2,i_S,i_M)
         enddo
       enddo
     endif
     if (write_soc_corr) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+2
       rv(n_elements-1:n_elements)=(/BS_E_SOC_corr(1,j2),BS_E_SOC_corr(2,j2)/)*HA2EV*1000._SP
     endif
     !
     call msg('o sort','',rv(:n_elements),INDENT=-2,USE_TABS=.TRUE.)
     !
   enddo
   !
   call of_open_close(ch_dummy)
   !
 enddo
 !
 call set_real_printed_length( )
 !
end subroutine
