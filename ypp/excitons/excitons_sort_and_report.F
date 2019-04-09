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
subroutine excitons_sort_and_report(E,BS_R,BS_E,BS_E_SOC_corr,Singlet,Triplet,S_sh)
 !
 use pars,          ONLY:SP,schlen,pi
 use units,         ONLY:HA2EV
 use electrons,     ONLY:spin_occ,levels,spin,E_SOC_nbands,n_sp_pol
 use stderr,        ONLY:intc,set_real_printed_length
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,BSS_n_eig
 use com,           ONLY:msg,of_open_close
 use R_lattice,     ONLY:d3k_factor,q0_def_norm
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species,atom_pos,Z_species
 use vec_operate,   ONLY:sort
 use YPP,           ONLY:EXCITONS_user_indexes
 !
 implicit none 
 !
 type(levels)       :: E
 complex(SP)        :: BS_R(BSS_n_eig),BS_E(BSS_n_eig)
 real(SP),pointer   :: BS_E_SOC_corr(:,:)
 real(SP),optional  :: Singlet(BSS_n_eig)
 real(SP),optional  :: Triplet(BSS_n_eig,3)
 real(SP),optional  :: S_sh(BSS_n_eig)
 !
 ! Work Space
 !
 integer            :: j1,j2,ia,is,i_m,ic,n_elements
 integer            :: S_indx(BSS_n_eig)
 real(SP)           :: Residuals(BSS_n_eig),v2sort(BSS_n_eig),Rmax,rv(10)
 character(schlen)  :: titles(7),ch_dummy,dump_ch
 logical            :: write_widths,write_S_sh,write_soc_corr,write_Singlet,E_mode,I_mode,write_Triplet
 !
 ! Excitonc amplitude
 !
 character(3), parameter:: R_normalize="yes"
 !
 call set_real_printed_length(f_length=15,g_length=15)
 !
 write_Singlet = present(Singlet)
 write_Triplet = present(Triplet)
 write_S_sh    = present(S_sh)
 write_widths  = any(abs(aimag(BS_E))>1.E-5)
 write_soc_corr= (E_SOC_nbands/=0)
 !
 dump_ch=" "
 if (write_Singlet)  dump_ch=trim(dump_ch)//" Singlet"
 if (write_Triplet)  dump_ch=trim(dump_ch)//" Triplet"
 if (write_S_sh)     dump_ch=trim(dump_ch)//" S_sh"
 if (write_widths)   dump_ch=trim(dump_ch)//" Im[E_lambda]"
 if (write_soc_corr) dump_ch=trim(dump_ch)//" SOC-corr"
 if (len_trim(dump_ch)>0) dump_ch="vs "//trim(dump_ch)
 !
 call section('=','Reporting sorted Energies and Strengths '//trim(dump_ch))
 !
 do i_m=1,2
   !
   E_mode=i_m==1
   I_mode=i_m==2
   !
   if (E_mode) then
     ch_dummy='exc_E_sorted'
     if (write_S_sh.or.write_Singlet) ch_dummy='exc_E+spin_sorted'
   else if (I_mode) then
     ch_dummy='exc_I_sorted'
     if (write_S_sh.or.write_Singlet) ch_dummy='exc_I+spin_sorted'
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
   if (write_Singlet) then
     n_elements=n_elements+1
     titles(n_elements)='|S=0,M=0>'
   endif
   if (write_Triplet) then
     do ic=1,3
       if (n_sp_pol==2.and.(ic==1.or.ic==3)) cycle
       n_elements=n_elements+1
       if (ic==1) titles(n_elements)='|S=1,M=-1>'
       if (ic==2) titles(n_elements)='|S=1,M= 0>'
       if (ic==3) titles(n_elements)='|S=1,M= 1>'
     enddo
   endif
   if (write_S_sh) then
     n_elements=n_elements+1
     titles(n_elements)='S_sh'
   endif
   if (write_soc_corr) then
     n_elements=n_elements+2
     titles(n_elements-1)='dE(1)[meV]'
     titles(n_elements  )='dE(2)[meV]'
   endif
   !
   Residuals(:) = BS_R(:)*conjg(BS_R(:))
   if (write_widths)  Residuals(:) = abs(BS_R(:))
   Residuals(:) = Residuals(:)*real(spin_occ,SP)/(2._SP*pi)**3*&
&         d3k_factor*4._SP*pi/q0_def_norm**2*HA2EV
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
     if (write_Singlet) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+1
       rv(n_elements)=Singlet(j2)
     endif
     if (write_Triplet) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       do ic=1,3
         if (n_sp_pol==2.and.(ic==1.or.ic==3)) cycle
         n_elements=n_elements+1
         rv(n_elements)=Triplet(j2,ic)
       enddo
     endif
     if (write_S_sh) then
       if (.not.any((/EXCITONS_user_indexes==j2/))) cycle
       n_elements=n_elements+1
       rv(n_elements)=S_sh(j2)
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
