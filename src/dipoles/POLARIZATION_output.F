!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA

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
subroutine POLARIZATION_output(Xen,Xk,Dip)
 ! 
 ! Print the intrisic Polarization of the system
 !
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:e_SI,AU2M
 use electrons,     ONLY:levels,n_sp_pol
 use R_lattice,     ONLY:bz_samp,k_map
 use com,           ONLY:msg
 use D_lattice,     ONLY:DL_vol,a,n_atomic_species,Z_species
 use DIPOLES,       ONLY:DIPOLE_t
 use vec_operate,   ONLY:get_id_perpendicular
 use stderr,        ONLY:intc
 !
 implicit none
 type(levels)  ::Xen
 type(bz_samp) ::Xk
 type(DIPOLE_t)::Dip
 ! 
 ! Work Space
 !
 type(bz_samp)::k,q
 complex(SP) :: P_Berry_RED(3)  ! In reduced (crystaline) coordinated 
 real(SP)    :: I_Berry_RED(3),Tot_Berry_RED(3)  ! In reduced (crystaline) coordinated 
 real(SP)    :: modfac,rmod
 complex(SP) :: All_S_det(6,Xk%nbz,n_sp_pol)
 complex(SP) :: Sm1_plus (Xen%nbm,Xen%nbm,6)
 complex(SP) :: Sm1_minus(Xen%nbm,Xen%nbm,6)
 character(lchlen) :: dumb_ch
 integer           :: id,Nperpend,id2,id3,ia,mod_tot,ikbz,i_sp_pol
 logical           :: l_odd
 !
 ! Check for atoms with odd number of electrons
 !
 l_odd=.false.
 do ia=1,n_atomic_species
   if(mod(Z_species(ia),2) == 1) l_odd=.true.
 enddo
 !                                         
 do i_sp_pol=1,n_sp_pol
   do ikbz=1,Xk%nbz
     call Build_Overlaps_Det_EQ(Xen,i_sp_pol,ikbz,Sm1_plus,Sm1_minus,All_S_det(:,ikbz,i_sp_pol))
   enddo
 enddo
 !
 ! Calculate Electronic polarization
 !
 call Berry_polarization_EQ(P_Berry_RED,All_S_det,Xen,Xk,k_map)
 !
 ! Calculate Electronic polarization and set l_odd
 !
 call Ionic_polarization(I_berry_RED,Xen)
 !
 Tot_berry_RED=real(P_berry_RED,SP)+I_berry_RED
 !
 mod_tot=2
 if(l_odd) mod_tot=1
 !
 Tot_Berry_RED=Tot_Berry_RED-real(mod_tot)*NINT(Tot_Berry_RED/real(mod_tot)) 
 !
 ! Write Output in the report
 !
 call msg('r','')
 call msg('r',' * * * Polarization * * * ')
 call msg('r','==========================')
 !
 write(dumb_ch,'(a,f16.8,a)') ' Volume : ',DL_vol,' bohr^3 '
 call msg('r',dumb_ch)
 ! 
 do id=1,3
   !
   rmod=sqrt(sum(a(id,:)*a(id,:)))
   !
   if(k_map%max_kdir(id)==1) cycle
   call get_id_perpendicular(id,id2,id3)
   Nperpend=k_map%max_kdir(id2)*k_map%max_kdir(id3)
   call msg('r',' Direction: '//intc(id))
   call msg('r',' Numer of k-points per string : '//intc(k_map%max_kdir(id)))
   call msg('r',' Numer of different strings   : '//intc(k_map%max_kdir(id2)*k_map%max_kdir(id3)))
   write(dumb_ch,'(a,f16.8,a,i1.1,a)') ' Electronic Phase : ',real(P_Berry_RED(id),SP),'   mod(',2,')'
   call msg('r',dumb_ch)
   write(dumb_ch,'(a,f16.8,a,i1.1,a)') ' Ionic      Phase : ',I_Berry_RED(id)  ,'   mod(',mod_tot,')'
   call msg('r',dumb_ch)
   write(dumb_ch,'(a,f16.8,a,i1.1,a)') ' Total      Phase : ',Tot_Berry_RED(id),'   mod(',mod_tot,')'
   call msg('r',dumb_ch)
   modfac=rmod
   write(dumb_ch,'(a,f16.8,a,f11.8,a)') ' Polarization : ',Tot_Berry_RED(id)*modfac,& 
&       '  mod(',real(mod_tot)*modfac,') (e/Volume).bohr'
   call msg('r',dumb_ch)
   modfac=rmod/DL_vol
   write(dumb_ch,'(a,f16.8,a,f11.8,a)') ' Polarization : ',Tot_Berry_RED(id)*modfac,&
&       '  mod(',real(mod_tot)*modfac,') e/bohr^2'
   call msg('r',dumb_ch)
   modfac=(rmod/DL_vol)*(e_SI/AU2M**2)
   write(dumb_ch,'(a,f16.8,a,f11.8,a)') ' Polarization : ',Tot_Berry_RED(id)*modfac,&
&       '  mod(',real(mod_tot)*modfac,') C/m^2'
   call msg('r',dumb_ch)
   call msg('r','')
 enddo
 !
 if(l_odd) call msg('r',' The total polarization has module 1 !! ')
 call msg('r','')
 !
end subroutine POLARIZATION_output
