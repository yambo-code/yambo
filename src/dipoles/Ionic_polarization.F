!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG, CA
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
subroutine Ionic_polarization(I_Berry_RED,en)
 !
 ! Calculate polarization due to ions
 ! in reduced (crystal) coordinated
 !
 use pars,           ONLY:SP,pi
 use electrons,      ONLY:levels
 use D_lattice,      ONLY:Z_species,atom_pos,n_atoms,n_atoms_species,n_atomic_species
 use R_lattice,      ONLY:k_map,g_vec
 use vec_operate,    ONLY:c2a
 !
 implicit none           
 !
 type(levels), intent(in)  :: en
 real(SP),     intent(out) :: I_Berry_RED(3)
 !
 ! Work Space
 !
 integer  :: mod_ion(n_atoms)
 real(SP) :: pdl_ion(n_atoms,3)
 real(SP) :: gpar(3,3)
 integer  :: is,ia,na,id
 logical  :: l_odd
 !
 mod_ion=2
 l_odd=.false.
 !
 if(.not.allocated(Z_species)) then
   call warning(" Ionic polarization cannot be calculated!! ")
   I_Berry_RED=0._SP
   return
 endif
 !
 na=1
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     if(mod(nint(real(Z_species(is),SP)),2) == 1) then
       mod_ion(na)=1
       l_odd=.true.
     endif
     na=na+1
   enddo
 enddo
 !
 do id=1,3
   gpar(:,id)=g_vec(k_map%g0_idx(id,1),:)
   call c2a(v_in=gpar(:,id),mode='ki2c')
 enddo
 !
 na=1
 pdl_ion=0._SP
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     do id=1,3
       if(k_map%max_kdir(id)==1) cycle
       pdl_ion(na,id)=real(Z_species(is),SP)*dot_product(atom_pos(:,ia,is),gpar(:,id))/(2._SP*pi)
       pdl_ion(na,id)=pdl_ion(na,id)-real(mod_ion(na),SP)*nint(pdl_ion(na,id)/real(mod_ion(na),SP))
     enddo
     na=na+1
   enddo
 enddo
 !
 I_Berry_RED=0._SP
 !
 do id=1,3
   if(k_map%max_kdir(id)==1) cycle
   I_Berry_RED(id)=sum(pdl_ion(1:n_atoms,id))
   if(l_odd) then
     I_Berry_RED(id)=I_Berry_RED(id)-1._SP*nint(I_Berry_RED(id)/1._SP)
   else
     I_Berry_RED(id)=I_Berry_RED(id)-2._SP*nint(I_Berry_RED(id)/2._SP)
   endif
 enddo
 !
end subroutine Ionic_polarization
