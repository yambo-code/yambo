!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
! 
! Copyright (C) 2002-2005 J. K. Dewhurst, S. Sharma and C. Ambrosch-Draxl.
! This file is distributed under the terms of the GNU General Public License.
! See the file COPYING for license details.
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
subroutine elk_wf(wf_disk,ikibz,ib_grp,KSS_file_name)
 !
 use pars,      ONLY:SP,DP
 use electrons, ONLY:n_spin,n_bands
 use R_lattice, ONLY:nkibz
 use D_lattice, ONLY:n_atomic_species,n_atoms_species
 use com,       ONLY:error
 use wave_func, ONLY:wf_ncx,wf_nb_io,wf_nc_k
 implicit none
 !
 integer        :: ikibz,ib_grp
 character(*)   :: KSS_file_name
 real(SP)       :: wf_disk(2,wf_nb_io,wf_ncx,n_spin)
 !
 ! Work Space
 !
 integer        :: is,ia,ik,nsym,ib
 complex(DP)    :: wf_disk_DP(wf_ncx,1,n_bands)
 !
 open(unit=11,file=KSS_file_name,form='unformatted')
 !
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 do is=1,n_atomic_species
   read(11,err=10) 
   read(11,err=10) 
   read(11,err=10) 
   read(11,err=10) 
   do ia=1,n_atoms_species(is)
     read(11,err=10) 
   end do
 end do
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) nsym
 do is=1,nsym
   read(11,err=10) 
 end do
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) wf_ncx
 do ik=1,ikibz-1
   read(11,err=10) 
   read(11,err=10) 
   read(11,err=10) 
   read(11,err=10) 
   read(11,err=10) 
 enddo
 !
 wf_disk=(0._SP,0._SP)
 !
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) 
 read(11,err=10) wf_disk_DP(:,:,:)
 wf_disk_DP(wf_nc_k(ik)+1:,:,:)=(0._DP,0._DP)
 do ib=1,n_bands
   wf_disk(1,ib,:,1) =real(wf_disk_DP(:,1,ib))
   wf_disk(2,ib,:,1)=aimag(wf_disk_DP(:,1,ib))
 enddo
 !
 close(11)
 return
 !
 10 call error("Reading from YAMBO")
 !
end subroutine
