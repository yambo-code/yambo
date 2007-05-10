!
! Copyright (C) 2000-2005 C. Hogan and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine p2s_qexml_wf(wf_disk,ikibz, pwunit)
 !
 use pars,                  ONLY:SP,DP
 use com,                   ONLY:msg, error
 use electrons,             ONLY:n_bands, n_spin, n_sp_pol
 use D_lattice,             ONLY:n_atom_species,pp_n_l_comp_max,&
&                                PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             ONLY:nkibz
 use wave_func,             ONLY:wf_ncx,wf_nc_k,  wf_igk, wf_ng
 use interfaces,            ONLY:make_real_wf
 use qexml_module
 implicit none
 !
 integer,           intent(in)  :: ikibz,pwunit
 real(SP),          intent(out) :: wf_disk(2,n_bands,wf_ncx,n_spin)
 !
 ! Work Space
 !
 integer        :: ib, ierr, npwk, ik, ig, ispin
 !
 ! holds the double precision pwscf record
 !
 complex(DP)   :: wf_disk_DP(wf_nc_k(ikibz), n_bands) ! note order
 complex(DP)   :: wf_disk_DP_T(n_bands,wf_nc_k(ikibz)) ! note order
 !
 !---------------------------------------------------------------------*
 !    Main data                                                        *
 !---------------------------------------------------------------------*
 npwk = wf_nc_k(ikibz)
 do ispin=1,n_spin
   ! 
   call qexml_read_wfc(ibnds=1, ibnde=n_bands, ik=ikibz, ispin=ispin, &
&       npwk=npwk, igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP, ierr=ierr)
   if(ierr.ne.0) call errore('qexml_read_wfc','Check qexml version?',ierr)
   !
   wf_disk_DP_T=transpose(wf_disk_DP)
   !
   wf_disk(1,1:n_bands,1:wf_nc_k(ikibz),ispin) =  real( wf_disk_DP_T(:,:) )
   wf_disk(2,1:n_bands,1:wf_nc_k(ikibz),ispin) = aimag( wf_disk_DP_T(:,:) )
   !
   !do ib=1,n_bands
   !  call make_real_wf(ib,ikibz,wf_disk(:,ib,:wf_nc_k(ikibz),ispin),wf_nc_k(ikibz))
   !enddo
   !
 enddo
 !
 end subroutine
