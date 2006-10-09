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
!==============================================
 use pars
 use com,                   only : msg, error
 use electrons,             only : n_bands, n_spin, n_sp_pol
 use D_lattice,             only : n_atom_species,pp_n_l_comp_max,&
&                                  PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             only : nkibz
 use wave_func,             only : wf_ng_disk,ng_wf,  wf_igk, wf_ng
 use qexml_module
 implicit none
 integer,           intent(in)  :: ikibz,pwunit
 real(SP),          intent(out) :: wf_disk(2,n_bands,wf_ng_disk,n_spin)
!ws
 integer        :: ib, ierr, npwk, ik, ig, ispinor
!
! holds the double precision pwscf record
!
 complex(dbl)   :: wf_disk_DP(ng_wf(ikibz), n_bands) ! note order
 complex(dbl)   :: wf_disk_DP_T(n_bands,ng_wf(ikibz)) ! note order
!
!---------------------------------------------------------------------*
!    Main data                                                        *
!---------------------------------------------------------------------*
 if(n_sp_pol.eq.2) stop 'p2s: n_sp_pol=2 not coded in WF'

 npwk = ng_wf(ikibz)
 do ispinor=1,n_spin
   
   call qexml_read_wfc(ibnds=1, ibnde=n_bands, ik=ikibz, ispin=ispinor, &
&       npwk=npwk, igk = wf_igk(1:npwk,ikibz), wf = wf_disk_DP, ierr=ierr)
   if(ierr.ne.0) call errore('qexml_read_wfc','Check qexml version?',ierr)

   wf_disk_DP_T=transpose(wf_disk_DP)

   wf_disk(1,1:n_bands,1:ng_wf(ikibz),ispinor) =  real( wf_disk_DP_T(:,:) )
   wf_disk(2,1:n_bands,1:ng_wf(ikibz),ispinor) = aimag( wf_disk_DP_T(:,:) )
 enddo

  return
 end subroutine
