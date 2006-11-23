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
 subroutine p2s_wf(wf_disk,ikibz, pwunit)
!==============================================
 use pars
 use electrons,             ONLY : n_bands, n_spin
 use D_lattice,             ONLY : n_atom_species,pp_n_l_comp_max,&
&                                  PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             ONLY : nkibz
 use wave_func,             ONLY : wf_ng_disk,ng_wf_k
 use defs_datatypes,        ONLY : hdr_type, wffile_type
 use pw_interface
 implicit none
 integer,  intent(in)           :: ikibz, pwunit
 real(SP), intent(out)          :: wf_disk(2,n_bands,wf_ng_disk,n_spin)
!ws
 integer        :: ib, ikeff, ispinor
!
! holds the double precision pwscf record
!
 complex(DP)    :: wf_disk_DP(ng_wf_k(ikibz))
!
 wf_disk=(0._SP,0._SP)
!---------------------------------------------------------------------*
!    Main data                                                        *
!---------------------------------------------------------------------*
 if (ikibz==1) call pw_wfcstart(pwunit)

 do ispinor=1,n_spin
   ikeff = ikibz + (ispinor-1)*nkibz
   call pw_wfcscan( pwunit, ikeff ) ! scan for this ikibz
   do ib = 1, n_bands       ! loop over bands

     call pw_wfcread(pwunit,ib,ng_wf_k(ikibz),wf_disk_DP)
     wf_disk(1,ib,1:ng_wf_k(ikibz),ispinor) =  real( wf_disk_DP(1:ng_wf_k(ikibz)) )
     wf_disk(2,ib,1:ng_wf_k(ikibz),ispinor) = aimag( wf_disk_DP(1:ng_wf_k(ikibz)) )

   enddo
   call pw_wfcscanend(pwunit,ikeff)
 enddo

 if (ikibz==nkibz) then
   call pw_wfcstop(pwunit)
 endif
 end subroutine
