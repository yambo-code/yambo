!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
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
 subroutine f2s_wf(wf_disk,ikibz,fhi_inp,fhi_wvf)
!==================================================
 use com,                   ONLY : msg
 use electrons,             ONLY : n_bands, n_spin
 use D_lattice,             ONLY : n_atom_species,pp_n_l_comp_max,&
&                                  PP_alloc,pp_kbs,pp_kb,pp_kbd
 use R_lattice,             ONLY : nkibz
 use wave_func,             ONLY : wf_ng_disk, ng_wf_k
 use f2s_datatypes
 implicit none
 integer        :: ikibz          
 real(SP)       :: wf_disk(2,n_bands,wf_ng_disk,n_spin)
 type(finp_type),  intent(in)   :: fhi_inp
 type(fwvf_type)                :: fhi_wvf
!ws
 integer                        :: ib, ik, npw, i1, i2, ig
! type(iospecs)                  :: ioswf, iospp
! type(iolines)                  :: dlwf
! character(schlen)              :: ch
! character(schlen)              :: infile
 type(fpp_type)                  :: fhi_pp
! integer                        :: nngw



 if (ikibz==1) then

! open and wind wave function file to the beginning of wvf section 
! for reading wave functions
  open(unit=12,file=trim(fhi_wvf%file),form='unformatted')
  read(12) !! cspin,tdouble
  read(12) !! fhi_wvf%ngwx, fhi_wvf%ecut, fhi_wvf%nkptx
  read(12) !!(( fhi_wvf%xk(ii,ik)  ,  ii=1,3        ), ik=1,fhi_wvf%nkptx),...
  read(12) !! fhi_wvf%nr1x,fhi_wvf%nr2x,fhi_wvf%nr3x, fhi_wvf%nnrx
  read(12) !! fhi_wvf%nel, fhi_wvf%_nx
 endif
  read(12) !! fhi_wvf%ngw(ik)
  do i1=1,n_bands
   read(12) (fhi_wvf%c0(ig),ig=1,fhi_wvf%ngwx)
   wf_disk(1,i1,:wf_ng_disk,1)=real(fhi_wvf%c0(:wf_ng_disk))
   wf_disk(2,i1,:wf_ng_disk,1)=aimag(fhi_wvf%c0(:wf_ng_disk))
  enddo
  read(12)!! (fhi_wvf%en(ii,ik),ii=1,fhi_wvf%nx)

 if (ikibz==nkibz) close(12)

 end subroutine

