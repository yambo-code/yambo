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
 subroutine read_wvf(fhi_inp,fhi_gg,fhi_wvf)
!===========================================
  use com,                   ONLY : msg
  use f2s_datatypes
 implicit none
 type(finp_type)::fhi_inp
 type(fgg_type)::fhi_gg
 type(fwvf_type)::fhi_wvf
 ! 
 ! Work Space
 !
 integer:: ii,ik,ig,is,ia,jj,kk
 logical::tdouble
 character(1):: cspin

 open(unit=12,file=trim(fhi_wvf%file),form='unformatted')
 read(12) cspin,tdouble 
 if (cspin=='n') then
  fhi_wvf%nspin=0;fhi_wvf%nspin_pt=0;
 elseif (cspin=='p') then
  fhi_wvf%nspin=0;fhi_wvf%nspin_pt=1;
 elseif (cspin=='s') then
  fhi_wvf%nspin=1;fhi_wvf%nspin_pt=1;
 endif
! spin polarization not implemented in SELF
 if ((fhi_wvf%nspin==1) .or. (fhi_wvf%nspin_pt==1)) then
  call msg('l','Spin polarized wvf encountered ... not yet supported')
  stop
 endif
 read(12)  fhi_wvf%ngwx, fhi_wvf%ecut, fhi_wvf%nkptx
! enforce consistency with G- and k-vectors from inp.ini
 if ((fhi_gg%ngwx<fhi_wvf%ngwx).or.(fhi_inp%nkpt/=fhi_wvf%nkptx)) then
  call msg('l','Inconsistency of wvf dimension or k-point sampling')
  stop
 endif

 call allocate_fwvf_wvf(fhi_wvf)

 read(12) fhi_wvf%xk,fhi_wvf%wkpt,fhi_wvf%igk,fhi_wvf%n1,fhi_wvf%n2,fhi_wvf%n3
 read(12) fhi_wvf%nr1x,fhi_wvf%nr2x,fhi_wvf%nr3x, fhi_wvf%nnrx
! enforce consistency of fourier meshes
 if ((fhi_wvf%nr1x/=fhi_gg%nr11).or.(fhi_wvf%nr2x/=fhi_gg%nr22) &
&   .or.(fhi_wvf%nr3x/=fhi_gg%nr33)) then
  call msg('l','Inconsistency of fourier mesh')
  stop
 endif

 read(12) fhi_wvf%nel, fhi_wvf%nx
 call allocate_fwvf_en(fhi_wvf)

 do ik=1,fhi_wvf%nkptx
  read(12) fhi_wvf%ngw(ik)
! skip wave function here (to be read in rdkss_wrdb2)
  do ii=1,fhi_wvf%nx
   read(12)
  enddo
! now read the KS eigenvalues and occupation numbers
  read(12) (fhi_wvf%eig(ii,ik),ii=1,fhi_wvf%nx)
 enddo
 do ik=1,fhi_wvf%nkptx
  read(12) (fhi_wvf%focc(ii,ik),ii=1,fhi_wvf%nx)
 enddo
 read(12) fhi_wvf%efermi
 read(12) fhi_wvf%pgind
 read(12) fhi_wvf%alat,fhi_wvf%omega
! enforce consistency regarding alat and omega
 if  ((abs(fhi_inp%alat-fhi_wvf%alat)>=1d-4)&
&   .or.(abs(fhi_inp%omega-fhi_wvf%omega)>1d-2)) then
  call msg('l','Inconsistency regarding cell size')
  stop
 endif
 read(12) fhi_wvf%nsx,fhi_wvf%nax
! only the following value of nsx is relevant for SELF
 read(12) fhi_wvf%nsx
! Enforce consistency number of species
 if (fhi_inp%nsp/=fhi_wvf%nsx) then
  call msg('l','Inconsistency regarding number of species')
  stop
 endif

 call allocate_fwvf_nsp(fhi_wvf)

 do is=1,fhi_wvf%nsx
  read(12) fhi_wvf%na(is),fhi_wvf%atom(is)
! Enforce consistency number of atoms per species 
  if (fhi_inp%nsp/=fhi_wvf%nsx) then
   call msg('l','Inconsistency regarding number of atoms per species')
   stop
  endif
 enddo
 
 do is=1,fhi_wvf%nsx
  do ia=1,fhi_wvf%na(is)
   read(12) (fhi_wvf%tau0(ii,ia,is),ii=1,3)
  enddo
 enddo

 close(12) 

 end subroutine read_wvf

