!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DV
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
subroutine c2y_wf(wf_disk,i_sp_pol,ikibz,ib_grp,KSS_file_name)
 !
 use pars,                  ONLY:SP,DP,cZERO
 use electrons,             ONLY:n_bands,n_spinor
 use R_lattice,             ONLY:nkibz
 use wave_func,             ONLY:wf_ncx,wf_nb_io,wf_nb_io_groups
 implicit none
 !
 integer        :: i_sp_pol,ikibz,ib_grp
 character(*)   :: KSS_file_name
 complex(SP)    :: wf_disk(wf_ncx,n_spinor,wf_nb_io)
 ! 
 ! Workspace ...
 ! 
 integer            :: i1,i2,nb_to_read
 integer            :: isection,irecord,idum, idum2
 character*80       :: str
 real(DP)           :: scale 
 real(DP), allocatable :: wfdum(:)
 logical            :: ldum
 !
 integer            :: i_spinor
 !
 !
 if (ib_grp==1) then
 !
   open(unit=11,file=KSS_file_name,form='unformatted')
 !
 !     ==  Section  1: Header                                          ==
 !
   read(11) str
 !
 ! Here I skip all the section till the wfs.
   do isection =2,8 
     read(11) irecord
     call fskip(irecord)
   enddo  
!
   read(11) irecord
   read(11) irecord
   read(11) nb_to_read,idum,idum2,scale
   read(11) ldum,idum,idum2
 !
 endif
 !
 wf_disk=cZERO
 !
 i_spinor=1
 ! 
 ! Read all bands or only the remaining in the last block
 !
 nb_to_read=wf_nb_io
 if (ib_grp*wf_nb_io>n_bands) nb_to_read=n_bands-wf_nb_io*(ib_grp-1) 
 !
   do i1=1,nb_to_read
     read(11) idum 
     allocate (wfdum(idum))
     read(11) wfdum
     idum2=idum/2
     wf_disk(1,1,i1) = cmplx(wfdum(1),wfdum(2),kind=SP)
     do i2=2,idum2,2
       wf_disk(i2  ,1,i1) = cmplx(wfdum(i2+1),wfdum(i2+2),kind=SP) 
       wf_disk(i2+1,1,i1) = conjg(wf_disk(i2,1,i1))   
     enddo
   deallocate(wfdum) 
   enddo
   ! 
 !
 if (ikibz==nkibz.and.ib_grp==wf_nb_io_groups) close(11)
 !
 end subroutine 
