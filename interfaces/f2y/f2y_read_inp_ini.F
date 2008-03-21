!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
 subroutine read_inp_ini(fhi_inp)
!================================
 use com,                   ONLY : msg
 use f2y_datatypes
 implicit none
 type(finp_type)::fhi_inp
 ! 
 ! Work Space
 !
 integer::ia,ii,ik,is,jj

 open(unit=11,file=trim(fhi_inp%file),form='formatted')
 read(11,*) fhi_inp%ibrav,fhi_inp%pgind
 read(11,*) fhi_inp%nel,fhi_inp%tmetal,fhi_inp%ekt,fhi_inp%tdegen
 read(11,*) fhi_inp%ecut,fhi_inp%ecuti
 read(11,*) fhi_inp%tmold,fhi_inp%tband,fhi_inp%nrho
 read(11,*) fhi_inp%npos,fhi_inp%nthm,fhi_inp%nseed
 read(11,*) fhi_inp%T_ion,fhi_inp%T_init,fhi_inp%Q,fhi_inp%nfi_rescale
 read(11,*) fhi_inp%nsp,fhi_inp%tpsmesh,fhi_inp%coordwave
 read(11,*) fhi_inp%nkpt

 call preallocate_finp(fhi_inp)

 do ik=1,fhi_inp%nkpt
  read(11,*) (fhi_inp%xk(ii,ik),ii=1,3)
 enddo
 do jj=1,3     
  read(11,*) (fhi_inp%a(ii,jj),ii=1,3)
 enddo
 do jj=1,3     
  read(11,*) (fhi_inp%b(ii,jj),ii=1,3)
 enddo
 read(11,*) fhi_inp%alat,fhi_inp%omega

 fhi_inp%nax=200 !! auxilary setting to simplify the subroutine
 call allocate_finp_coor(fhi_inp)
 do is=1,fhi_inp%nsp
  read(11,*)  fhi_inp%atom(is),fhi_inp%na(is),fhi_inp%zv(is),fhi_inp%ion_fac
  read(11,*)  fhi_inp%ion_damp,fhi_inp%rgauss,fhi_inp%lmax(is),fhi_inp%lloc(is)
  read(11,*)
  if (fhi_inp%nax<fhi_inp%na(is)) then
   call msg('nl','number of atoms exceeds 200; adjust fhi2s')
  endif
  do ia=1,fhi_inp%na(is)
   read(11,*) (fhi_inp%tau0(ii,ia,is),ii=1,3)
  enddo
  read(11,*) (fhi_inp%ineq_pos(ii),ii=1,3)
 enddo
 read(11,*) fhi_inp%nrot

 call allocate_finp_rot(fhi_inp)

 do ik=1,fhi_inp%nrot 
  read(11,*)
  read(11,'(3i4)') ((fhi_inp%srot(ii,jj,ik),ii=1,3),jj=1,3)
 enddo
 close(11)

 end subroutine read_inp_ini

