!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AF, PDA
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
module cube_m
 ! 
#include<memory.h>
 !
 contains
   !
   subroutine cube_read(filename,nr1,nr2,nr3,natom,al1,al2,al3,Z_atoms,tau,dat,orig,ierr)
     use pars, ONLY:SP
     implicit none
     !
     character(*),       intent(in)  :: filename
     integer,  optional, intent(out) :: nr1,nr2,nr3,natom
     integer,  optional, intent(out) :: Z_atoms(*)
     real(SP), optional, intent(out) :: al1(3),al2(3),al3(3)
     real(SP), optional, intent(out) :: tau(3,*)
     real(SP), optional, intent(out) :: dat(*)
     real(SP), optional, intent(out) :: orig(3)
     integer,            intent(out) :: ierr
     !
     integer :: nr1_,nr2_,nr3_,natom_
     integer :: Z_at
     real(SP):: al1_(3),al2_(3),al3_(3),orig_(3)
     real(SP):: rtmp, tau_at(3)
     real(SP), allocatable:: data_tmp(:,:,:) 
     !
     integer :: ia,i,i1,i2,i3,ind
     integer :: iun=12345
     !
     ierr=0
     open(iun,file=filename,iostat=ierr)
     if (ierr/=0) return
     !
     do i = 1, 2
       read(iun,*,iostat=ierr)
       if (ierr/=0) return
     enddo
     !
     read(iun,*,iostat=ierr) natom_, orig_
     if (ierr/=0) return
     !
     read(iun,*,iostat=ierr) nr1_, al1_
     if (ierr/=0) return
     read(iun,*,iostat=ierr) nr2_, al2_
     if (ierr/=0) return
     read(iun,*,iostat=ierr) nr3_, al3_
     if (ierr/=0) return
     !
     do ia = 1, natom_
        read(iun,*,iostat=ierr) Z_at, rtmp, tau_at
        if (ierr/=0) return
        if (present(Z_atoms)) Z_atoms(ia)=Z_at
        if (present(tau))     tau(:,ia)=tau_at
     enddo
     !
     if (present(dat)) then
       !
       YAMBO_ALLOC(data_tmp,(nr1_,nr2_,nr3_))
       read(iun,*,iostat=ierr) data_tmp(:,:,:)
       if (ierr/=0) return
       !
       do i1=1,nr1_
       do i2=1,nr2_
       do i3=1,nr3_
         ind=i3+(i2-1)*nr3_+(i1-1)*nr2_*nr3_
         dat(ind)=data_tmp(i1,i2,i3)
       enddo
       enddo
       enddo
       !
       YAMBO_FREE(data_tmp)
     endif
     !
     close(iun)
     !
     if (present(nr1))      nr1=nr1_
     if (present(nr2))      nr2=nr2_
     if (present(nr3))      nr3=nr3_
     if (present(natom))    natom=natom_
     if (present(al1))      al1=al1_
     if (present(al2))      al2=al2_
     if (present(al3))      al3=al3_
     if (present(orig))     orig=orig_
     !
   end subroutine cube_read
   !
end module cube_m
