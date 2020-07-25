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
 use pars, ONLY:DP
 ! 
#include<memory.h>
 !
 integer, parameter :: cube_strlen=256
 !
 type cube_t
    character(cube_strlen) :: filename 
    character(cube_strlen) :: msg(2)
    integer     :: nr(3)
    integer     :: natom
    real(DP)    :: a1(3), a2(3), a3(3)
    real(DP)    :: orig(3)
    integer,  allocatable :: Z_atoms(:)
    real(DP), allocatable :: charges(:)
    real(DP), allocatable :: tau(:,:)
    real(DP), allocatable :: dat(:,:,:)
    logical     :: alloc=.false.
 end type
 !
 contains
   !
   subroutine cube_alloc(nr,natom,obj)
     implicit none
     integer      :: nr(3),natom
     type(cube_t) :: obj
     obj%nr=nr
     obj%natom=natom
     YAMBO_ALLOC(obj%Z_atoms,(natom))
     YAMBO_ALLOC(obj%tau,(3,natom))
     YAMBO_ALLOC(obj%charges,(natom))
     YAMBO_ALLOC(obj%dat,(nr(1),nr(2),nr(3)))
     obj%alloc=.true. 
   end subroutine
   !
   subroutine cube_free(obj)
     implicit none
     type(cube_t) :: obj
     obj%nr=0
     obj%natom=0
     YAMBO_FREE(obj%Z_atoms)
     YAMBO_FREE(obj%tau)
     YAMBO_FREE(obj%charges)
     YAMBO_FREE(obj%dat)
     obj%alloc=.false. 
   end subroutine
   !
   subroutine cube_write(obj)
     implicit none
     type(cube_t) :: obj
     if (.not.obj%alloc) return
     if (len_trim(obj%filename)==0) return
     call cube_write_base(obj%filename,obj%msg(1),obj%msg(2),obj%nr(1),obj%nr(2),obj%nr(3),obj%natom, &
                          obj%a1,obj%a2,obj%a3,obj%Z_atoms,obj%charges,obj%tau,obj%dat,obj%orig)
   end subroutine cube_write
   !
   subroutine cube_read(obj,ierr)
     implicit none
     type(cube_t) :: obj
     integer      :: ierr
     integer :: nr(3),natom
     !
     ierr=1
     if (len_trim(obj%filename)==0) return
     call cube_read_base(obj%filename,NR1=nr(1),NR2=nr(2),NR3=nr(3),NATOM=natom,IERR=ierr)
     if (ierr/=0) return 
     !
     call cube_alloc(nr,natom,obj)
     call cube_read_base(obj%filename,obj%msg(1),obj%msg(2),obj%nr(1),obj%nr(2),obj%nr(3),obj%natom, &
                         obj%a1,obj%a2,obj%a3,obj%Z_atoms,obj%charges,obj%tau,obj%dat,obj%orig,IERR=ierr)
     if (ierr/=0) return 
   end subroutine cube_read
   !
   subroutine cube_read_base(filename,msg1,msg2,nr1,nr2,nr3,natom,al1,al2,al3,Z_atoms,charges,tau,dat,orig,ierr)
     implicit none
     !
     character(*),           intent(in)  :: filename
     character(*), optional, intent(out) :: msg1,msg2
     integer,      optional, intent(out) :: nr1,nr2,nr3,natom
     integer,      optional, intent(out) :: Z_atoms(*)
     real(DP),     optional, intent(out) :: al1(3),al2(3),al3(3)
     real(DP),     optional, intent(out) :: charges(*)
     real(DP),     optional, intent(out) :: tau(3,*)
     real(DP),     optional, intent(out) :: dat(*)
     real(DP),     optional, intent(out) :: orig(3)
     integer,                intent(out) :: ierr
     !
     integer :: nr1_,nr2_,nr3_,natom_
     integer :: Z_at
     character(256) :: msg1_,msg2_
     real(DP):: al1_(3),al2_(3),al3_(3),orig_(3)
     real(DP):: charge_at, tau_at(3)
     real(DP), allocatable:: data_tmp(:,:,:) 
     !
     integer :: ia,i,i1,i2,i3,ind
     integer :: iun=12345
     !
     ierr=0
     open(iun,file=filename,iostat=ierr)
     if (ierr/=0) return
     !
     read(iun,*,iostat=ierr) msg1_
     if (ierr/=0) return
     read(iun,*,iostat=ierr) msg2_
     if (ierr/=0) return
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
        read(iun,*,iostat=ierr) Z_at, charge_at, tau_at
        if (ierr/=0) return
        if (present(Z_atoms)) Z_atoms(ia)=Z_at
        if (present(charges)) charges(ia)=charge_at
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
     if (present(msg1))     msg1=msg1_
     if (present(msg2))     msg2=msg2_
     if (present(nr1))      nr1=nr1_
     if (present(nr2))      nr2=nr2_
     if (present(nr3))      nr3=nr3_
     if (present(natom))    natom=natom_
     if (present(al1))      al1=al1_
     if (present(al2))      al2=al2_
     if (present(al3))      al3=al3_
     if (present(orig))     orig=orig_
     !
   end subroutine cube_read_base
   !
   subroutine cube_write_base(filename,msg1,msg2,nr1,nr2,nr3,natom,al1,al2,al3,Z_atoms,charges,tau,dat,orig)
     implicit none
     !
     character(*), intent(in) :: filename
     character(*), intent(in) :: msg1,msg2
     integer,      intent(in) :: nr1,nr2,nr3,natom
     integer,      intent(in) :: Z_atoms(*)
     real(DP),     intent(in) :: charges(*)
     real(DP),     intent(in) :: al1(3),al2(3),al3(3)
     real(DP),     intent(in) :: tau(3,*)
     real(DP),     intent(in) :: dat(nr1,nr2,nr3)
     real(DP),     intent(in) :: orig(3)
     !
     integer :: iun=12345
     integer :: ia
     !
     open(iun,file=filename)
     !
     write(iun,"(a,/,a)") trim(msg1),trim(msg2)
     write(iun,"(i4,3f15.9)") natom, orig
     !
     write(iun,"(i4,3f15.9)") nr1, al1
     write(iun,"(i4,3f15.9)") nr2, al2
     write(iun,"(i4,3f15.9)") nr3, al3
     !
     do ia = 1, natom
        write(iun,"(i4,3f15.9)") Z_atoms(ia), charges(ia), tau(:,ia)
     enddo
     !
     write(iun,"(4f15.9)") dat(:,:,:)
     !
     close(iun)
     !
   end subroutine cube_write_base
   !
end module cube_m

