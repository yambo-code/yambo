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
 private
 !
 integer, parameter :: DP=kind(1.0d0)
 integer, parameter :: cube_strlen=256
 integer, parameter :: iun=12345
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
    real(DP), allocatable :: gindex(:,:,:)
    real(DP), allocatable :: grid(:,:)
    logical     :: alloc=.false.
 end type
 !
 public :: cube_t
 public :: cube_alloc
 public :: cube_free
 public :: cube_copy
 public :: cube_read
 public :: cube_read_base
 public :: cube_write
 public :: cube_write_base
 public :: cube_build_grid
 public :: cube_setval
 !
 contains
   !
   subroutine cube_alloc(nr,natom,obj)
     implicit none
     integer      :: nr(3),natom
     type(cube_t) :: obj
     obj%nr=nr
     obj%natom=natom
     allocate(obj%Z_atoms(natom))
     allocate(obj%tau(3,natom))
     allocate(obj%charges(natom))
     allocate(obj%dat(nr(1),nr(2),nr(3)))
     obj%alloc=.true. 
   end subroutine
   !
   subroutine cube_free(obj)
     implicit none
     type(cube_t) :: obj
     obj%nr=0
     obj%natom=0
     if(allocated(obj%Z_atoms))  deallocate(obj%Z_atoms)
     if(allocated(obj%tau))      deallocate(obj%tau)
     if(allocated(obj%charges))  deallocate(obj%charges)
     if(allocated(obj%dat))      deallocate(obj%dat)
     if(allocated(obj%gindex))   deallocate(obj%gindex)
     if(allocated(obj%grid))     deallocate(obj%grid)
     obj%alloc=.false. 
   end subroutine
   !
   subroutine cube_copy(obj_out,obj_in)
     implicit none
     type(cube_t) :: obj_out,obj_in
     !
     call cube_free(obj_out)
     if (.not.obj_in%alloc) return
     !
     call cube_alloc(obj_in%nr,obj_in%natom,obj_out)
     !
     obj_out%filename=obj_in%filename
     obj_out%msg=obj_in%msg
     obj_out%a1=obj_in%a1
     obj_out%a2=obj_in%a2
     obj_out%a3=obj_in%a3
     obj_out%orig=obj_in%orig
     obj_out%Z_atoms=obj_in%Z_atoms
     obj_out%charges=obj_in%charges
     obj_out%tau=obj_in%tau
     obj_out%dat=obj_in%dat
     if (allocated(obj_in%gindex)) obj_out%gindex=obj_in%gindex
     if (allocated(obj_in%grid))   obj_out%grid=obj_in%grid
     obj_out%alloc=obj_in%alloc
     !
   end subroutine
   !
   subroutine cube_build_grid(obj)
     implicit none
     type(cube_t) :: obj
     integer :: i1,i2,i3,ind,nr(3)
     real(DP):: vv(3)
     
     if (.not.obj%alloc) return
     !
     nr=obj%nr
     if (allocated(obj%gindex)) deallocate(obj%gindex)
     if (allocated(obj%grid))   deallocate(obj%grid)
     allocate(obj%gindex(nr(1),nr(2),nr(3)))
     allocate(obj%grid(3,product(nr))) 
     !
     do i3=1,nr(3)
     do i2=1,nr(2)
     do i1=1,nr(1)
       !
       ind=i3+(i2-1)*nr(3)+(i1-1)*nr(2)*nr(3)
       obj%gindex(i1,i2,i3)=ind
       !
       vv = obj%orig + (i1-1)*obj%a1 + (i2-1)*obj%a2 + (i3-1)*obj%a3
       obj%grid(:,ind)=vv(:)
     enddo
     enddo
     enddo
     !
   end subroutine
   !
   subroutine cube_setval(obj,mode,val,radius,orig,ierr)
     implicit none
     type(cube_t) :: obj
     character(*) :: mode
     real(DP)     :: val(2)    ! val_in, val_out
     real(DP)     :: radius(3) ! incl ellipsoid
     real(DP)     :: orig(3)
     integer, intent(out) :: ierr
     !
     real(DP) :: x(3),rtmp
     integer  :: ind,i1,i2,i3

     ierr=1
     if (.not.obj%alloc) return
     if (.not.allocated(obj%grid)) call cube_build_grid(obj)
     ierr=0
     !
     !
     select case(trim(mode))
       !
     case ("constant","uniform")
       obj%dat=val(1)
       !
     case ("sphere")
       !
       obj%dat=val(2)   ! outer value
       !
       do i3=1,obj%nr(3)
       do i2=1,obj%nr(2)
       do i1=1,obj%nr(1)
         !
         ind = obj%gindex(i1,i2,i3)
         x=obj%grid(:,ind)
         rtmp=sum( (x(:)-orig(:))**2 ) /radius(1)**2
         if (rtmp<=1.0d0) obj%dat(i1,i2,i3)=val(1)   ! inner value
         !
       enddo
       enddo
       enddo
       !
     case ("ellipsoid")
       !
       obj%dat=val(2)   ! outer value
       !
       do i3=1,obj%nr(3)
       do i2=1,obj%nr(2)
       do i1=1,obj%nr(1)
         !
         ind = obj%gindex(i1,i2,i3)
         x=obj%grid(:,ind)
         rtmp=sum((x(:)-orig(:))**2/radius(:)**2)
         if (rtmp<=1.0d0) obj%dat(i1,i2,i3)=val(1)   ! inner value
         !
       enddo
       enddo
       enddo
       !
     case default
       ierr=2
       return
     end select
     !
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
     real(DP), allocatable:: data_tmp(:)
     !
     integer :: ia,i,i1,i2,i3
     integer :: ind,ind_for
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
       allocate(data_tmp(nr1_*nr2_*nr3_))
       read(iun,*,iostat=ierr) data_tmp
       if (ierr/=0) return
       !
       ! data layout is C-like (i3 is the fastest index)
       ! dat needs to have a FOR-layout (i1 fastest)
       !
       do i1=1,nr1_
       do i2=1,nr2_
       do i3=1,nr3_
         ind    =i3+(i2-1)*nr3_+(i1-1)*nr2_*nr3_
         ind_for=i1+(i2-1)*nr1_+(i3-1)*nr1_*nr2_
         dat(ind_for)=data_tmp(ind)
       enddo
       enddo
       enddo
       !
       deallocate(data_tmp)
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
     integer :: ia,i1,i2,i3,ind
     real(DP), allocatable :: data_tmp(:)
     !
     allocate(data_tmp(nr1*nr2*nr3))
     ind=0
     do i1=1,nr1
     do i2=1,nr2
     do i3=1,nr3
       ind=ind+1
       data_tmp(ind)=dat(i1,i2,i3)
     enddo
     enddo
     enddo
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
        write(iun,"(i4,4f15.9)") Z_atoms(ia), charges(ia), tau(:,ia)
     enddo
     !
     write(iun,"(4f15.9)") data_tmp
     close(iun)
     !
     deallocate(data_tmp)
     !
   end subroutine cube_write_base
   !
end module cube_m

