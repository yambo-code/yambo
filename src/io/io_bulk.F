!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, HM, DS
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
subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,I4,I5,&
                   &             R0,R1,R2,R3,R4,R5,&
                   &             C0,C1,C2,C3,C4,   &
                   &             L0,L1,L2,L3,L4,   &
                   &                            IPOS)
 !
 use pars,    ONLY:SP,LP,nf90_SP
 use netcdf
 use IO_m,    ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,ver_is_gt_or_eq,&
&                  netcdf_dim,read_is_on
 implicit none
 integer      :: ID, error
 character(*),optional :: VAR
 integer,     optional :: VAR_SZ(:),IPOS(:)
 integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:),I4(:,:,:,:),I5(:,:,:,:,:)
 real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:),R4(:,:,:,:),R5(:,:,:,:,:)
 complex(SP), optional :: C0,C1(:),C2(:,:),C3(:,:,:),C4(:,:,:,:)
 !
 ! These types can be used instead of logical that 
 ! are not supported by NetCdf
 !
 integer(LP), optional :: L0,L1(:),L2(:,:),L3(:,:,:),L4(:,:,:,:)
 ! 
 ! Work Space
 !
 integer, allocatable :: var_dim(:),ipos_loc(:),isize_loc(:)
 integer              :: j1,dims
#if defined _HDF5_COMPRESSION
 integer              :: shuffle,deflate,deflate_level
#endif
 !
 !====================
 !== NETCDF I/O ======
 !====================
 !
 ! Variable definition and ID
 !
 if (present(VAR).and.present(VAR_SZ)) then
   if (write_is_on(ID)) then
     allocate(var_dim(size(VAR_SZ)))
     call netcdf_call(nf90_redef(io_unit(ID)))
     do j1=1,size(VAR_SZ)
       var_dim(j1)=netcdf_dim(ID,VAR_SZ(j1))
     enddo
     call netcdf_call(nf90_def_var(io_unit(ID),VAR,nf90_SP,var_dim,io_netcdf_var(ID)))
#if defined _HDF5_COMPRESSION
     shuffle=0
     deflate=1
     deflate_level=2
     call netcdf_call(nf90_def_var_deflate(io_unit(ID),io_netcdf_var(ID),shuffle,deflate,deflate_level))
#endif
     call netcdf_call(nf90_enddef(io_unit(ID))) 
     deallocate(var_dim)
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID)))
#if defined _HDF5_COMPRESSION
     call netcdf_call(nf90_inq_var_deflate(io_unit(ID),io_netcdf_var(ID),shuffle,deflate,deflate_level))
#endif
   endif
   return
 endif
 !
 ! Actual IO
 !
 error=0
 !
 if (present(L1).or.present(I1).or.present(R1)               ) dims=1
 if (present(L2).or.present(I2).or.present(R2).or.present(C1)) dims=2
 if (present(L3).or.present(I3).or.present(R3).or.present(C2)) dims=3
 if (present(L4).or.present(I4).or.present(R4).or.present(C3)) dims=4
 if (               present(I5).or.present(R5).or.present(C4)) dims=5
 !
 allocate(ipos_loc(dims))
 !
 if (     present(IPOS)) ipos_loc=IPOS
 if (.not.present(IPOS)) ipos_loc=1
 !
 ! Logical numbers / vectors / matricies
 !
 if (present(L0)) then
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), L0, (/1/))
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), L0, (/1/))
 endif
 !
 if (present(L1)) then
   allocate(isize_loc(1))
   isize_loc=(/size(L1)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), L1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), L1, ipos_loc, isize_loc)
 endif
 !
 if (present(L2)) then
   allocate(isize_loc(2))
   isize_loc=(/size(L2,1),size(L2,2)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), L2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), L2, ipos_loc, isize_loc)
 endif
 !
 if (present(L3)) then
   allocate(isize_loc(3))
   isize_loc=(/size(L3,1),size(L3,2),size(L3,3)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), L3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), L3, ipos_loc, isize_loc)
 endif
 !
 if (present(L4)) then
   allocate(isize_loc(3))
   isize_loc=(/size(L4,1),size(L4,2),size(L4,3),size(L4,4)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), L4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), L4, ipos_loc, isize_loc)
 endif
 !
 ! Integer numbers / vectors / matricies
 !
 if (present(I0)) then
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I0, (/1/))
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I0, (/1/))
 endif
 !
 if (present(I1)) then
   allocate(isize_loc(1))
   isize_loc=(/size(I1)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I1, ipos_loc, isize_loc)
 endif
 !
 if (present(I2)) then
   allocate(isize_loc(2))
   isize_loc=(/size(I2,1),size(I2,2)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I2, ipos_loc, isize_loc)
 endif
 !
 if (present(I3)) then
   allocate(isize_loc(3))
   isize_loc=(/size(I3,1),size(I3,2),size(I3,3)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I3, ipos_loc, isize_loc)
 endif
 !
 if (present(I4)) then
   allocate(isize_loc(4))
   isize_loc=(/size(I4,1),size(I4,2),size(I4,3),size(I4,4)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I4, ipos_loc, isize_loc)
 endif
 !
 if (present(I5)) then
   allocate(isize_loc(5))
   isize_loc=(/size(I5,1),size(I5,2),size(I5,3),size(I5,4),size(I5,5)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), I5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), I5, ipos_loc, isize_loc)
 endif
 !
 ! Real numbers / vectors / matricies
 !
 if (present(R0)) then
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R0, (/1/))
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R0, (/1/))
 endif
 !
 if (present(R1)) then
   allocate(isize_loc(1))
   isize_loc=(/size(R1)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R1, ipos_loc, isize_loc)
 endif
 !
 if (present(R2)) then
   allocate(isize_loc(2))
   isize_loc=(/size(R2,1),size(R2,2)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R2, ipos_loc, isize_loc)
 endif
 !
 if (present(R3)) then
   allocate(isize_loc(3))
   isize_loc=(/size(R3,1),size(R3,2),size(R3,3)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R3, ipos_loc, isize_loc)
 endif
 !
 if (present(R4)) then
   allocate(isize_loc(3))
   isize_loc=(/size(R4,1),size(R4,2),size(R4,3),size(R4,4)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R4, ipos_loc, isize_loc)
 endif
 !
 if (present(R5)) then
   allocate(isize_loc(5))
   isize_loc=(/size(R5,1),size(R5,2),size(R5,3),size(R5,4),size(R5,5)/)
   if (write_is_on(ID)) error=nf90_put_var(io_unit(ID), io_netcdf_var(ID), R5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) error=nf90_get_var(io_unit(ID), io_netcdf_var(ID), R5, ipos_loc, isize_loc)
 endif
 !
 ! Complex numbers / vectors / matricies
 !
 if (present(C0)) then
   if (write_is_on(ID)) call nf_put_vara(io_unit(ID), io_netcdf_var(ID), (/1/), (/2/), C0)
   if (read_is_on(ID) ) call nf_get_vara(io_unit(ID), io_netcdf_var(ID), (/1/), (/2/), C0)
 endif
 !
 if (present(C1)) then
   allocate(isize_loc(2))
   isize_loc=(/2,size(C1)/)
   if (write_is_on(ID)) call nf_put_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C1)
   if (read_is_on(ID) ) call nf_get_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C1)
 endif
 !
 if (present(C2)) then
   allocate(isize_loc(3))
   isize_loc=(/2,size(C2,1),size(C2,2)/)
   if (write_is_on(ID)) call nf_put_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C2)
   if (read_is_on(ID) ) call nf_get_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C2)
 endif
 !
 if (present(C3)) then
   allocate(isize_loc(4))
   isize_loc=(/2,size(C3,1),size(C3,2),size(C3,3)/)
   if (write_is_on(ID)) call nf_put_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C3)
   if (read_is_on(ID) ) call nf_get_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C3)
 endif
 !
 if (present(C4)) then
   allocate(isize_loc(5))
   isize_loc=(/2,size(C4,1),size(C4,2),size(C4,3),size(C4,4)/)
   if (write_is_on(ID)) call nf_put_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C4)
   if (read_is_on(ID) ) call nf_get_vara(io_unit(ID), io_netcdf_var(ID), ipos_loc, isize_loc, C4)
 endif
 !
 call netcdf_call(error)
 !
 deallocate(ipos_loc)
 if (allocated(isize_loc)) deallocate(isize_loc)
 !
end subroutine
