!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM HM DS
!
subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,I4,I5,&
                   &             R0,R1,R2,R3,R4,R5,&
                   &             D0,D1,D2,D3,D4,D5,&
                   &             C0,C1,C2,C3,C4,C5,&
                   &             Z0,Z1,Z2,Z3,Z4,Z5,&
                   &             L0,L1,L2,L3,L4,   &
                   &                            IPOS)
 !
 use netcdf
 use pars,          ONLY:SP,DP,LP,nf90_SP
 use com,           ONLY:fat_log
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use IO_m,          ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,&
&                        netcdf_dim,read_is_on,io_file
 !
 implicit none
 !
 integer      :: ID, nf_error
 character(*),optional :: VAR
 integer,     optional :: VAR_SZ(:),IPOS(:)
 integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:),I4(:,:,:,:),I5(:,:,:,:,:)
 real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:),R4(:,:,:,:),R5(:,:,:,:,:)
 real(DP),    optional :: D0,D1(:),D2(:,:),D3(:,:,:),D4(:,:,:,:),D5(:,:,:,:,:)
 complex(SP), optional, target :: C0,C1(:),C2(:,:),C3(:,:,:),C4(:,:,:,:),C5(:,:,:,:,:)
 complex(DP), optional, target :: Z0,Z1(:),Z2(:,:),Z3(:,:,:),Z4(:,:,:,:),Z5(:,:,:,:,:)
 !
 ! These types can be used instead of logical that 
 ! are not supported by NetCdf
 !
 integer(LP), optional :: L0,L1(:),L2(:,:),L3(:,:,:),L4(:,:,:,:)
 ! 
 ! Work Space
 !
 real(SP), pointer    :: RC0(:),RC1(:,:),RC2(:,:,:),RC3(:,:,:,:),RC4(:,:,:,:,:),RC5(:,:,:,:,:,:)
 real(DP), pointer    :: RZ0(:),RZ1(:,:),RZ2(:,:,:),RZ3(:,:,:,:),RZ4(:,:,:,:,:),RZ5(:,:,:,:,:,:)
 integer, allocatable :: var_dim(:),ipos_loc(:),isize_loc(:)
 integer              :: j1,dims,dims_db
 integer, allocatable :: var_dim_db(:)
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
     call netcdf_call(nf90_redef(io_unit(ID)),ID,VAR=VAR)
     allocate(var_dim(size(VAR_SZ)))
     do j1=1,size(VAR_SZ)
       var_dim(j1)=netcdf_dim(ID,VAR_SZ(j1))
     enddo
     nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID,1))
     if (nf_error==NF90_NOERR) then
       allocate(var_dim_db(size(VAR_SZ)))
       nf_error=nf90_inquire_variable(io_unit(ID),io_netcdf_var(ID,1),ndims=dims_db,dimids=var_dim_db)
       call netcdf_call(nf_error,ID,VAR=VAR)
       if(any((/var_dim(:)/=var_dim_db(:)/))) then
         call error(' Trying to overwrite variable '//trim(VAR)//' in '//trim(io_file(ID))//' with wrong dimensions ')
       endif
       if (fat_log) call warning(' Overwriting variable '//trim(VAR)//' in file '//trim(io_file(ID)))
       deallocate(var_dim_db)
     endif
     if (nf_error/=NF90_NOERR) nf_error=nf90_def_var(io_unit(ID),VAR,nf90_SP,var_dim,io_netcdf_var(ID,1)) 
     call netcdf_call(nf_error,ID,VAR=VAR)
#if defined _HDF5_COMPRESSION
     shuffle=0
     deflate=1
     deflate_level=2
     call netcdf_call(nf90_def_var_deflate(io_unit(ID),io_netcdf_var(ID,1),shuffle,deflate,deflate_level),ID,VAR=VAR)
#endif
     call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
     deallocate(var_dim)
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID,1)),ID,VAR=VAR)
#if defined _HDF5_COMPRESSION
     call netcdf_call(nf90_inq_var_deflate(io_unit(ID),io_netcdf_var(ID,1),shuffle,deflate,deflate_level),ID,VAR=VAR)
#endif
   endif
   return
 endif
 !
 ! Actual IO
 !
 nf_error=0
 dims =0
 !
 if (present(L1).or.present(I1).or.present(R1).or.present(D1)) dims=1
 if (present(L2).or.present(I2).or.present(R2).or.present(D2).or.present(C1).or.present(Z1)) dims=2
 if (present(L3).or.present(I3).or.present(R3).or.present(D3).or.present(C2).or.present(Z2)) dims=3
 if (present(L4).or.present(I4).or.present(R4).or.present(D4).or.present(C3).or.present(Z3)) dims=4
 if (               present(I5).or.present(R5).or.present(D5).or.present(C4).or.present(Z4)) dims=5
 if (                                             present(C5).or.present(Z5)) dims=6
 !
 if(dims>0) then
   allocate(ipos_loc(dims),isize_loc(dims))
   if (     present(IPOS)) ipos_loc=IPOS
   if (.not.present(IPOS)) ipos_loc=1
 endif
 !
 ! Logical numbers / vectors / matricies
 !
 if (present(L0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), L0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), L0, (/1/))
 endif
 !
 if (present(L1)) then
   isize_loc=(/size(L1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), L1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), L1, ipos_loc, isize_loc)
 endif
 !
 if (present(L2)) then
   isize_loc=(/size(L2,1),size(L2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), L2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), L2, ipos_loc, isize_loc)
 endif
 !
 if (present(L3)) then
   isize_loc=(/size(L3,1),size(L3,2),size(L3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), L3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), L3, ipos_loc, isize_loc)
 endif
 !
 if (present(L4)) then
   isize_loc=(/size(L4,1),size(L4,2),size(L4,3),size(L4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), L4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), L4, ipos_loc, isize_loc)
 endif
 !
 ! Integer numbers / vectors / matricies
 !
 if (present(I0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I0, (/1/))
 endif
 !
 if (present(I1)) then
   isize_loc=(/size(I1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I1, ipos_loc, isize_loc)
 endif
 !
 if (present(I2)) then
   isize_loc=(/size(I2,1),size(I2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I2, ipos_loc, isize_loc)
 endif
 !
 if (present(I3)) then
   isize_loc=(/size(I3,1),size(I3,2),size(I3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I3, ipos_loc, isize_loc)
 endif
 !
 if (present(I4)) then
   isize_loc=(/size(I4,1),size(I4,2),size(I4,3),size(I4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I4, ipos_loc, isize_loc)
 endif
 !
 if (present(I5)) then
   isize_loc=(/size(I5,1),size(I5,2),size(I5,3),size(I5,4),size(I5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), I5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), I5, ipos_loc, isize_loc)
 endif
 !
 ! Real numbers / vectors / matricies
 !
 if (present(R0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R0, (/1/))
 endif
 !
 if (present(R1)) then
   isize_loc=(/size(R1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R1, ipos_loc, isize_loc)
 endif
 !
 if (present(R2)) then
   isize_loc=(/size(R2,1),size(R2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R2, ipos_loc, isize_loc)
 endif
 !
 if (present(R3)) then
   isize_loc=(/size(R3,1),size(R3,2),size(R3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R3, ipos_loc, isize_loc)
 endif
 !
 if (present(R4)) then
   isize_loc=(/size(R4,1),size(R4,2),size(R4,3),size(R4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R4, ipos_loc, isize_loc)
 endif
 !
 if (present(R5)) then
   isize_loc=(/size(R5,1),size(R5,2),size(R5,3),size(R5,4),size(R5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), R5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), R5, ipos_loc, isize_loc)
 endif
 !
 ! Double precision numbers / vectors / matricies
 !
 if (present(D0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D0, (/1/))
 endif
 !
 if (present(D1)) then
   isize_loc=(/size(D1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D1, ipos_loc, isize_loc)
 endif
 !
 if (present(D2)) then
   isize_loc=(/size(D2,1),size(D2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D2, ipos_loc, isize_loc)
 endif
 !
 if (present(D3)) then
   isize_loc=(/size(D3,1),size(D3,2),size(D3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D3, ipos_loc, isize_loc)
 endif
 !
 if (present(D4)) then
   isize_loc=(/size(D4,1),size(D4,2),size(D4,3),size(D4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D4, ipos_loc, isize_loc)
 endif
 !
 if (present(D5)) then
   isize_loc=(/size(D5,1),size(D5,2),size(D5,3),size(D5,4),size(D5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), D5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), D5, ipos_loc, isize_loc)
 endif
 !
 ! Complex numbers / vectors / matricies
 !
 if (present(C0)) then
   call C_F_POINTER(C_LOC(C0), RC0, (/2/) )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC0, (/1/), (/2/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC0, (/1/), (/2/))
 endif
 !
 if (present(C1)) then
   isize_loc=(/2,size(C1)/)
   call C_F_POINTER(C_LOC(C1), RC1, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC1, ipos_loc, isize_loc)
 endif
 !
 if (present(C2)) then
   isize_loc=(/2,size(C2,1),size(C2,2)/)
   call C_F_POINTER(C_LOC(C2), RC2, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC2, ipos_loc, isize_loc)
 endif
 !
 if (present(C3)) then
   isize_loc=(/2,size(C3,1),size(C3,2),size(C3,3)/)
   call C_F_POINTER(C_LOC(C3), RC3, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC3, ipos_loc, isize_loc)
 endif
 !
 if (present(C4)) then
   isize_loc=(/2,size(C4,1),size(C4,2),size(C4,3),size(C4,4)/)
   call C_F_POINTER(C_LOC(C4), RC4, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC4, ipos_loc, isize_loc)
 endif
 !
 if (present(C5)) then
   isize_loc=(/2,size(C5,1),size(C5,2),size(C5,3),size(C5,4),size(C5,5)/)
   !RC5 => linkC2R(C5,isize_loc)
   call C_F_POINTER(C_LOC(C5), RC5, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RC5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RC5, ipos_loc, isize_loc)
 endif
 !
 ! DP Complex numbers / vectors / matricies
 !
 if (present(Z0)) then
   call C_F_POINTER(C_LOC(Z0), RZ0, (/2/) )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ0, (/1/), (/2/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ0, (/1/), (/2/))
 endif
 !
 if (present(Z1)) then
   isize_loc=(/2,size(Z1)/)
   call C_F_POINTER(C_LOC(Z1), RZ1, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ1, ipos_loc, isize_loc)
 endif
 !
 if (present(Z2)) then
   isize_loc=(/2,size(Z2,1),size(Z2,2)/)
   call C_F_POINTER(C_LOC(Z2), RZ2, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ2, ipos_loc, isize_loc)
 endif
 !
 if (present(Z3)) then
   isize_loc=(/2,size(Z3,1),size(Z3,2),size(Z3,3)/)
   call C_F_POINTER(C_LOC(Z3), RZ3, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ3, ipos_loc, isize_loc)
 endif
 !
 if (present(Z4)) then
   isize_loc=(/2,size(Z4,1),size(Z4,2),size(Z4,3),size(Z4,4)/)
   call C_F_POINTER(C_LOC(Z4), RZ4, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ4, ipos_loc, isize_loc)
 endif
 !
 if (present(Z5)) then
   isize_loc=(/2,size(Z5,1),size(Z5,2),size(Z5,3),size(Z5,4),size(Z5,5)/)
   call C_F_POINTER(C_LOC(Z5), RZ5, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,1), RZ5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,1), RZ5, ipos_loc, isize_loc)
 endif
 !
 call netcdf_call(nf_error,ID)
 !
 if (allocated(ipos_loc))  deallocate(ipos_loc)
 if (allocated(isize_loc)) deallocate(isize_loc)
 !
end subroutine
