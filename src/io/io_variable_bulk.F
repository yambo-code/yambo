!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine io_variable_bulk(ID, VAR_ID, I0, I1, I2, I3, I4, I5,    &
                                     &  R0, R1, R2, R3, R4, R5,    &
                                     &  D0, D1, D2, D3, D4, D5,    &
                                     &  C0, C1, C2, C3, C4, C5, C6,&
                                     &  Z0, Z1, Z2, Z3, Z4, Z5,    &
                                     &  L0, L1, L2, L3, L4,        &
                                     & CH0,CH1,CH2,CH3,CH4,        &
                                     &                     IPOS)
 !
 use pars,          ONLY:SP,DP,LP
 !use pointers,     ONLY:linkC2R,linkZ2D
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use IO_m,          ONLY:write_is_on,read_is_on,io_unit,netcdf_call,io_netcdf_var
 use netcdf
 !
 implicit none
 !
 integer, intent(in)   :: ID,VAR_ID
 integer,     optional :: IPOS(:)
 integer,     optional ::  I0, I1(:), I2(:,:), I3(:,:,:), I4(:,:,:,:), I5(:,:,:,:,:)
 real(SP),    optional ::  R0, R1(:), R2(:,:), R3(:,:,:), R4(:,:,:,:), R5(:,:,:,:,:)
 real(DP),    optional ::  D0, D1(:), D2(:,:), D3(:,:,:), D4(:,:,:,:), D5(:,:,:,:,:)
 complex(SP), optional, target ::  C0, C1(:), C2(:,:), C3(:,:,:), C4(:,:,:,:), C5(:,:,:,:,:), C6(:,:,:,:,:,:)
 complex(DP), optional, target ::  Z0, Z1(:), Z2(:,:), Z3(:,:,:), Z4(:,:,:,:), Z5(:,:,:,:,:)
 !
 ! These types can be used instead of logical that are not supported by NetCdf
 !
 integer(LP), optional ::  L0, L1(:), L2(:,:), L3(:,:,:), L4(:,:,:,:)
 !
 ! Characters (nf90_char) in netcdf occupy the same space as L0 (nf90_byte)
 !
 character,   optional :: CH0,CH1(:),CH2(:,:),CH3(:,:,:),CH4(:,:,:,:)
 ! 
 ! Work Space
 !
 real(SP), pointer    :: RC0(:),RC1(:,:),RC2(:,:,:),RC3(:,:,:,:),RC4(:,:,:,:,:),RC5(:,:,:,:,:,:),RC6(:,:,:,:,:,:,:)
 real(DP), pointer    :: RZ0(:),RZ1(:,:),RZ2(:,:,:),RZ3(:,:,:,:),RZ4(:,:,:,:,:),RZ5(:,:,:,:,:,:)
 !
 integer, allocatable :: ipos_loc(:),isize_loc(:)
 integer              :: nf_error,dims,link_err
 !
 nf_error=0
 dims =0
 !
 if (present(CH1).or.present(L1).or.present(I1).or.present(R1).or.present(D1)                              ) dims=1
 if (present(CH2).or.present(L2).or.present(I2).or.present(R2).or.present(D2).or.present(C1).or.present(Z1)) dims=2
 if (present(CH3).or.present(L3).or.present(I3).or.present(R3).or.present(D3).or.present(C2).or.present(Z2)) dims=3
 if (present(CH4).or.present(L4).or.present(I4).or.present(R4).or.present(D4).or.present(C3).or.present(Z3)) dims=4
 if (                               present(I5).or.present(R5).or.present(D5).or.present(C4).or.present(Z4)) dims=5
 if (                                                                            present(C5).or.present(Z5)) dims=6
 if (                                                                            present(C6)               ) dims=7
 !
 if(dims>0) then
   allocate(ipos_loc(dims),isize_loc(dims))
   if (     present(IPOS)) ipos_loc=IPOS
   if (.not.present(IPOS)) ipos_loc=1
 endif
 !
 ! Character numbers / vectors / matricies
 !
 if (present(CH0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH0, (/1/))
 endif
 !
 if (present(CH1)) then
   isize_loc=(/size(CH1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH1, ipos_loc, isize_loc)
 endif
 !
 if (present(CH2)) then
   isize_loc=(/size(CH2,1),size(CH2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH2, ipos_loc, isize_loc)
 endif
 !
 if (present(CH3)) then
   isize_loc=(/size(CH3,1),size(CH3,2),size(CH3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH3, ipos_loc, isize_loc)
 endif
 !
 if (present(CH4)) then
   isize_loc=(/size(CH4,1),size(CH4,2),size(CH4,3),size(CH4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), CH4, ipos_loc, isize_loc)
 endif
 !
 ! Logical numbers / vectors / matricies
 !
 if (present(L0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L0, (/1/))
 endif
 !
 if (present(L1)) then
   isize_loc=(/size(L1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L1, ipos_loc, isize_loc)
 endif
 !
 if (present(L2)) then
   isize_loc=(/size(L2,1),size(L2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L2, ipos_loc, isize_loc)
 endif
 !
 if (present(L3)) then
   isize_loc=(/size(L3,1),size(L3,2),size(L3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L3, ipos_loc, isize_loc)
 endif
 !
 if (present(L4)) then
   isize_loc=(/size(L4,1),size(L4,2),size(L4,3),size(L4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), L4, ipos_loc, isize_loc)
 endif
 !
 ! Integer numbers / vectors / matricies
 !
 if (present(I0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I0, (/1/))
 endif
 !
 if (present(I1)) then
   isize_loc=(/size(I1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I1, ipos_loc, isize_loc)
 endif
 !
 if (present(I2)) then
   isize_loc=(/size(I2,1),size(I2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I2, ipos_loc, isize_loc)
 endif
 !
 if (present(I3)) then
   isize_loc=(/size(I3,1),size(I3,2),size(I3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I3, ipos_loc, isize_loc)
 endif
 !
 if (present(I4)) then
   isize_loc=(/size(I4,1),size(I4,2),size(I4,3),size(I4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I4, ipos_loc, isize_loc)
 endif
 !
 if (present(I5)) then
   isize_loc=(/size(I5,1),size(I5,2),size(I5,3),size(I5,4),size(I5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), I5, ipos_loc, isize_loc)
 endif
 !
 ! Real numbers / vectors / matricies
 !
 if (present(R0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R0, (/1/))
 endif
 !
 if (present(R1)) then
   isize_loc=(/size(R1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R1, ipos_loc, isize_loc)
 endif
 !
 if (present(R2)) then
   isize_loc=(/size(R2,1),size(R2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R2, ipos_loc, isize_loc)
 endif
 !
 if (present(R3)) then
   isize_loc=(/size(R3,1),size(R3,2),size(R3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R3, ipos_loc, isize_loc)
 endif
 !
 if (present(R4)) then
   isize_loc=(/size(R4,1),size(R4,2),size(R4,3),size(R4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R4, ipos_loc, isize_loc)
 endif
 !
 if (present(R5)) then
   isize_loc=(/size(R5,1),size(R5,2),size(R5,3),size(R5,4),size(R5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), R5, ipos_loc, isize_loc)
 endif
 !
 ! Double precision numbers / vectors / matricies
 !
 if (present(D0)) then
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D0, (/1/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D0, (/1/))
 endif
 !
 if (present(D1)) then
   isize_loc=(/size(D1)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D1, ipos_loc, isize_loc)
 endif
 !
 if (present(D2)) then
   isize_loc=(/size(D2,1),size(D2,2)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D2, ipos_loc, isize_loc)
 endif
 !
 if (present(D3)) then
   isize_loc=(/size(D3,1),size(D3,2),size(D3,3)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D3, ipos_loc, isize_loc)
 endif
 !
 if (present(D4)) then
   isize_loc=(/size(D4,1),size(D4,2),size(D4,3),size(D4,4)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D4, ipos_loc, isize_loc)
 endif
 !
 if (present(D5)) then
   isize_loc=(/size(D5,1),size(D5,2),size(D5,3),size(D5,4),size(D5,5)/)
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), D5, ipos_loc, isize_loc)
 endif
 !
 ! Complex numbers / vectors / matricies
 !
 if (present(C0)) then
   !RC0 => linkC2R(C0,(/2/))
   call C_F_POINTER(C_LOC(C0), RC0, (/2/) )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC0, (/1/), (/2/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC0, (/1/), (/2/))
 endif
 !
 if (present(C1)) then
   isize_loc=(/2,size(C1)/)
   !RC1 => linkC2R(C1,isize_loc)
   call C_F_POINTER(C_LOC(C1), RC1, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC1, ipos_loc, isize_loc)
 endif
 !
 if (present(C2)) then
   isize_loc=(/2,size(C2,1),size(C2,2)/)
   !RC2 => linkC2R(C2,isize_loc)
   call C_F_POINTER(C_LOC(C2), RC2, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC2, ipos_loc, isize_loc)
 endif
 !
 if (present(C3)) then
   isize_loc=(/2,size(C3,1),size(C3,2),size(C3,3)/)
   !RC3 => linkC2R(C3,isize_loc)
   call C_F_POINTER(C_LOC(C3), RC3, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC3, ipos_loc, isize_loc)
 endif
 !
 if (present(C4)) then
   isize_loc=(/2,size(C4,1),size(C4,2),size(C4,3),size(C4,4)/)
   !RC4 => linkC2R(C4,isize_loc)
   call C_F_POINTER(C_LOC(C4), RC4, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC4, ipos_loc, isize_loc)
 endif
 !
 if (present(C5)) then
   isize_loc=(/2,size(C5,1),size(C5,2),size(C5,3),size(C5,4),size(C5,5)/)
   !RC5 => linkC2R(C5,isize_loc)
   call C_F_POINTER(C_LOC(C5), RC5, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC5, ipos_loc, isize_loc)
 endif
 !
 if (present(C6)) then
   isize_loc=(/2,size(C6,1),size(C6,2),size(C6,3),size(C6,4),size(C6,5),size(C6,6)/)
   !RC6 => linkC2R(C6,isize_loc)
   call C_F_POINTER(C_LOC(C6), RC6, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC6, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RC6, ipos_loc, isize_loc)
 endif
 !
 ! DP Complex numbers / vectors / matricies
 !
 if (present(Z0)) then
   !RZ0 => linkZ2D(Z0,(/2/))
   call C_F_POINTER(C_LOC(Z0), RZ0, (/2/) )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ0, (/1/), (/2/))
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ0, (/1/), (/2/))
 endif
 !
 if (present(Z1)) then
   isize_loc=(/2,size(Z1)/)
   !RZ1 => linkZ2D(Z1,isize_loc)
   call C_F_POINTER(C_LOC(Z1), RZ1, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ1, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ1, ipos_loc, isize_loc)
 endif
 !
 if (present(Z2)) then
   isize_loc=(/2,size(Z2,1),size(Z2,2)/)
   !RZ2 => linkZ2D(Z2,isize_loc)
   call C_F_POINTER(C_LOC(Z2), RZ2, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ2, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ2, ipos_loc, isize_loc)
 endif
 !
 if (present(Z3)) then
   isize_loc=(/2,size(Z3,1),size(Z3,2),size(Z3,3)/)
   !RZ3 => linkZ2D(Z3,isize_loc)
   call C_F_POINTER(C_LOC(Z3), RZ3, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ3, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ3, ipos_loc, isize_loc)
 endif
 !
 if (present(Z4)) then
   isize_loc=(/2,size(Z4,1),size(Z4,2),size(Z4,3),size(Z4,4)/)
   !RZ4 => linkZ2D(Z4,isize_loc)
   call C_F_POINTER(C_LOC(Z4), RZ4, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ4, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ4, ipos_loc, isize_loc)
 endif
 !
 if (present(Z5)) then
   isize_loc=(/2,size(Z5,1),size(Z5,2),size(Z5,3),size(Z5,4),size(Z5,5)/)
   !RZ5 => linkZ2D(Z5,isize_loc)
   call C_F_POINTER(C_LOC(Z5), RZ5, isize_loc )
   if (write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ5, ipos_loc, isize_loc)
   if (read_is_on(ID) ) nf_error=nf90_get_var(io_unit(ID), io_netcdf_var(ID,VAR_ID), RZ5, ipos_loc, isize_loc)
 endif
 !
 !
 call netcdf_call(nf_error,ID,ID_VAR=VAR_ID)
 !
 if (allocated(ipos_loc))  deallocate(ipos_loc)
 if (allocated(isize_loc)) deallocate(isize_loc)
 !
end subroutine io_variable_bulk
