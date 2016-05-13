!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine io_bulk(ID,VAR,VAR_SZ,I0,I1,I2,I3,R0,R1,R2,R3,C0,C1,C2,C3,L1,L3,IPOS)
 !
 use pars,    ONLY:SP,LP,nf90_SP
 use netcdf
 use IO_m,    ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,ver_is_gt_or_eq,&
&                  netcdf_dim,io_rec_pos,read_is_on
 implicit none
 integer      :: ID, error
 character(*),optional :: VAR
 integer,     optional :: VAR_SZ(:),IPOS(:)
 integer,     optional :: I0,I1(:),I2(:,:),I3(:,:,:)
 real(SP),    optional :: R0,R1(:),R2(:,:),R3(:,:,:)
 complex(SP), optional :: C0,C1(:),C2(:,:),C3(:,:,:)
 !
 ! These types can be used instead of logical that 
 ! are not supported by NetCdf
 !
 integer(LP), optional :: L1(:),L3(:,:,:)
 ! 
 ! Work Space
 !
 integer :: var_dim(10),j1
 !
 !====================
 !== NETCDF I/O ======
 !====================
 !
 if (present(VAR).and.present(VAR_SZ)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_redef(io_unit(ID)))
     do j1=1,size(VAR_SZ)
       var_dim(j1)=netcdf_dim(ID,VAR_SZ(j1))
     enddo
     io_rec_pos(ID)=1
     if (ver_is_gt_or_eq(ID,revision=723)) then
       call netcdf_call(nf90_def_var(io_unit(ID),VAR,nf90_SP,var_dim(:size(VAR_SZ)),io_netcdf_var(ID)))
     else
       call netcdf_call(nf90_def_var(io_unit(ID),VAR,nf90_real,var_dim(:size(VAR_SZ)),io_netcdf_var(ID)))
     endif
     call netcdf_call(nf90_enddef(io_unit(ID))) 
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_var(ID)))
   endif
   io_rec_pos(ID)=1
 endif
 !
 if (present(I0)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     I0,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
   call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                   I0,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+1
 endif
 !
 if (present(I1)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     I1,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
   call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                   I1,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(I1)
 endif
 !
 if (present(I2)) then
   if (write_is_on(ID)) then
   call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                   I2,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                     I2,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(I2)
 endif
 if (present(I3)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     I3,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                     I3,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(I3)
 endif
 !
 if (present(L1)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     L1,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                       L1,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(L1)
 endif
 !
 if (present(L3)) then
   if (write_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),L3,IPOS))
     else
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                       L3,(/io_rec_pos(ID)/)))
     endif
   else if (read_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),L3,IPOS))
     else
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                       L3,(/io_rec_pos(ID)/)))
     endif
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(L3)
 endif             
 !
 if (present(R0)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     R0,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                     R0,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+1
 endif
 !
 if (present(R1)) then
   if (write_is_on(ID)) then
     call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                     R1,(/io_rec_pos(ID)/)))
   else if (read_is_on(ID)) then
     call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                       R1,(/io_rec_pos(ID)/)))
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(R1)
 endif
 !
 if (present(R2)) then
   if (write_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),R2,IPOS))
     else
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                       R2,(/io_rec_pos(ID)/)))
     endif
   else if (read_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),R2,IPOS))
     else
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                       R2,(/io_rec_pos(ID)/)))
     endif
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(R2)
 endif
 !
 if (present(R3)) then
   if (write_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),R3,IPOS))
     else
       call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),&
&                       R3,(/io_rec_pos(ID)/)))
     endif
   else if (read_is_on(ID)) then
     if (present(IPOS)) then
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),R3,IPOS))
     else
       call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),&
&                       R3,(/io_rec_pos(ID)/)))
     endif
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(R3)
 endif
 !
 ! Added by HM 7/05/2015
 ! Here we call the C functions directly to circumvent the fortran type check
 ! and write complex numbers as real
 !
 if (present(C2)) then
   if (write_is_on(ID)) then
     if (present(IPOS)) then
       call ncvpt(io_unit(ID), io_netcdf_var(ID), IPOS, (/size(C2,1),size(C2,2),2/), C2, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),C2,IPOS))
     else
       call ncvpt(io_unit(ID), io_netcdf_var(ID), (/1,1,1/), (/size(C2,1),size(C2,2),2/), C2, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),C2,(/io_rec_pos(ID)/)))
     endif
   else if (read_is_on(ID)) then
     if (present(IPOS)) then
       call ncvgt(io_unit(ID), io_netcdf_var(ID), IPOS, (/size(C2,1),size(C2,2),2/), C2, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),C2,IPOS))
     else
       call ncvgt(io_unit(ID), io_netcdf_var(ID), (/1,1,1/), (/size(C2,1),size(C2,2),2/), C2, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),C2,(/io_rec_pos(ID)/)))
     endif
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(C2)
 endif
 !
 if (present(C3)) then
   if (write_is_on(ID)) then
     if (present(IPOS)) then
       call ncvpt(io_unit(ID), io_netcdf_var(ID), IPOS, (/2,size(C3,1),size(C3,2),size(C3,3)/), C3, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),C3,IPOS))
     else
       call ncvpt(io_unit(ID), io_netcdf_var(ID), (/1,1,1,1/), (/2,size(C3,1),size(C3,2),size(C3,3)/), C3, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_put_var(io_unit(ID),io_netcdf_var(ID),C3,(/io_rec_pos(ID)/)))
     endif
   else if (read_is_on(ID)) then
     if (present(IPOS)) then
       call ncvgt(io_unit(ID), io_netcdf_var(ID), IPOS, (/2,size(C3,1),size(C3,2),size(C3,3)/), C3, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),C3,IPOS))
     else
       call ncvgt(io_unit(ID), io_netcdf_var(ID), (/1,1,1,1/), (/2,size(C3,1),size(C3,2),size(C3,3)/), C3, error)
       call netcdf_call(error)
       !call netcdf_call(nf90_get_var(io_unit(ID),io_netcdf_var(ID),C3,(/io_rec_pos(ID)/)))
     endif
   endif
   io_rec_pos(ID)=io_rec_pos(ID)+size(C3)
 endif
 !
end subroutine
