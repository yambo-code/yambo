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
integer function ioDB1_ver_2(en,k,ID)
 !
 use pars,             ONLY:SP
 use electrons,        ONLY:levels,default_nel,n_spin,n_sp_pol
 use D_lattice,        ONLY:a, alat, nsym, i_time_rev, default_Tel, dl_sop,Tel
 use R_lattice,        ONLY:ng_vec, g_vec, bz_samp
 use wave_func,        ONLY:wf_ncx, wf_nc_k, wf_igk,wf_ng
 use IO_m,             ONLY:io_connect, io_disconnect,  &
&                           io_bulk, read_is_on, serial_number,&
&                           io_netcdf_support
 implicit none
 type(levels)  :: en
 type(bz_samp) :: k
 integer       :: ID
 !
 ! Work Space
 !
 integer,    external :: rand_identifier
 integer, allocatable :: I_on_disk(:)
 real(SP),allocatable :: R_on_disk(:)
 !
 ioDB1_ver_2 =io_connect(desc="s.db1",type=-1,ID=ID)
 if (ioDB1_ver_2/=0) goto 1
 !
 ! Serial Number
 !
 serial_number=rand_identifier()
 !
 ! H_versionID 
 !
 allocate(I_on_disk(4))
 call io_bulk(ID,VAR="H_versionID",VAR_SZ=(/4/))
 call io_bulk(ID,I1=I_on_disk)
 deallocate(I_on_disk)
 !
 ! HID 
 !
 allocate(I_on_disk(6))
 call io_bulk(ID,VAR="HID",VAR_SZ=(/6/))
 call io_bulk(ID,I1=I_on_disk)
 k%nibz=I_on_disk(1)
 ng_vec=I_on_disk(2)
 nsym=  I_on_disk(3)
 en%nb= I_on_disk(4)
 n_spin  =1
 n_sp_pol=1
 i_time_rev= I_on_disk(5)
 wf_ncx= I_on_disk(6)
 deallocate(I_on_disk)
 !
 ! HRD 
 !
 allocate(R_on_disk(14))
 call io_bulk(ID,VAR="HRD",VAR_SZ=(/14/))
 call io_bulk(ID,R1=R_on_disk)
 default_Tel=R_on_disk(1)
 default_nel=R_on_disk(2)
 alat=       R_on_disk(3:5)
 a(1,:)=     R_on_disk(6:8)
 a(2,:)=     R_on_disk(9:11)
 a(3,:)=     R_on_disk(12:14)
 deallocate(R_on_disk)

 if (read_is_on(ID)) then 
   allocate(en%E(en%nb,k%nibz,n_spin),g_vec(ng_vec,3),k%pt(k%nibz,3),&
&           dl_sop(3,3,nsym*(i_time_rev+1)),wf_nc_k(k%nibz))
 endif
 !
 ! Sym-GsID 
 !
 allocate(I_on_disk(k%nibz))
 call io_bulk(ID,VAR="Sym-GsID",VAR_SZ=(/k%nibz/))
 call io_bulk(ID,I1=I_on_disk)
 wf_nc_k=I_on_disk
 wf_ncx=maxval(wf_nc_k)
 deallocate(I_on_disk)
 !
 if (read_is_on(ID)) allocate(wf_igk(wf_ncx,k%nibz))
 !
 ! Sym-GsRD 
 !
 allocate(R_on_disk(9*nsym+3*ng_vec))
 call io_bulk(ID,VAR="Sym-GsRD",VAR_SZ=(/9*nsym+3*ng_vec/))
 call io_bulk(ID,R1=R_on_disk)
 dl_sop(:,:,:nsym)=reshape(R_on_disk(:9*nsym),(/3,3,nsym/))
 g_vec(:,:)=reshape(R_on_disk(9*nsym+1:),(/ng_vec,3/))
 deallocate(R_on_disk)
 do i1=1,nsym
   if (i_time_rev==1) dl_sop(:,:,i1+nsym)=-dl_sop(:,:,i1)
 enddo
 if (i_time_rev==1) nsym=2*nsym

 if (io_netcdf_support(ID)) then
!
! En-IgkID 
!
   allocate(I_on_disk(k%nibz*wf_ncx))
   call io_bulk(ID,VAR="En-IgkID",VAR_SZ=(/k%nibz*wf_ncx/))
   call io_bulk(ID,I1=I_on_disk)
   !
   ! En-IgkRD 
   !
   allocate(R_on_disk((3+en%nb)*k%nibz))
   call io_bulk(ID,VAR="En-IgkRD",VAR_SZ=(/ (3+en%nb)*k%nibz /) )
   call io_bulk(ID,R1=R_on_disk)

   do i1=1,k%nibz
     k%pt(i1,:)=R_on_disk((3+en%nb)*(i1-1)+1:(3+en%nb)*(i1-1)+3)
     en%E(:,i1,1)=R_on_disk((3+en%nb)*(i1-1)+4:(3+en%nb)*i1)
     wf_igk(:,i1)=I_on_disk(wf_ncx*(i1-1)+1:wf_ncx*i1)
   enddo
   deallocate(I_on_disk)
   deallocate(R_on_disk)
 else
   allocate(I_on_disk(wf_ncx))
   allocate(R_on_disk(3+en%nb))
   do i1=1,k%nibz
     call io_bulk(ID,I1=I_on_disk)
     call io_bulk(ID,R1=R_on_disk)
     k%pt(i1,:)=R_on_disk(1:3)
     en%E(:,i1,1)=R_on_disk(4:)
     wf_igk(:,i1)=I_on_disk
   enddo
   deallocate(I_on_disk)
   deallocate(R_on_disk)
 endif

1 call io_disconnect(ID=ID)

end function ioDB1_ver_2
