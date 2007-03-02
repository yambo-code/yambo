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
 integer function ioELPH(ID)
 !==========================
 !
 use pars,           ONLY:SP,schlen
 use IO_m,           ONLY:io_connect,io_disconnect,io_sec,&
&                         io_elemental,io_status,io_bulk,io_header,&
&                         read_is_on,write_is_on,RD_CL,RD_CL_IF_END,io_action,&
&                         io_mode,DUMP,io_fragment 
 use ELPH,           ONLY:ph_modes,elph_nb,elph_gkkp_alloc,ph_freqs_sq,elph_gkkp,&
&                         QP_PH_n_G_bands,max_ph_freq,elph_DW
 use R_lattice,      ONLY:nkbz,nqibz
 use matrix_operate, ONLY:mat_c2r,mat_r2c
 implicit none
 integer      ::ID
 !
 !Work Space
 !
 integer                ::iq,im,ik,i1,i2,ierr
 real(SP), allocatable  ::elph_gkkp_disk(:,:,:)
 character(schlen)      ::var_name
 !
 ioELPH=io_connect(desc='elph_gkkp',type=2,ID=ID)
 if (ioELPH/=0) goto 1
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   ioELPH=io_header(ID)
   if (ioELPH/=0) goto 1
   !
   call io_elemental(ID,VAR="PARS",VAR_SZ=2,MENU=1)
   call io_elemental(ID,&
&       VAR=" Phonon modes           :",I0=ph_modes,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,&
&       VAR=" El-PH bands            :",I0=elph_nb,CHECK=.true.,OP=(/"=="/))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   !
   if (io_mode(ID)==DUMP.or.write_is_on(ID)) then
     QP_PH_n_G_bands=elph_nb
   endif
   !
   ioELPH=io_status(ID)
   if (ioELPH/=0) goto 1
   !
   call io_bulk(ID,VAR="MAX_PH_FREQ",VAR_SZ=(/1/))
   call io_bulk(ID,R0=max_ph_freq)
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
 !
 ! Fragmentation
 !
 call io_fragment(ID,i_pointer=iq,ierr=ierr)
 !
 ! When the DB is fragmented I allow a partial reading checking
 ! if the fragment exists or not.
 ! If the fragment does not exist (ierr<0) I return an error code
 !
 if (ierr<0.and.read_is_on(ID)) then
   ioELPH=-1
   goto 1
 endif
 !
 ! Allocation
 !
 if (read_is_on(ID)) call elph_gkkp_alloc()
 !
 ! Manage RD_CL_IF_END
 !
 if (io_action(ID)==RD_CL_IF_END.and.iq==nqibz) io_action(ID)=RD_CL
 !
 ! PH frequencies
 !
 write (var_name,'(a,i3.3)') "PH_FREQS",iq
 call io_bulk(ID,VAR=trim(var_name),VAR_SZ=(/ph_modes/))
 call io_bulk(ID,R1=ph_freqs_sq(iq,:))
 ! 
 ! ELPH_gkkp 
 !
 allocate(elph_gkkp_disk(elph_nb*elph_nb,ph_modes,2))
 !
 write (var_name,'(a,i3.3)') "ELPH_GKKP_Q",iq
 call io_bulk(ID,VAR=trim(var_name),VAR_SZ=(/elph_nb*elph_nb,ph_modes,2,nkbz/) )
 !
 do ik=1,nkbz
   ! 
   ! WRITE
   ! 
   if (write_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp_disk((i1-1)*elph_nb+i2,im,1)=real(elph_gkkp(ik,im,i1,i2))
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp_disk((i1-1)*elph_nb+i2,im,2)=aimag(elph_gkkp(ik,im,i1,i2))
   endif
   ! 
   call io_bulk(ID,R3=elph_gkkp_disk,IPOS=(/1,1,1,ik/))
   ! 
   ! READ 
   ! 
   if (read_is_on(ID)) then
     forall(im=1:ph_modes,i1=1:elph_nb,i2=1:elph_nb) &
&          elph_gkkp(ik,im,i1,i2)=elph_gkkp_disk((i1-1)*elph_nb+i2,im,1)+&
&                   (0._SP,1._SP)*elph_gkkp_disk((i1-1)*elph_nb+i2,im,2)
   endif
   !
 enddo
 !
 deallocate(elph_gkkp_disk)
 ! 
 ! ELPH_DW 
 !
 write (var_name,'(a,i3.3)') "ELPH_DW_Q",iq
 call io_bulk(ID,VAR=trim(var_name),VAR_SZ=(/ph_modes,elph_nb,elph_nb,nkbz/))
 !
 do ik=1,nkbz
   ! 
   call io_bulk(ID,R3=elph_DW(ik,:,:,:),IPOS=(/1,1,1,ik/))
   ! 
 enddo
 !
1 call io_disconnect(ID=ID)
 !
 end function
