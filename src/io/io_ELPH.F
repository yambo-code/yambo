!
!        Copyright (C) 2000-2022 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM AC
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
#if defined _ELPH
integer function io_ELPH(ID,what)
 !
 use pars,           ONLY:schlen
 use IO_m,           ONLY:io_sec,read_is_on,frag_ELPH
 use IO_int,         ONLY:io_connect,io_disconnect,io_fragment,&
&                         io_bulk,io_header
 use ELPH,           ONLY:ph_modes,elph_nb,PH_freqs_sq,l_GKKP_hosts_bare_dV,&
&                         l_GKKP_hosts_DW,PH_pol_vector,&
&                         GKKP,elph_grids_are_expanded,GKKP,GKKP_bare
 use stderr,         ONLY:intc
 use interfaces,     ONLY:ELPH_alloc
 use R_lattice,      ONLY:nkbz
 use D_lattice,      ONLY:n_atoms
 use timing_m,       ONLY:timing
 !
#include<memory.h>
 !
 integer      ::ID
 character(*) ::what
 !
 !Work Space
 !
 integer                ::iq,ierr,ID_frag
 character(schlen)      ::db_name
 logical                ::l_no_matrix_elements,l_gkkp
 integer, external      ::variables_ELPH
 !
 call timing('io_ELPH',OPR='start')
 !
 l_no_matrix_elements=.FALSE.
 l_gkkp              =.FALSE.
 !
 ID_frag=0
 !
 ! DB name
 !
 if (index(what,'gkkp_expanded')>0) then
   db_name='elph_gkkp_expanded'
 else 
   db_name='elph_gkkp'
   l_gkkp=.TRUE.
 endif
 !
 elph_grids_are_expanded=index(what,'gkkp_expanded')>0
 l_no_matrix_elements   =index(what,'no_matrix_elements')>0
 !
 io_ELPH=io_connect(desc=trim(db_name),type=2,ID=ID,FORCE_READ_MODE=(.not.any((/io_sec(ID,:)==1/))).and.frag_ELPH)
 if (io_ELPH/=0) then
   elph_grids_are_expanded=.FALSE.
   goto 1
 endif
 !
 if (any((/io_sec(ID,:)==1/))) then
   !
   if (.not.elph_grids_are_expanded) io_ELPH=io_header(ID,R_LATT=.true.,KPTS=.true.,FRAG=frag_ELPH)
   if (     elph_grids_are_expanded) io_ELPH=io_header(ID,R_LATT=.true.,KPTS=.false.,IMPOSE_SN=.FALSE.,FRAG=frag_ELPH)
   if (io_ELPH/=0) goto 1
   !
   io_ELPH=variables_ELPH(ID,what)
   if (io_ELPH/=0) goto 1
   !
 endif
 !
 iq=maxval(io_sec(ID,:))-1
 if (iq<=0) goto 1
 !
 ! Fragmentation
 !
 call io_fragment(ID,ID_frag,i_fragment=iq,ierr=io_ELPH)
 if (ierr<0.and.read_is_on(ID)) goto 1
 !
 ! When the DB is fragmented I allow a partial reading checking
 ! if the fragment exists or not.
 ! If the fragment does not exist (ierr<0) I return an error code
 !
 ! Allocation
 !
 if (read_is_on(ID)) then
   if (.not.l_no_matrix_elements) then
     call ELPH_alloc("LEVELS dV DW",GKKP=GKKP)
     if (l_GKKP_hosts_bare_dV) call ELPH_alloc("dV_bare",GKKP=GKKP_bare)
   else
     call ELPH_alloc("LEVELS",GKKP=GKKP)
   endif
 endif
 !
 ! PH frequencies
 !
 call io_bulk(ID_frag,VAR="PH_FREQS"//trim(intc(iq)),VAR_SZ=(/ph_modes/))
 call io_bulk(ID_frag,R1=PH_freqs_sq(iq,:))
! write(*,*) "w(",iq,")= ",PH_freqs_sq(iq,:)
 !
 ! Polarization vectors
 ! 
 call io_bulk(ID_frag,VAR="POLARIZATION_VECTORS",VAR_SZ=(/2,ph_modes,n_atoms,3/))
 call io_bulk(ID_frag,C3=PH_pol_vector(:,:,:,iq))
 ! 
 call io_bulk(ID_frag,VAR="E_K_PLUS_Q"//trim(intc(iq)),VAR_SZ=(/elph_nb,nkbz,1/) )
 call io_bulk(ID_frag,R3=GKKP%E_kpq(iq)%E)
 !
 if(read_is_on(ID)) then
   GKKP%E_kpq(iq)%nb=elph_nb
   GKKP%E_kpq(iq)%nk=nkbz
 endif
 ! 
 if (l_no_matrix_elements) goto 1
 ! 
 ! ELPH_gkkp 
 !
 call io_bulk(ID_frag,VAR="ELPH_GKKP_Q"//trim(intc(iq)),VAR_SZ=(/2,ph_modes,elph_nb,elph_nb,nkbz/) )
 call io_bulk(ID_frag,C4=GKKP%dVc(:,:,:,:,1))
 !
 !
 if (l_GKKP_hosts_bare_dV) then
   ! 
   ! ELPH_gkkp_bare 
   !
   call io_bulk(ID_frag,VAR="ELPH_GKKP_BARE_Q"//trim(intc(iq)),VAR_SZ=(/2,ph_modes,elph_nb,elph_nb,nkbz/) )
   call io_bulk(ID_frag,C4=GKKP_bare%dVc(:,:,:,:,1))
   !
 endif
 !
 if (l_GKKP_hosts_DW) then
   !
   ! ELPH_DW 
   !
   call io_bulk(ID_frag,VAR="ELPH_DW_Q"//trim(intc(iq)),VAR_SZ=(/ph_modes,elph_nb,elph_nb,nkbz/))
   call io_bulk(ID_frag,R4=GKKP%dVr(:,:,:,:,1))
   !
 endif
 !
1 call io_fragment_disconnect(ID,ID_frag)
 !
 call io_disconnect(ID)
 !
 call timing("io_ELPH",OPR="STOP")
 !
end function
#endif
