!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine io_fragment(ID,ID_frag,i_fragment,j_fragment,ierr)
 !
 ! This routine opens the fragment.
 !
 use pars,        ONLY:schlen
 use IO_int,      ONLY:io_connect,io_control
 use IO_m,        ONLY:io_file,io_mode,io_com,io_PAR_comm,io_PAR_cpu,&
&                      io_sec,io_extension,read_is_on,write_is_on,&
&                      OP_RD_CL,OP_APP_CL,io_type,io_folder,io_raw_extension
 use IO_m,        ONLY:frag_WF,frag_DIP,frag_RESPONSE,frag_QINDX,frag_HF,&
                       frag_SC,frag_RT,frag_ELPH,frag_BS_K,frag_DG
 use stderr,      ONLY:intc,write_fragments_IO_log,logfile
 use parallel_m,  ONLY:ncpu 
#if ! defined _io_lib
 use parallel_int,ONLY:PARALLEL_live_message
 use timing_m,    ONLY:timing
#endif
 !
 implicit none
 !
 integer, intent(in)   :: ID
 integer, intent(out)  :: ID_frag
 integer, optional     :: i_fragment
 integer, optional     :: j_fragment
 integer, optional     :: ierr
 !
 ! Work Space
 !
 integer           :: ierr_,i_frag,ACT_NOW
 character(schlen) :: fragment_filename
 !
 if (present(ierr)) ierr=0
 !
 ID_frag=ID
 !
 select case (trim(io_raw_extension(ID)))
   case("Vnl","wf","kb_pp","kb_pp_pwscf")
     if (.not.frag_WF) return
   case("kindx")
     if (.not.frag_QINDX) return
   case("dip_iR_and_P","Overlaps")
     if (.not.frag_DIP) return
   case("HF_and_locXC","xxvxc")
     if (.not.frag_HF) return
   case("em1s","em1d","pp","Xx")
     if (.not.frag_RESPONSE) return
   case("carriers","THETA","OBSERVABLES","G","REF")
     if (.not.frag_RT) return
   case("scE","scWFs","scV","sc_MeanPotential")
     if (.not.frag_SC) return
   case("elph_gkkp","elph_gkkp_expanded","elph_gFsq")
     if (.not.frag_ELPH) return
   case("PH_Double_Grid")
     if (.not.frag_DG) return
 end select
 !
 if (index(trim(io_raw_extension(ID)),"BS_Q")>0 .and. .not.frag_BS_K) return
 !
#if ! defined _io_lib
 call timing('io_fragment',OPR='start')
#endif
 !
 if (read_is_on(ID))  ACT_NOW=OP_RD_CL
 if (write_is_on(ID)) ACT_NOW=OP_APP_CL
 !
 if (     present(i_fragment)) i_frag=i_fragment
 if (.not.present(i_fragment)) i_frag=maxval(io_sec(ID,:))
 !
 call io_control(ACTION=ACT_NOW,SEC=(/i_frag/),COM=io_com(ID),MODE=io_mode(ID),ID=ID_frag)
 !
 io_sec(ID_frag,:)   =io_sec(ID,:)
 io_PAR_comm(ID_frag)=io_PAR_comm(ID)
 io_PAR_cpu(ID_frag) =io_PAR_cpu(ID)
 !
 ! io_file is redefined by io_connect
 !
 io_file(ID_frag)=' '
 !
 fragment_filename=trim(io_extension(ID))//"_fragment_"//trim(intc(i_frag))
 if (present(j_fragment)) then
   fragment_filename=trim(io_extension(ID)) &
&                    //"_fragments_"//trim(intc(i_frag))//"_"//trim(intc(j_fragment))
 endif
 !
 ierr_=io_connect(fragment_filename,type=io_type(ID),subfolder=trim(io_folder(ID)),ID=ID_frag)
 if (present(ierr)) ierr=ierr_
 !
#if ! defined _io_lib
 if (ncpu>1.and.write_fragments_IO_log.and.index(logfile,"stderr")==0) then
   if (write_is_on(ID_frag)) call PARALLEL_live_message("Writing "//trim(fragment_filename))
   if (read_is_on(ID_frag) ) call PARALLEL_live_message("Reading "//trim(fragment_filename))
 endif
#endif
 !
#if ! defined _io_lib
 call timing('io_fragment',OPR='stop')
#endif
 !
end subroutine io_fragment
