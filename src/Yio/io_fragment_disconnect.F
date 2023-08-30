!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine io_fragment_disconnect(ID,ID_frag)
 !
 ! This routine closes the fragment. 
 !
 use IO_int,      ONLY:io_disconnect
 use IO_m,        ONLY:frag_WF,frag_DIP,frag_RESPONSE,frag_QINDX,frag_HF,&
                       frag_SC,frag_RT,frag_ELPH,io_raw_extension,frag_DG
 !
 implicit none
 !
 integer, intent(in) :: ID,ID_frag
 !
 if(ID_frag==0) return
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
 call io_disconnect(ID_frag)
 !
end subroutine io_fragment_disconnect
