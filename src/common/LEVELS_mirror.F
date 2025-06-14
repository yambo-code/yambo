!
! License-Identifier: GPL
!
! Copyright (C) 2024 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine LEVELS_mirror(what,E,X,Dip,l_USER_field)
 !
 use electrons,     ONLY:levels
 use parser_m,      ONLY:parser
 use X_m,           ONLY:X_t
 use DIPOLES,       ONLY:DIPOLE_t
 use it_tools,      ONLY:runlevel_is_on
 use BS,            ONLY:BS_q,BS_bands
 use stderr,        ONLY:STRING_same
 use QP_m,          ONLY:QP_n_G_bands
#if defined _SC 
 use SC,            ONLY:SC_bands
#endif
#if defined _RT || defined _QED || defined _NL
 use real_time,     ONLY:RT_bands,RT_hole_bands,RT_elec_bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
 use collision_ext, ONLY:COLL_bands,COLL_hole_bands,COLL_elec_bands
 use hamiltonian,   ONLY:H_ref_bands,H_ref_hole_bands,H_ref_elec_bands
#endif
#if defined _NL
 use nl_optics,      ONLY:NL_bands,NL_hole_bands,NL_elec_bands
#endif
 !
 implicit none
 !
 character(*)   ::what
 type(levels)   ::E
 type(DIPOLE_t) ::Dip
 type(X_t)      ::X(5) 
 logical        ::l_USER_field(16),dip_bands_defined
 !
 integer :: i1,SCATT_bands(2),SCATT_hole_bands(2),SCATT_elec_bands(2),DIP_bands(2)
 !
 if (STRING_same(what,"USER_fields")) then
   !
   l_USER_field=.false.
   !
   call parser('BndsRnXx', l_USER_field(1))
   call parser('BndsRnXs', l_USER_field(2))
   call parser('BndsRnXd', l_USER_field(3))
   call parser('BndsRnXp', l_USER_field(4))
   call parser('BndsRnXm', l_USER_field(5))
   call parser('DipBands', l_USER_field(6))
   call parser('BSEBands', l_USER_field(7))
   call parser('RTBands',  l_USER_field(8))
   call parser('SCBands',  l_USER_field(9))
   call parser('NLBands',  l_USER_field(10))
   call parser('COLLBands',l_USER_field(11))
   call parser('el_photon_corr',l_USER_field(12))
   if (l_USER_field(12)) call parser('life',l_USER_field(12))
   call parser('QpntsRXd',l_USER_field(13))
   call parser('QpntsRXs',l_USER_field(14))
   call parser('QpntsRXx',l_USER_field(15))
   call parser('BSEQptR', l_USER_field(16))
 endif
 !
 if (STRING_same(what,"MIRROR_q")) then
   !
   ! Q's
   !-----
   if (l_USER_field(13).and.runlevel_is_on('optics')) Dip%iq=X(3)%iq
   if (l_USER_field(13).and.runlevel_is_on('em1d')  ) Dip%iq=X(3)%iq
   if (l_USER_field(13).and.runlevel_is_on('ppa')   ) Dip%iq=X(4)%iq
   if (l_USER_field(13).and.runlevel_is_on('mpa')   ) Dip%iq=X(5)%iq
   if (l_USER_field(14)                             ) Dip%iq=X(2)%iq
   if (l_USER_field(15)                             ) Dip%iq=X(1)%iq
   if (l_USER_field(16).and..not.(runlevel_is_on('ppa').or.runlevel_is_on('em1s').or.runlevel_is_on('mpa') )) Dip%iq=BS_q
  !
 endif
 !
 if (STRING_same(what,"MIRROR_bands")) then
   !
   ! Bands (Electronic levels)
   !---------------------------
   SCATT_bands=-1
   SCATT_hole_bands=-1
   SCATT_elec_bands=-1
   do i1=8,12
     if(.not.(l_USER_field(i1))) cycle
     !
#if defined _RT || defined _QED || defined _NL
     if(i1==8) then
       SCATT_bands=RT_bands
       SCATT_hole_bands=RT_hole_bands
       SCATT_elec_bands=RT_elec_bands
     endif
#endif
#if defined _SC
     if(i1==9) SCATT_bands=SC_bands
#endif
#if defined _NL
     if(i1==10) then
       SCATT_bands=NL_bands
       SCATT_hole_bands=NL_hole_bands
       SCATT_elec_bands=NL_elec_bands
     endif
#endif
#if defined _SC 
     if(i1==11) then
       SC_bands   =COLL_bands
       SCATT_bands=COLL_bands
     endif
#endif
#if defined _RT || defined _QED || defined _NL
     if(i1==11) then
       RT_bands   =COLL_bands
       RT_hole_bands   =COLL_hole_bands
       RT_elec_bands   =COLL_elec_bands
       SCATT_bands=COLL_bands
       SCATT_hole_bands=COLL_hole_bands
       SCATT_elec_bands=COLL_elec_bands
     endif
#endif
#if defined _QED
     if(i1==12) SCATT_bands=QP_n_G_bands
#endif
   enddo
   !
   ! Bands (Dipoles)
   !----------------
   DIP_bands=-1
   dip_bands_defined=.false.
   ! Start from input if present
   if( l_USER_field(6) )  call def_dip_bands(Dip%ib)
   !
   ! Test scatt bands
   if( all((/SCATT_bands/=-1/) ) ) call def_dip_bands(SCATT_bands)
   !
   ! Test other fields
   do i1=1,5
     if(.not.(l_USER_field(i1))) cycle
     call def_dip_bands(X(i1)%ib)
   enddo
   !
   if( l_USER_field(7) ) call def_dip_bands(BS_bands)
#if defined _RT || defined _QED || defined _NL
   if( l_USER_field(8) ) call def_dip_bands(RT_bands)
#endif
#if defined _NL
   if( l_USER_field(10)) call def_dip_bands(NL_bands)
#endif
   !
   ! Any negative bands?
   !---------------------
   call fix_me(SCATT_bands)
   call fix_me(DIP_bands)
   !
   ! Now MIRROR in related bands ranges
   !------------------------------------
#if defined _SC || defined _RT || defined _QED || defined _NL
   COLL_bands =SCATT_bands
   COLL_hole_bands =SCATT_hole_bands
   COLL_elec_bands =SCATT_elec_bands
   H_ref_bands=SCATT_bands     
   H_ref_hole_bands=SCATT_hole_bands     
   H_ref_elec_bands=SCATT_elec_bands     
#endif
   Dip%ib=DIP_bands
   !
 endif
 !
 contains
   !
   subroutine fix_me(bands)
     integer :: bands(2)
     if(all((/bands>0/))) return
     bands(1)=max(bands(1),1)
     if (bands(2)>0) bands(2)=min(bands(2),E%nb)
     if (bands(2)<0) bands(2)=E%nb
   end subroutine
   !
   subroutine def_dip_bands(bands_in)
       integer, intent(in)    :: bands_in(2)
       if (dip_bands_defined) then
          DIP_bands(1)=min(DIP_bands(1),bands_in(1))
          DIP_bands(2)=max(DIP_bands(2),bands_in(2))
       else
          dip_bands_defined=.true.
          DIP_bands=bands_in
       endif
   end subroutine def_dip_bands
   !
end subroutine
