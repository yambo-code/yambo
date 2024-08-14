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
 use real_time,     ONLY:RT_bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
 use collision_ext, ONLY:COLL_bands
 use hamiltonian,   ONLY:H_ref_bands
#endif
#if defined _NL
 use nl_optics,      ONLY:NL_bands
#endif
 !
 implicit none
 !
 character(*)   ::what
 type(levels)   ::E
 type(DIPOLE_t) ::Dip
 type(X_t)      ::X(5) 
 logical        ::l_USER_field(16)
 !
 integer :: i1,bands(2),tmp_bands(2)
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
 if (STRING_same(what,"MIRROR_Dip_and_X")) then
   !
   ! Q's
   !
   if (l_USER_field(13).and.runlevel_is_on('optics')) Dip%iq=X(3)%iq
   if (l_USER_field(13).and.runlevel_is_on('em1d')  ) Dip%iq=X(3)%iq
   if (l_USER_field(13).and.runlevel_is_on('ppa')   ) Dip%iq=X(4)%iq
   if (l_USER_field(13).and.runlevel_is_on('mpa')   ) Dip%iq=X(5)%iq
   if (l_USER_field(14)                             ) Dip%iq=X(2)%iq
   if (l_USER_field(15)                             ) Dip%iq=X(1)%iq
   if (l_USER_field(16).and..not.(runlevel_is_on('ppa').or.runlevel_is_on('em1s').or.runlevel_is_on('mpa') )) Dip%iq=BS_q
   !
   ! Bands
   !
   bands=-1
   do i1=1,11
     if(.not.(l_USER_field(i1))) cycle
     if(i1< 6) tmp_bands=X(i1)%ib
     if(i1==6) tmp_bands=Dip%ib
     if(i1==7) tmp_bands=BS_bands
#if defined _RT || defined _QED || defined _NL
     if(i1==8) tmp_bands=RT_bands
#endif
#if defined _SC
     if(i1==9) tmp_bands=SC_bands
#endif
#if defined _NL
     if(i1==10) tmp_bands=NL_bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
     if(i1==11) tmp_bands=COLL_bands
#endif
#if defined _QED
   if(i1==12) tmp_bands=QP_n_G_bands
#endif
     if(any((/bands==-1/))) then
       bands(1)=max(tmp_bands(1),   1   )
       bands(2)=min(tmp_bands(2),E%nb)
     else
       bands(1)=max(min(tmp_bands(1),bands(1)),   1   )
       bands(2)=min(max(tmp_bands(2),bands(2)),E%nb)
     endif
   enddo
   !
   if (any(bands<1)) bands=(/1,E%nb/)
   !
   Dip%ib=bands
   !
 endif
 !
 if (STRING_same(what,"MIRROR_bands")) then
   !
#if defined _SC 
   if (l_USER_field(9)) bands=SC_bands
#endif
#if defined _RT || defined _QED || defined _NL
   if (l_USER_field(8)) bands=RT_bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
   if (l_USER_field(11)) bands=COLL_bands
#endif
#if defined _QED
   if (l_USER_field(12)) bands=QP_n_G_bands
#endif
#if defined _NL
   if (l_USER_field(10)) bands=NL_bands
#endif
   !
#if defined _SC 
   SC_bands  =bands
#endif
#if defined _RT || defined _QED
   RT_bands  =bands
#endif
#if defined _NL
   NL_bands  =bands
#endif
#if defined _SC || defined _RT || defined _QED || defined _NL
   COLL_bands =bands
   H_ref_bands=bands     
#endif
 endif
 !
end subroutine
