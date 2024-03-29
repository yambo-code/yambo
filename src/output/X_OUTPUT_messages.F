!
! License-Identifier: GPL
!
! Copyright (C) 2021 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine X_OUTPUT_messages(iq,ig,Vnl,GF,ordering,Q_plus_G,MORE)
 !
 use pars,           ONLY:schlen,SP
 use TDDFT,          ONLY:FXC_n_g_corr
 use com,            ONLY:msg,com_compose_msg
 use X_m,            ONLY:global_gauge,X_obs,alpha_dim,N_X_obs,X_ng
 use global_XC,      ONLY:X_E_xc_string
 use stderr,         ONLY:intc,STRING_match
 use BS,             ONLY:BSE_dipole_geometry
 !
 implicit none
 !
 integer                :: iq
 integer,      optional :: ig
 logical,      optional :: Vnl
 logical,      optional :: GF
 character(2), optional :: ordering
 real(SP),     optional :: Q_plus_G(3)
 character(*), optional :: MORE
 !
 ! WorkSpace
 integer           :: it,N
 character(schlen) :: wk
 !
 do it=1,N_X_obs
   !
   if (.not.X_obs(it)%active) cycle
   !
   if (present(Q_plus_G)) then
     if (ig==1) then
       wk=com_compose_msg(pre='# ',body=trim(X_obs(it)%title)//' @ Q('//trim(intc(iq))//')')
       if (iq==1) call msg("o "//trim(X_obs(it)%prefix),trim(wk),Q_plus_G,INDENT=0,after_msg="[q->0 direction]")
       if (iq> 1) call msg("o "//trim(X_obs(it)%prefix),trim(wk),Q_plus_G,INDENT=0,after_msg="[iku]")
       if (STRING_match(X_obs(it)%what,"off").or.STRING_match(X_obs(it)%what,"beta").or.&
&          STRING_match(X_obs(it)%what,"moke")) then
         call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[DIP] Rotation plane"),&
&                 trim(BSE_dipole_geometry),INDENT=0)
       endif
     else
       wk=com_compose_msg(pre='# ',body=trim(X_obs(it)%title)//' @Q('//trim(intc(iq))//') + G('//trim(intc(ig))//')')
       call msg("o "//trim(X_obs(it)%prefix),trim(wk),Q_plus_G,INDENT=0)
     endif
     call msg("o "//trim(X_obs(it)%prefix),"#")
     if (STRING_match(X_obs(it)%what,"alpha")) then
       call msg("o "//trim(X_obs(it)%prefix),"# Polarizability units",alpha_dim,INDENT=0)
       call msg("o "//trim(X_obs(it)%prefix),"#")
     endif
     if (STRING_match(X_obs(it)%what,"moke")) then
       call msg("o "//trim(X_obs(it)%prefix),"# Moke units","Radiants",INDENT=0)
       call msg("o "//trim(X_obs(it)%prefix),"#")
     endif
     if (STRING_match(X_obs(it)%what,"beta")) then
       call msg("o "//trim(X_obs(it)%prefix),"# Beta units",alpha_dim,INDENT=0)
       call msg("o "//trim(X_obs(it)%prefix),"#")
     endif
   endif
   !
   if (present(GF)) then
     if (GF) then
       call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[GEN] GF poles"),trim(X_E_xc_string(4)),INDENT=0)
     else
       call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[GEN] GF Energies"),trim(X_E_xc_string(4)),INDENT=0)
     endif
   endif
   !
   if (present(ordering)) then
     wk='T-ordered'
     if (trim(ordering)=='Ra') wk='Retarded-antiresonant'
     if (trim(ordering)=='Ta') wk='T-ordered-antiresonant'
     if (trim(ordering)=='r')  wk='Resonant'
     if (trim(ordering)=='R')  wk='Retarded'
     call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[GEN] Green`s Function"),trim(wk),INDENT=0)
   endif
   !
   if (present(Vnl).and.iq==1) then
     if (trim(global_gauge)=='length')         wk='Length'
     if (trim(global_gauge)=='velocity')       wk='Velocity'
     call msg("o "//trim(X_obs(it)%prefix),"#")
     call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[GEN] Gauge"),trim(wk),INDENT=0)
     call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[GEN] [r,Vnl] included"),Vnl,INDENT=0)
   endif
   !
   if (present(MORE)) then
     if (STRING_match(MORE,"Diamagnetic")) then
       call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="Diamagnetic term")," ",INDENT=0)
     endif
     if (STRING_match(MORE,"FXC_size")) then
       call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[FXC] G-size"),FXC_n_g_corr,INDENT=0)
     endif
     if (STRING_match(MORE,"X_size")) then
       call msg("o "//trim(X_obs(it)%prefix),com_compose_msg(pre="# ",body="[ X ] Hartree size"),X_ng,INDENT=0)
     endif
   endif
   !
 enddo
 !
end subroutine
