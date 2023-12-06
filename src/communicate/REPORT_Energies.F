!
! License-Identifier: GPL
!
! Copyright (C) 2008 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine REPORT_Energies(E,k,Xk,n_bands_,k_range,what,l_kpoints,l_what)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,nXkibz,MAX_kpts_to_report
 use electrons,      ONLY:n_sp_pol,nel,n_full_bands
 use stderr,         ONLY:intc
 use wave_func,      ONLY:wf_nc_k
 use com,            ONLY:msg,fat_log
 use vec_operate,    ONLY:c2a
 !
 implicit none
 !
 type(bz_samp),intent(in) :: k,Xk
 integer,      intent(in) :: n_bands_,k_range(2)
 real(SP),     intent(in) :: E(n_bands_,k%nibz,n_sp_pol)
 character(*), intent(in) :: what
 logical,      intent(in) :: l_kpoints,l_what
 !
 ! Work Space
 !
 real(SP)          :: k_cc(3),k_al(3)
 integer           :: ik,is_pol,ib,i1,nb_to_print
 character(schlen) :: dumb_ch1,dumb_ch2
 real(SP)          :: E_(n_bands_)
 !
 if(l_kpoints) then
   call msg('nr','IBZ '//trim(what)//'-points ',k%nibz)
   if (trim(what)/="Q".and.Xk%nibz/=k%nibz) then
     call msg('r', 'IBZ K-points for X ',nXkibz)
   endif
   call msg('rn','BZ  '//trim(what)//'-points ',k%nbz)
   call msg('r','K/Q-points units:')
   call msg('rn','rlu = crystal or reduced units; cc = cartesian coordinates; iku = interal k-units')
 endif
 !
 K_LOOP: do ik=k_range(1),k_range(2)
   !
   if (.not.fat_log.and.ik>MAX_kpts_to_report) then
     dumb_ch1=trim(intc(k%nibz-MAX_kpts_to_report))
     call msg('nrn','Remaining '//trim(dumb_ch1)//' points informations can be written running with -fatlog')
     exit K_LOOP
   endif
   !
   call c2a(v_in=k%pt(ik,:),v_out=k_al,mode='ki2a')
   call c2a(v_in=k%pt(ik,:),v_out=k_cc,mode='ki2c')
   !
   dumb_ch1=trim(intc(ik))//'] '
   i1=9
   do while (i1<k%nibz)
     if(ik<=i1) dumb_ch1=' '//trim(dumb_ch1)
     i1=i1*10+9
   enddo
   if(trim(what)=="Q") dumb_ch1='Q ['//trim(dumb_ch1)
   if(trim(what)/="Q") dumb_ch1='K ['//trim(dumb_ch1)
   !
   if (l_kpoints) then
     call msg('r',trim(dumb_ch1),k_al,'[rlu]')
     if (fat_log) call msg('r','      :',k%pt(ik,:),'[iku]')
     if (fat_log) call msg('r','      :',k_cc,'[cc]')
     call msg('r','      : weight ',k%weights(ik))
     if (trim(what)/="Q") then
       call msg('r','      : wf components',wf_nc_k(ik) )
     endif
   endif
   !
   if (l_what) then
     !
     do is_pol=1,n_sp_pol
       !
       nb_to_print=n_full_bands(is_pol)+4*int(nel)
       if (.not.fat_log) nb_to_print=2*n_full_bands(is_pol)
       !
       E_=E(:,ik,is_pol)*HA2EV
       dumb_ch2=trim(what)
       if (n_sp_pol==2) then
         if (is_pol==1) dumb_ch2=trim(what)//' [up]'
         if (is_pol==2) dumb_ch2=trim(what)//' [dn]'
       endif
       dumb_ch2=trim(dumb_ch2)//' @ '//trim(dumb_ch1)
       dumb_ch1=" "
       if (trim(what)=="E") dumb_ch1=" [eV] "
       call msg('r',trim(dumb_ch2),(/dumb_ch1/))
       do i1=1,min(n_bands_,nb_to_print),8
         call msg('r',' ',(/(E_(ib),ib=i1,min(i1+7,n_bands_))/))
       enddo
     enddo
   endif
   !
 enddo K_LOOP
 !
end subroutine
