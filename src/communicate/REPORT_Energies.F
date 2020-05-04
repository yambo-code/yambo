!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine REPORT_Energies(E,k,Xk,n_bands_,k_range,what,l_kpoints,l_what)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,nXkibz
 use electrons,      ONLY:n_sp_pol,nel,n_full_bands
 use stderr,         ONLY:intc
 use wave_func,      ONLY:wf_nc_k
 use com,            ONLY:msg,verbose
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
 real(SP)          :: k_cc(3),k_rlu(3)
 integer           :: ik,is_pol,ib,i1
 character(schlen) :: dumb_ch1,dumb_ch2,dumb_ch3
 real(SP)          :: E_(n_bands_,n_sp_pol)
 !
 if(l_kpoints) then
   call msg('nr','IBZ '//trim(what)//'-points ',k%nibz)
   call msg('rn','BZ  '//trim(what)//'-points ',k%nbz)
 endif
 !
 do ik=k_range(1),k_range(2)
   !
   !call msg('r','')
   !
   call c2a(v_in=k%pt(ik,:),v_out=k_rlu,mode='ki2a')
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
     if (ik<=nXkibz .or. trim(what)=="Q") call msg('r',trim(dumb_ch1)//' : weight       ',Xk%weights(ik) )
     if(trim(what)=="K") call msg('r','      : wf components',wf_nc_k(ik) )
     if (ik<=nXkibz .or. (trim(what)=="Q")) then
       if(verbose) call msg('r','      : point-cc     ',k_cc,' [cc] ')
                   call msg('r','      : point-rlu    ',k_rlu,' [rlu] ')
       if(verbose) call msg('r','      : point-iku    ',k%pt(ik,:),' [iku] ')
     endif
   endif
   !
   if (l_what) then
     E_(:,:)=E(:,ik,:)*HA2EV
     do is_pol=1,n_sp_pol
       dumb_ch2=trim(what)
       if (n_sp_pol==2) then
         if (is_pol==1) dumb_ch2=trim(what)//' [up]'
         if (is_pol==2) dumb_ch2=trim(what)//' [dn]'
       endif
       dumb_ch2=trim(dumb_ch2)//' @ '//trim(dumb_ch1)
       dumb_ch1=" "
       if (trim(what)=="E") dumb_ch1=" [eV] "
       call msg('r',trim(dumb_ch2),(/dumb_ch1/))
       do i1=1,min(n_bands_,n_full_bands+4*int(nel)),8
         call msg('r',' ',(/(E_(ib,is_pol),ib=i1,min(i1+7,n_bands_))/))
       enddo
     enddo
   endif
   !
 enddo
 !
end subroutine
