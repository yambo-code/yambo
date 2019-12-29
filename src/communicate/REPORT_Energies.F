!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine REPORT_Energies(E,k,Xk,n_bands_,k_range,what,header)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,nXkibz
 use electrons,      ONLY:n_sp_pol,nel,n_full_bands
 use stderr,         ONLY:intc,real2ch
 use wave_func,      ONLY:wf_nc_k
 use com,            ONLY:msg
 use vec_operate,    ONLY:c2a
 !
 implicit none
 !
 type(bz_samp) :: k,Xk
 integer       :: n_bands_,k_range(2)
 real(SP)      :: E(n_bands_,k%nibz,n_sp_pol)
 real(SP)      :: k_cc(3),k_al(3)
 logical       :: header
 character(*)  :: what
 !
 ! Work Space
 !
 integer           :: ik,is_pol,ib,i1
 character(schlen) :: dumb_ch1,dumb_ch2
 real(SP)          :: E_(n_bands_,n_sp_pol)
 !
 do ik=k_range(1),k_range(2)
   !
   call c2a(v_in=k%pt(ik,:),v_out=k_al,mode='ki2a')
   call c2a(v_in=k%pt(ik,:),v_out=k_cc,mode='ki2c')
   ! 
   dumb_ch1='K ['
   if(ik>   9) dumb_ch1=trim(dumb_ch1)//' '
   if(ik>  99) dumb_ch1=trim(dumb_ch1)//' '
   if(ik> 999) dumb_ch1=trim(dumb_ch1)//' '
   if(ik>9999) dumb_ch1=trim(dumb_ch1)//' '
   dumb_ch1=trim(dumb_ch1)//trim(intc(ik))//'] '
   !
   if (header) then
     call msg('r',trim(dumb_ch1)//' weight      ',Xk%weights(ik) )
     call msg('r',trim(dumb_ch1)//' components  ',wf_nc_k(ik) )
     if (ik> 1    ) call msg('r','')
     if (ik<=nXkibz) then
       call msg('r',trim(dumb_ch1)//' point (cc)    ',k_cc)
       call msg('r',trim(dumb_ch1)//' point (alat)  ',k_al)
     else
       call msg('r',trim(dumb_ch1))
       call msg('r',trim(dumb_ch2))
     endif
   endif
   !
   E_(:,:)=E(:,ik,:)*HA2EV
   !
   do is_pol=1,n_sp_pol
     dumb_ch1=trim(what)
     if (n_sp_pol==1) then
       if (is_pol==1) dumb_ch1=trim(what)//' [up]'
       if (is_pol==2) dumb_ch1=trim(what)//' [dn]'
     endif
     call msg('r','list ',trim(dumb_ch1))
     !
     do i1=1,min(n_bands_,n_full_bands+4*int(nel)),8
       call msg('r',' ',(/(E_(ib,1),ib=i1,min(i1+7,n_bands_))/))
     enddo
   enddo
   !
 enddo
 !
end subroutine
