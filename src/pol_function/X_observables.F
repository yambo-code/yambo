!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine X_observables(WHAT,W,keys,IQ,IG,DESC,data)
 !
 use pars,          ONLY:rZERO,cZERO,schlen,SP
 use X_m,           ONLY:X_alloc,Epsilon_ii,Joint_DOS,BS_E_sorted,Epsilon_ij,X_magnons,X_dichroism,&
&                        X_obs,N_X_obs,X_fxc,EEL,eval_alpha,eval_eels,Alpha
 use frequency,     ONLY:w_samp
 use drivers,       ONLY:l_chi,l_optics,l_bs_fxc,l_rpa_IP
 use BS_solvers,    ONLY:B_Hall
 use PHOTOLUM,      ONLY:PL,PL_init
 use BS,            ONLY:l_BS_kerr,l_BS_magnons,l_BS_anomalous_Hall,l_BS_dichroism,&
&                        l_BS_photolum,l_BS_abs,l_BS_jdos,l_BS_esort
 use stderr,         ONLY:STRING_match,STRING_split,intc,STRING_remove
 use OUTPUT,         ONLY:OUTPUT_driver
 use interfaces,     ONLY:DESC_write
 use descriptors,    ONLY:IO_desc
 !
#include<memory.h>
 !
 character(*)           :: WHAT
 type(w_samp), optional :: W
 character(*), optional :: keys
 integer,      optional :: IQ,IG
 type(IO_desc),optional :: DESC
 real(SP),     optional :: data(:)
 !
 ! Work Space
 integer            ::it,ik,ic
 character(schlen)  ::key(10)
 !
 if (WHAT=="INIT") then
   X_obs(1)%what="fxc"
   X_obs(2)%what="pl"
   X_obs(3)%what="eps"
   X_obs(4)%what="refl"
   X_obs(5)%what="eel"
   X_obs(6)%what="alpha"
   X_obs(7)%what='E_IP'
   X_obs(8)%what='Esort'
   X_obs(9)%what='off'
   X_obs(10)%what='beta'
   X_obs(11)%what='moke'
   X_obs(12)%what='jdos'
   X_obs(13)%what='Spm'
   X_obs(14)%what='Smp'
   X_obs(15)%what='CD'
   do it=1,N_X_obs
     X_obs(it)%active=.FALSE.
     X_obs(it)%prefix=X_obs(it)%what
   enddo
 endif
 !
   !X_obs(2)%active="pl"
   !X_obs(4)%active="refl"
   !X_obs(7)%active='E_IP'
   !X_obs(8)%active='Esort'
   !X_obs(9)%active='off'
   !X_obs(10)%active='beta'
   !X_obs(11)%active='moke'
   !X_obs(12)%active='jdos'
   !X_obs(13)%active='Spm'
   !X_obs(14)%active='Smp'
   !X_obs(15)%active='CD'

 write (*,*) WHAT
 !
 if (STRING_match(WHAT,"keys")) then
   key=' '
   if (present(keys)) call STRING_split(keys,key)
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(what,trim(X_obs(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     if (present(IQ)) then
       X_obs(it)%prefix=trim(X_obs(it)%prefix)//"_q"//trim(intc(IQ))
     endif
     if (present(IG)) then
       if (IG>1) X_obs(it)%prefix=trim(X_obs(it)%prefix)//"_G"//trim(intc(IG))
     endif
     do ik=1,10 
       if (len_trim(key(ik))==0) cycle
       X_obs(it)%prefix=trim(X_obs(it)%prefix)//"_"//trim(key(ik))
     enddo
   enddo
 endif
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"append").or.STRING_match(WHAT,"overwrite")) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(WHAT,trim(X_obs(it)%what)).and..not.STRING_match(WHAT,"ALL")) cycle
     if (STRING_match(WHAT,"open")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="open")  
     if (STRING_match(WHAT,"overwrite")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="overwrite")  
     if (STRING_match(WHAT,"append")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="append")  
   enddo
 endif
 !
 if (STRING_match(WHAT,"DESC").and.present(DESC)) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(WHAT,trim(X_obs(it)%what)).and..not.STRING_match(WHAT,"ALL")) cycle
     call DESC_write(trim(X_obs(it)%prefix),'#',DESC)
   enddo
 endif
 !
 if (STRING_match(WHAT,"close")) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(WHAT,trim(X_obs(it)%what)).and..not.STRING_match(WHAT,"ALL")) cycle
     call OUTPUT_driver(trim(X_obs(it)%prefix),action="close")
   enddo
 endif
 !
 if (STRING_match(WHAT,"WRITE")) then
   do it=1,N_X_obs
     if (.not.X_obs(it)%active) cycle
     if (.not.STRING_match(WHAT,trim(X_obs(it)%what)).and..not.STRING_match(WHAT,"ALL")) cycle
     call OUTPUT_driver(trim(X_obs(it)%prefix),R_VALUES=data(1:1),TITLES=X_obs(it)%COL_header(1:1),UNIT="eV")
     do ic=2,X_obs(it)%N_columns
       call OUTPUT_driver(trim(X_obs(it)%prefix),R_VALUES=data(ic:ic),TITLES=X_obs(it)%COL_header(ic:ic))
     enddo
     call OUTPUT_driver(trim(X_obs(it)%prefix),action="write")
   enddo
 endif
 !
! if (STRING_match(WHAT,"HEADERS")) then
!   do it=1,N_X_obs
!     if (.not.X_obs(it)%active) cycle
!     if (.not.STRING_match(WHAT,trim(X_obs(it)%what)).and..not.STRING_match(WHAT,"ALL")) cycle
!   enddo
! endif
 !
 if (WHAT=="ALLOC") then
   !
   if(l_BS_abs.or.l_BS_kerr.or.(l_optics.and.l_chi)) then
     call X_alloc("EPS",(/W%n_freqs,4/))
     Epsilon_ii(:,1)=W%p(:)
     X_obs(3)%active=.TRUE.
     X_obs(3)%N_columns=5 
     if (l_rpa_IP) X_obs(3)%N_columns=3
     X_obs(3)%COL_header(1)   = 'E/ev[1]'
     X_obs(3)%COL_header(2:3) = (/'Im(EPS)[2]',  'Re(EPS)[3]'/)
     X_obs(3)%COL_header(4:5)  = (/'Im(EPS_o)[4]','Re(EPS_o)[5]'/)
     X_obs(3)%COL_header(6:7)  = (/'Im(EPS_{n-1})[6]','Re(EPS_{n-1})[7]'/)
   endif
   !
   if(l_BS_kerr) then
     YAMBO_ALLOC(Epsilon_ij,(W%n_freqs,4))
     Epsilon_ij=cZERO
     Epsilon_ij(:,1)=W%p(:)
   endif
   if(l_BS_anomalous_Hall) B_Hall=cZERO
   !
   if(l_BS_magnons) then
     YAMBO_ALLOC(X_magnons,(W%n_freqs,2,4))
     X_magnons=cZERO
     X_magnons(:,1,1)=W%p(:)
     X_magnons(:,2,1)=W%p(:)
   endif
   !
   if(l_BS_dichroism) then
     YAMBO_ALLOC(X_dichroism,(W%n_freqs,4))
     X_dichroism=cZERO
     X_dichroism(:,1)=W%p(:)
   endif
   !
   if(l_BS_photolum) then
     call PL_init( )
     PL=rZERO
     PL(:,1) = W%p(:)  
   endif
   !
   if(eval_eels) then
     YAMBO_ALLOC(EEL,(W%n_freqs,2))
     EEL=cZERO
     X_obs(5)%active=.TRUE.
     X_obs(5)%N_columns=X_obs(3)%N_columns
     do ic=1,X_obs(5)%N_columns
       X_obs(5)%COL_header(ic)=STRING_remove(X_obs(3)%COL_header(ic),"EPS","EEL")
     enddo
   endif
   !
   if(eval_alpha.or.(l_optics.and.l_chi)) then
     YAMBO_ALLOC(Alpha,(W%n_freqs,2))
     Alpha=cZERO
     X_obs(6)%active=eval_alpha
     X_obs(6)%N_columns=X_obs(3)%N_columns
     do ic=1,X_obs(6)%N_columns
       X_obs(6)%COL_header(ic)=STRING_remove(X_obs(3)%COL_header(ic),"EPS","EEL")
     enddo
   endif
   !
   if(l_BS_jdos) then
     YAMBO_ALLOC(Joint_DOS,(W%n_freqs,4))
     Joint_DOS=cZERO
     Joint_DOS(:,1)=W%p(:)
   endif
   !
   if(l_BS_esort) then
     YAMBO_ALLOC(BS_E_sorted,(1000,9,2))
     BS_E_sorted=cZERO
   endif
   !
   if (l_bs_fxc) then
     YAMBO_ALLOC(X_fxc,(W%n_freqs))
     X_fxc=cZERO
     X_obs(1)%active=.TRUE.
     X_obs(1)%N_columns=2
     X_obs(1)%COL_header(1)   = 'E/ev[1]'
     X_obs(1)%COL_header(2:3) = (/'Re(q^2Fxc)[2]','Re(q^2Fxc)[3]'/)
   endif
   !
 else if (WHAT=="FREE") then
   YAMBO_FREE(Epsilon_ij)
   YAMBO_FREE(X_magnons)
   YAMBO_FREE(X_dichroism)
   YAMBO_FREE(Alpha)
   YAMBO_FREE(EEL)
   YAMBO_FREE(PL)
   call X_alloc("EPS")
   YAMBO_FREE(Joint_DOS)
   YAMBO_FREE(BS_E_sorted)
   YAMBO_FREE(X_fxc)
 endif
 !
end subroutine
