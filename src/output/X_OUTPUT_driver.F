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
subroutine X_OUTPUT_driver(WHAT,keys,IQ,IG,DESC,data)
 !
 use pars,          ONLY:schlen,SP
 use X_m,           ONLY:X_obs,N_X_obs,EEL,alpha_dim
 use drivers,       ONLY:l_chi,l_optics,l_bs_fxc,l_rpa_IP,l_col_cut
 use electrons,     ONLY:n_spinor
 use PHOTOLUM,      ONLY:PL
 use BS,            ONLY:l_BS_kerr,l_BS_magnons,l_BS_dichroism,&
&                        l_BS_photolum,l_BS_abs,l_BS_jdos,l_BS_esort
 use stderr,         ONLY:STRING_match,STRING_split,intc,STRING_remove
 use OUTPUT,         ONLY:OUTPUT_driver
 use interfaces,     ONLY:DESC_write
 use descriptors,    ONLY:IO_desc
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 character(*)           :: WHAT
 character(*), optional :: keys
 integer,      optional :: IQ,IG
 type(IO_desc),optional :: DESC
 real(SP),     optional :: data(:)
 !
 ! Work Space
 integer            ::it,ik,ic
 character(schlen)  ::key(10),BASIC_col(7)
 !
 if (WHAT=="DEF") then
   call ELEMENTAL_init(1,"fxc","Exchange-Correlation Kernel","optics")
   call ELEMENTAL_init(2,"pl","Photoluminescence","PL")
   call ELEMENTAL_init(3,"eps","Absorption","optics")
   call ELEMENTAL_init(4,"refl","Reflectivity","optics")
   call ELEMENTAL_init(5,"eel","Electron Energy Loss","optics")
   call ELEMENTAL_init(6,"alpha","Polarizability  ( -<<X>> )","optics")
   call ELEMENTAL_init(7,"E_IP","IP Energies","optics")
   call ELEMENTAL_init(8,"Esort","Sorted Energies","optics")
   call ELEMENTAL_init(9,"off","Off-diagonal Absorption","kerr")
   call ELEMENTAL_init(10,"beta","Off-diagonal Polarizability","kerr")
   call ELEMENTAL_init(11,"moke","MOKE parameters","kerr")
   call ELEMENTAL_init(12,"jdos","Joint Density of states","optics")
   call ELEMENTAL_init(13,"Spm","Magnons (S+-)","magnons")
   call ELEMENTAL_init(14,"Smp","Magnons (S-p)","magnons")
   call ELEMENTAL_init(15,"DC","Dichroism","dichroism")
 endif
 !
 if (WHAT=="INIT") then
   !
   ! Logicals activation
   X_obs(1)%active =l_bs_fxc
   X_obs(2)%active =l_BS_photolum
   X_obs(3)%active =l_BS_abs.or.l_BS_kerr.or.(l_optics.and.l_chi)
   !
   if (l_BS_abs) then
     call parser('Reflectivity',X_obs(4)%active)
     if (X_obs(6)%active) X_obs(4)%active=.FALSE.
   endif
   !
   ! Check if EELS(5)/ALPHA(6) can be evaluated
   X_obs(5)%active=.TRUE.
   if (l_col_cut) X_obs(5)%active=.FALSE.
   if (trim(alpha_dim)=="volume") X_obs(5)%active=.FALSE.
   !
   X_obs(6)%active=trim(alpha_dim)/='adim'
   !
   X_obs(8)%active =l_BS_esort
   X_obs(9)%active =l_BS_kerr
   !
   X_obs(10)%active = X_obs(6)%active .and. l_BS_kerr
   X_obs(11)%active = .not.X_obs(6)%active .and. l_BS_kerr
   !
   X_obs(12)%active=l_BS_jdos
   X_obs(13)%active=l_BS_magnons
   X_obs(14)%active=l_BS_magnons.and.n_spinor==2
   X_obs(15)%active=l_BS_dichroism
   !
   ! Headers
   !
   if (X_obs(1)%active) then
     X_obs(1)%N_columns=2
     X_obs(1)%COL_header(1)   = 'E/ev[1]'
     X_obs(1)%COL_header(2:3) = (/'Re(q^2Fxc)[2]','Re(q^2Fxc)[3]'/)
   endif
   !
   BASIC_col(1)   = 'E/ev[1]'
   BASIC_col(2:3) = (/'Im(XXX)[2]', 'Re(XXX)[3]'/)
   BASIC_col(4:5) = (/'Im(XXXo)[4]', 'Re(XXXo)[5]'/)
   BASIC_col(6:7) = (/'Im(XXX`)[6]','Re(XXX`)[7]'/)
   !
   if (X_obs(3)%active) then
     X_obs(3)%N_columns=7 
     do ic=1,X_obs(3)%N_columns
       X_obs(3)%COL_header(ic)=STRING_remove(BASIC_col(ic),"XXX","EPS")
     enddo
   endif
   !
   if (X_obs(5)%active) then
     X_obs(5)%N_columns=7 
     do ic=1,X_obs(5)%N_columns
       X_obs(5)%COL_header(ic)=STRING_remove(BASIC_col(ic),"XXX","EEL")
     enddo
   endif
   !
   if (X_obs(6)%active) then
     X_obs(6)%N_columns=7 
     do ic=1,X_obs(6)%N_columns
       X_obs(6)%COL_header(ic)=STRING_remove(BASIC_col(ic),"XXX","ALPHA")
     enddo
   endif
   !
   return
   !
 endif
 !
 if (STRING_match(WHAT,"OFF").or.STRING_match(WHAT,"ON")) then
   do it=1,N_X_obs
     if (STRING_match(WHAT,trim(X_obs(it)%what)).and.STRING_match(WHAT,"ON" )) X_obs(it)%active=.TRUE.
     if (STRING_match(WHAT,trim(X_obs(it)%what)).and.STRING_match(WHAT,"OFF")) X_obs(it)%active=.FALSE.
   enddo
   return
 endif
 !
 if (STRING_match(WHAT,"keys")) then
   key=' '
   if (present(keys)) call STRING_split(keys,key)
   do it=1,N_X_obs
     if (.not.IS_this_ok(it)) cycle
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
   return
 endif
 !
 if (STRING_match(WHAT,"open").or.STRING_match(WHAT,"append").or.STRING_match(WHAT,"override")) then
   do it=1,N_X_obs
     if (.not.IS_this_ok(it)) cycle
     if (STRING_match(WHAT,"open")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="open")  
     if (STRING_match(WHAT,"override")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="override")  
     if (STRING_match(WHAT,"append")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="append")  
   enddo
 endif
 !
 if ((STRING_match(WHAT,"DESC").and.present(DESC)).or.STRING_match(WHAT,"close").or.STRING_match(WHAT,"reset")) then
   do it=1,N_X_obs
     if (.not.IS_this_ok(it)) cycle
     if (STRING_match(WHAT,"DESC" )) call DESC_write("o "//trim(X_obs(it)%prefix),'#',DESC)
     if (STRING_match(WHAT,"close")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="close")
     if (STRING_match(WHAT,"reset")) call OUTPUT_driver(trim(X_obs(it)%prefix),action="reset")
   enddo
 endif
 !
 if (STRING_match(WHAT,"WRITE")) then
   do it=1,N_X_obs
     if (.not.IS_this_ok(it)) cycle
     call OUTPUT_driver(trim(X_obs(it)%prefix),R_VALUES=data(1:1),TITLES=X_obs(it)%COL_header(1:1),UNIT="eV")
     do ic=2,size(data)
       call OUTPUT_driver(trim(X_obs(it)%prefix),R_VALUES=data(ic:ic),TITLES=X_obs(it)%COL_header(ic:ic))
     enddo
     call OUTPUT_driver(trim(X_obs(it)%prefix),action="write")
   enddo
 endif
 !
 contains
   !
   logical function IS_this_ok(it)
     integer  :: it
     IS_this_ok=X_obs(it)%active
     if (.not.IS_this_ok) return
     if (.not.STRING_match(WHAT,trim(X_obs(it)%what) )) IS_this_ok=.FALSE.
     if (STRING_match(WHAT,trim(X_obs(it)%scattering))) IS_this_ok=.TRUE.
     if (STRING_match(WHAT,"ALL")) IS_this_ok=.TRUE.
   end function
   !
   subroutine ELEMENTAL_init(it,short,long,scattering)
     integer      :: it
     character(*) :: short,long,scattering
     X_obs(it)%what=short
     X_obs(it)%title=long
     X_obs(it)%scattering=scattering
     X_obs(it)%N_messages=0
     X_obs(it)%N_columns=0
     X_obs(it)%active=.FALSE.
     X_obs(it)%prefix=X_obs(it)%what
   end subroutine
 !
end subroutine
