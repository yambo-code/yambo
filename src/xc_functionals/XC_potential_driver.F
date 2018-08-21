!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): MG AM
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
subroutine XC_potential_driver(en,Xk,KIND,FUNCTIONAL,ORDER,EXX_FRACTION,MUTE,unMUTE,EXX_SCREENING)
 !
 ! Wrapper for calculation of the XC potential. If available libxc,
 ! will be used instead of internal routines
 !
 use pars,           ONLY:SP,rZERO,schlen
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels
 use com,            ONLY:msg
 use libxc_funcs_m,  ONLY:XC_LDA_X, XC_LDA_C_PW
 use xc_functionals, ONLY:XC_EXCHANGE_CORRELATION,xc_string,XC_libxc_driver,&
&                         XC_NOT_AVAILABLE,XC_FACTOR,current_xc_string,LIBXC_message_done
 use IO_m,           ONLY:IO_and_Messaging_switch
 !
 implicit none
 !
 type(levels),  intent(in)       ::en
 type(bz_samp), intent(in)       ::Xk
 integer,       intent(inout)    :: KIND,FUNCTIONAL
 integer,       intent(in)       :: ORDER
 real(SP),      intent(inout),optional :: EXX_FRACTION, EXX_SCREENING
 character(*),  intent(in),   optional :: MUTE
 logical,       intent(in),   optional :: unMUTE
 !
 ! Work space
 !
 integer           :: ic
 real(SP)          :: local_exx_fraction, local_exx_screening
 character(schlen) :: local_mute_ch
 !
 if (present(MUTE)) call IO_and_Messaging_switch(MUTE)
 !
 local_exx_fraction = rZERO
 local_exx_screening= rZERO
 if (present(EXX_FRACTION))  local_exx_fraction  = EXX_FRACTION
 if (present(EXX_SCREENING)) local_exx_screening = EXX_SCREENING
 !
 ! COM
 !
 if (KIND>0.and.FUNCTIONAL>0) then
   if (current_xc_string/=xc_string(FUNCTIONAL)) then
     call msg('rsn','[xc] Functional ',xc_string(FUNCTIONAL))
     current_xc_string=xc_string(FUNCTIONAL)
   endif
 else if (FUNCTIONAL==XC_NOT_AVAILABLE) then
   KIND=XC_EXCHANGE_CORRELATION
   FUNCTIONAL=XC_LDA_X*XC_FACTOR+XC_LDA_C_PW
   call msg('rsn','[xc] Functional unknown. Used ',xc_string(FUNCTIONAL))
 endif
 !
 ! Maybe also (spin)density should be calculated in the wrapper, since it does 
 ! not use anything of libxc.   
 !
 if (.not.LIBXC_message_done) then
   call msg('rsn','[xc] LIBXC used to calculate xc functional ')
   LIBXC_message_done=.TRUE.
 endif
 !
 call XC_libxc_driver(en,Xk,FUNCTIONAL,ORDER,EXX_FRACTION=local_exx_fraction,&
&                     EXX_SCREENING=local_exx_screening)
 if (local_exx_fraction.gt.rZERO) then
   if (present(EXX_FRACTION)) EXX_FRACTION = local_exx_fraction 
   call msg('rsn','[xc] Hybrid functional. EXX fraction: ',EXX_FRACTION)
 endif
 !
 if (local_exx_screening.gt.rZERO) then
   if (present(EXX_FRACTION)) EXX_SCREENING = local_exx_screening 
   call msg('rsn','[xc] Hybrid functional. EXX screening: ',EXX_SCREENING)
 endif
 !
 if (present(MUTE)) then
   local_mute_ch=MUTE
   do ic=1,len_trim(local_mute_ch)
     if (local_mute_ch(ic:ic)=="-") local_mute_ch(ic:ic)="+"
   enddo   
   if (present(unMUTE)) then
     call IO_and_Messaging_switch(trim(local_mute_ch),CONDITION=unMUTE)
   else
     call IO_and_Messaging_switch(MUTE)
   endif
 endif
 !
end subroutine XC_potential_driver
