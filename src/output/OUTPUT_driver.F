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
subroutine OUTPUT_driver(what,keys,IQ,IG)
 !
 use pars,           ONLY:schlen
 use OUTPUT,         ONLY:OUT_data,N_OUT_data
 use stderr,         ONLY:STRING_match,STRING_split,intc
 use com,            ONLY:of_open_close
 !
 implicit none
 !
 character(*)           :: what
 character(*), optional :: keys
 integer,      optional :: IQ,IG
 !
 ! Workspace
 integer            ::it,ik
 character(schlen)  ::key(10)
 !
 if (STRING_match(what,"def")) then
   OUT_data(1)%what="fxc"
   OUT_data(2)%what="pl"
   OUT_data(3)%what="eps"
   OUT_data(4)%what="refl"
   OUT_data(5)%what="eel"
   OUT_data(6)%what="alpha"
   OUT_data(7)%what='E_IP'
   OUT_data(8)%what='Esort'
   OUT_data(9)%what='off'
   OUT_data(10)%what='beta'
   OUT_data(11)%what='moke'
   OUT_data(12)%what='jdos'
   OUT_data(13)%what='Spm'
   OUT_data(14)%what='Smp'
   OUT_data(15)%what='CD'
   do it=1,N_OUT_data
     OUT_data(it)%active=.FALSE.
     OUT_data(it)%file=OUT_data(it)%what
   enddo
 endif
 !
 if (STRING_match(what,"activate")) then
   do it=1,N_OUT_data
     if (.not.STRING_match(what,trim(OUT_data(it)%what))) cycle
     OUT_data(it)%active=.TRUE.
   enddo
 endif
 !
 if (STRING_match(what,"open")) then
   do it=1,N_OUT_data
     if (.not.OUT_data(it)%active) cycle
     if (.not.STRING_match(what,trim(OUT_data(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     if (STRING_match(what,"Ot")) call of_open_close(OUT_data(it)%file,'Ot')
     if (STRING_match(what,"ot")) call of_open_close(OUT_data(it)%file,'ot')
     if (STRING_match(what,"oa")) call of_open_close(OUT_data(it)%file,'oa')
   enddo
 endif
 !
 if (STRING_match(what,"close")) then
   do it=1,N_OUT_data
     if (.not.OUT_data(it)%active) cycle
     if (.not.STRING_match(what,trim(OUT_data(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     call of_open_close(OUT_data(it)%file)
   enddo
 endif
 !
 if (STRING_match(what,"keys").and.present(keys)) then
   call STRING_split(keys,key)
   do it=1,N_OUT_data
     if (.not.OUT_data(it)%active) cycle
     if (.not.STRING_match(what,trim(OUT_data(it)%what)).and..not.STRING_match(what,"ALL")) cycle
     if (present(IQ)) then
       OUT_data(it)%file=trim(OUT_data(it)%file)//"_q"//trim(intc(IQ))
     endif
     if (present(IG)) then
       if (IG>1) OUT_data(it)%file=trim(OUT_data(it)%file)//"_G"//trim(intc(IG))
     endif
     do ik=1,10 
       if (len_trim(key(ik))==0) cycle
       OUT_data(it)%file=trim(OUT_data(it)%file)//"_"//trim(key(ik))
     enddo
   enddo
 endif
 !
 if (STRING_match(what,"reset")) then
   do it=1,N_OUT_data
     OUT_data(it)%active=.FALSE.
     OUT_data(it)%file=OUT_data(it)%what
   enddo
 endif
 !
end subroutine
