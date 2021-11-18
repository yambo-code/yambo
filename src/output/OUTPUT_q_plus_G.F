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
subroutine OUTPUT_q_plus_G(iq,Q_plus_G_pt,ig,Qprime)
 !
 use pars,      ONLY:SP,schlen,msg_len
 use X_m,       ONLY:alpha_dim
 use com,       ONLY:msg,com_compose_msg
 use stderr,    ONLY:intc
 !
 implicit none
 !
 integer            :: iq
 real(SP)           :: Q_plus_G_pt(3)
 character(msg_len) :: message(13)
 character(schlen)  :: unit_msg
 integer,  optional :: ig
 real(SP), optional :: Qprime(3)
 !
 if (iq==1.and.ig==1) then
   message(1) = com_compose_msg(pre='# ',body='Absorption @ Q('//trim(intc(iq))//')')
   message(2) = com_compose_msg(pre='# ',body='EELS @ Q('//trim(intc(iq))//')')
   message(3) = com_compose_msg(pre='# ',&
                body='Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//')')
   message(4) = com_compose_msg(pre='# ',body='F_xc @ Q('//trim(intc(iq))//')')
   message(5) = com_compose_msg(pre='# ',body='Off-diagonal Absorption @ Q('//trim(intc(iq))//')')
   message(6) = com_compose_msg(pre='# ',body='Off-diagonal Polarizability @ Q('//trim(intc(iq))//')')
   message(7) = com_compose_msg(pre='# ',body='moke parameters @ Q('//trim(intc(iq))//')')
   message(8) = com_compose_msg(pre='# ',body='Reflectivity @ Q('//trim(intc(iq))//')')
   unit_msg="[q->0 direction]"
   if (present(Qprime)) then
     message(12) = com_compose_msg(pre='# ',body='Off-diagonal Absorption @ Q`('//trim(intc(iq))//')')
     message(13) = com_compose_msg(pre='# ',body='Off-diagonal Polarizability @ Q`('//trim(intc(iq))//')')
   endif
 else  if (iq/=1.and.ig==1) then
   message(1) = com_compose_msg(pre='# ',body='Absorption @ Q('//trim(intc(iq))//')')
   message(2) = com_compose_msg(pre='# ',body='EELS @ Q('//trim(intc(iq))//')')
   message(3) = com_compose_msg(pre='# ',body='Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//')')
   message(4) = com_compose_msg(pre='# ',body='F_xc @ Q('//trim(intc(iq))//')')
   unit_msg="[iku]"
 else  if (ig/=1) then
   message(1) = com_compose_msg(pre='# ',body='Absorption @ Q('//trim(intc(iq))//') + G('//trim(intc(ig))//')')
   message(2) = com_compose_msg(pre='# ',body='EELS @ Q('//trim(intc(iq))//')  + G('//trim(intc(ig))//'')
   message(3) = com_compose_msg(pre='# ',body='Polarizability ( -<<X>> ) ['//trim(alpha_dim)//&
                                ' @ Q('//trim(intc(iq))//')  + G('//trim(intc(ig))//')')
   message(4) = com_compose_msg(pre='# ',body='F_xc @ Q('//trim(intc(iq))//')')
   unit_msg="[iku]"
 endif
 !
 call msg("o eps_q",trim(message(1)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o eel_q",trim(message(2)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o alpha",trim(message(3)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o fxc",trim(message(4)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o off",trim(message(5)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o beta",trim(message(6)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 if (present(Qprime)) then
   call msg("o off",trim(message(12)),Qprime,INDENT=0,after_msg=trim(unit_msg))
   call msg("o beta",trim(message(13)),Qprime,INDENT=0,after_msg=trim(unit_msg))
 endif
 call msg("o moke",trim(message(7)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 call msg("o refl",trim(message(8)),Q_plus_G_pt,INDENT=0,after_msg=trim(unit_msg))
 !
 message(9) = ' Joint density of states @ Q('//trim(intc(iq))//')'
 message(10)= ' Magnons (S+-) @ Q('//trim(intc(iq))//')'
 message(11)= ' Magnons (S-+) @ Q('//trim(intc(iq))//')'
 !
 call msg("o jdos","#",trim(message(9)),INDENT=0)
 call msg("o Spm" ,"#",trim(message(10)),INDENT=0)
 call msg("o Smp" ,"#",trim(message(11)),INDENT=0)
 !
 call msg('o eps refl eel fxc alpha off moke jdos beta Spm Smp CD','#')
 !
end subroutine
