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
subroutine INIT_read_command_line_corr_scatt(KIND,STR,POS,l_ee,l_ep,l_pe,l_eh)
 !
 use it_m,           ONLY:nrnlvls
 use pars,           ONLY:schlen
 use stderr,         ONLY:STRING_split,STRING_same
 !
 implicit none
 !
 character(*)       ::KIND
 character(schlen)  ::STR(2*nrnlvls)
 integer            ::POS
 logical            ::l_ee,l_ep,l_pe,l_eh
 !
 ! Work Space
 !
 character(schlen)  ::STR_pieces(10),suffix
 integer            ::i_f
 !
 if ( .not. trim(STR(POS)) ==  KIND ) return
 !
 if (KIND=="corrtp") suffix="corr"
 if (KIND=="scattp") suffix="scatt"
 !
 call STRING_split(STR(POS+1),STR_pieces,space="+")
 !
 do i_f=1,10
   if (len_trim(STR_pieces(i_f))==0) cycle
   if ( STRING_same(STR_pieces(i_f),"ee")) l_ee=.true.
   if ( STRING_same(STR_pieces(i_f),"ep")) l_ep=.true.
   if ( STRING_same(STR_pieces(i_f),"pe")) l_pe=.true.
   if ( STRING_same(STR_pieces(i_f),"eh")) l_eh=.true.
 enddo
 !
 if (.not.any((/l_ep,l_pe,l_eh,l_ee/))) l_ee=.true.
 !
 if (l_ee)     call initactivate(1,'el_el_'//trim(suffix))
 if (l_ep)     call initactivate(1,'el_ph_'//trim(suffix))
 if (l_pe)     call initactivate(1,'ph_el_'//trim(suffix))
 if (l_eh)     call initactivate(1,'el_photon_'//trim(suffix))
 !
 !
end subroutine
