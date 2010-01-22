!
! Copyright (C) 2000-2009 A. Marini and the YAMBO team
!              http://www.yambo-code.org
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
subroutine SC_History(it,E)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HARTREE
 use electrons,      ONLY:n_met_bands,n_full_bands,levels,n_sp_pol
 use R_lattice,      ONLY:nkibz
 use com,            ONLY:of_open_close,msg
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,it_now
 implicit none
 !
 integer      :: it
 type(levels) :: E
 !
 ! Work Space
 !
 integer  :: ik,ib,i_spin,n_headers
 real(SP) :: V(SC_bands)
 character(schlen), SAVE :: file_name(2)
 character(schlen)       :: headers(SC_bands*nkibz)
 !
 do i_spin=1,n_sp_pol
   !
   if ( it == 0 ) then
     headers(1)='Iteration'
     n_headers=1
     do ik=1,nkibz
       do ib=n_met_bands-1,n_full_bands+1
         n_headers=n_headers+1
         headers(n_headers)='E_b'//trim(intc(ib))//'_k'//trim(intc(ik))
       enddo
     enddo
     !
     file_name(i_spin)='SC_History'
     if (n_sp_pol==2.and.i_spin==1) file_name(i_spin)='SC_History'//'_spin_ch_UP'
     if (n_sp_pol==2.and.i_spin==2) file_name(i_spin)='SC_History'//'_spin_ch_DN'
     !
     call of_open_close(file_name(i_spin),'ot')
     if (i_spin==1)  then
       call msg('o History','#',headers(1:n_headers),INDENT=0,USE_TABS=.TRUE.)
       call msg('o History','#')
     endif
     !
   endif
   !
   if ( it >  0 ) then
     call msg('o History','',(/real(it_now),&
&                             ((E%E(ib,ik,i_spin)*HARTREE,ib=n_met_bands-1,n_full_bands+1),ik=1,nkibz) &
&                             /),INDENT=-2,USE_TABS=.true.)
   endif
   !
   if ( it <  0 ) call of_open_close(file_name(i_spin))
   !
 enddo
 !
end subroutine SC_History
