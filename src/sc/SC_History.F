!
!        Copyright (C) 2000-2014 the YAMBO team
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
subroutine SC_History(it,E)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:n_met_bands,n_full_bands,levels,n_sp_pol
 use R_lattice,      ONLY:nkibz
 use com,            ONLY:of_open_close,msg,OF_exists
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_bands,it_now,SC_description,n_SC_descriptions
#if defined _RT
 use real_time,      ONLY:RT_step,Occ_natural_orb
 use units,          ONLY:FS2AUT
#endif
 implicit none
 !
 integer      :: it
 type(levels) :: E
 !
 ! Work Space
 !
 integer  :: ik,ib,i_spin,n_headers,i_d,max_kpt,b_range(2)
 character(schlen), SAVE :: E_file_name(2)
 character(schlen)       :: headers(SC_bands(2)*nkibz)
#if defined _RT
 character(schlen), SAVE :: F_file_name(2)
 character(schlen)       :: F_headers(SC_bands(2)*nkibz)
#endif
 !
 b_range(1)=max(n_full_bands-1,1)
 b_range(2)=b_range(1)+2
 max_kpt=min(nkibz,6) ! print only the fist 6 kpoints
 !
 do i_spin=1,n_sp_pol
   !
#if defined _RT
   !
   if ( .not. OF_exists('RT_E_History') ) then
     !
     headers(1)='Time[fs]'
#else
   !
   if ( it == 1 ) then
     !
     headers(1)='Iteration'
#endif
     n_headers=1
     do ik=1,max_kpt
       do ib=b_range(1),b_range(2)
         n_headers=n_headers+1
         headers(n_headers)='E_b'//trim(intc(ib))//'_k'//trim(intc(ik))
#if defined _RT
         F_headers(n_headers)='f_b'//trim(intc(ib))//'_k'//trim(intc(ik))
#endif
       enddo
     enddo
     !
#if defined _RT
     E_file_name(i_spin)='RT_E_History'
     F_file_name(i_spin)='RT_Occ_History'
     call of_open_close(E_file_name(i_spin),'ot')
     call of_open_close(F_file_name(i_spin),'ot')
#else
     E_file_name(i_spin)='SC_E_History'
     if (n_sp_pol==2.and.i_spin==1) E_file_name(i_spin)='SC_E_History'//'_spin_ch_UP'
     if (n_sp_pol==2.and.i_spin==2) E_file_name(i_spin)='SC_E_History'//'_spin_ch_DN'
     call of_open_close(E_file_name(i_spin),'ot')
#endif
     !
     do i_d=1,n_SC_descriptions
       if (index(SC_description(i_d),"Iteration") == 0) call msg('o History','#',trim(SC_description(i_d)),INDENT=0)
     enddo
     call msg('o History','#')
     !
     if (i_spin==1)  then
       call msg('o History','#',headers(1:n_headers),INDENT=0,USE_TABS=.TRUE.)
       call msg('o History','#')
     endif
     !
   endif
   !
   if ( it >  0 ) then
#if defined _RT
     call msg('o E_History','',(/RT_step*real(it,SP)/FS2AUT,&
#else
     call msg('o E_History','',(/real(it_now,SP),&
#endif
&                             ((E%E(ib,ik,i_spin)*HA2EV,ib=b_range(1),b_range(2)),ik=1,max_kpt) &
&                             /),INDENT=-2,USE_TABS=.true.)
#if !defined _RT
     call of_open_close(E_file_name(i_spin),'f')
#endif
     !
#if defined _RT
     call msg('o Occ_History','',(/RT_step*real(it,SP)/FS2AUT,&
&                                ((Occ_natural_orb(ib,ik),ib=n_met_bands-1,n_full_bands+1),ik=1,max_kpt) &
&                                /),INDENT=-2,USE_TABS=.true.)
#endif
   endif
   !
   if ( it <  0 ) call of_open_close(E_file_name(i_spin))
   !
 enddo
 !
end subroutine SC_History
