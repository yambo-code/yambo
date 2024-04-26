!
! License-Identifier: GPL
!
! Copyright (C) 2010 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine SC_History(it,E)
 !
 use pars,           ONLY:schlen,SP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:n_full_bands,levels,n_sp_pol
 use R_lattice,      ONLY:nkibz
 use com,            ONLY:of_open_close,msg
 use stderr,         ONLY:intc
 use SC,             ONLY:SC_nbands,it_now,SC_desc
 use interfaces,     ONLY:DESC_write
 !
 implicit none
 !
 integer      :: it
 type(levels) :: E
 !
 ! Work Space
 !
 integer  :: ik,ib,i_spin,n_headers,max_kpt,b_range(2)
 character(schlen), SAVE :: E_file_name(2)
 character(schlen)       :: headers(SC_nbands*nkibz)
 !
 max_kpt=min(nkibz,6) ! print only the fist 6 kpoints
 !
 do i_spin=1,n_sp_pol
   !
   ! 2021/11/09 DS
   ! The commented lin is more correct for the definition of b_range(1).
   ! However the other one is kept to avoid problems with the test-suite
   !b_range(1)=max(n_full_bands(i_spin)-1,1)
   !
   b_range(1)=max(minval(n_full_bands)-1,1)
   b_range(2)=b_range(1)+2
   !
   if ( it == 1 ) then
     !
     headers(1)='Iteration'
     n_headers=1
     do ik=1,max_kpt
       do ib=b_range(1),b_range(2)
         n_headers=n_headers+1
         headers(n_headers)='E_b'//trim(intc(ib))//'_k'//trim(intc(ik))
       enddo
     enddo
     !
     E_file_name(i_spin)='SC_E_History'
     if (n_sp_pol==2.and.i_spin==1) E_file_name(i_spin)='SC_E_History'//'_spin_ch_UP'
     if (n_sp_pol==2.and.i_spin==2) E_file_name(i_spin)='SC_E_History'//'_spin_ch_DN'
     call of_open_close(E_file_name(i_spin),'ot')
     !
     call DESC_write("o "//trim(E_file_name(i_spin)),"#",SC_desc,skip_patt="Iteration")
     !
     call msg('o '//trim(E_file_name(i_spin)),'#',headers(1:n_headers),INDENT=0,USE_TABS=.TRUE.)
     call msg('o '//trim(E_file_name(i_spin)),'#')
     !
   endif
   !
   if ( it >  0 ) then
     call msg("o "//trim(E_file_name(i_spin)),'',(/real(it_now,SP),&
&                             ((E%E(ib,ik,i_spin)*HA2EV,ib=b_range(1),b_range(2)),ik=1,max_kpt) &
&                             /),INDENT=-2,USE_TABS=.true.)
     call of_open_close(E_file_name(i_spin),'f')
     !
   endif
   !
   if ( it <  0 ) call of_open_close(E_file_name(i_spin))
   !
 enddo
 !
end subroutine SC_History
