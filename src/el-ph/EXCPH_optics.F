!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM FP AC
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
!> @brief Calculate phonon-assisted absorption and emission (see PRL 122, 187401 )
!!
subroutine EXCPH_optics(E,k,q,X,wv)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,cZERO,rZERO,schlen
 use electrons,      ONLY:levels,n_sp_pol
 use R_lattice,      ONLY:bz_samp
 use X_m,            ONLY:X_t
 use X_output,       ONLY:eps_file_name,pl_file_name,X_setup_file_names,X_write_messages_before_headers, &
&                         X_write_q_plus_G         
 use parser_m,       ONLY:parser
 use com,            ONLY:msg,of_open_close
 use EXCPH,          ONLY:EXCPH_sum,EXCPH_states
 use frequency,     ONLY:w_samp
 !
#include<memory.h>
 !
 type(levels)  ::E
 type(bz_samp) ::k,q
 type(X_t)     ::X
 type(w_samp)  :: wv 
 !
 ! Work space
 !
 complex(SP), allocatable :: PL_ii(:,:),Epsilon_ii(:)
 real(SP)         :: output_data(5)
 integer          :: iw
 !
 call section('*','EXCPH-Optics')
 !
 call X_setup_file_names(1,'bse ','phass',' ') 
 !
 call of_open_close(eps_file_name,'ot')
 call of_open_close(pl_file_name,'ot')
 !
 call X_write_q_plus_G(1,(/0.0,0.0,0.0/))
 !
 call msg('o eps_q pl_q','# EXCITON PHONON')    
 call msg('o eps_q pl_q','# Exciton_states    :',EXCPH_states,INDENT=0)    
 call msg('o eps_q pl_q','# EXciton sum       :',EXCPH_sum,INDENT=0)    
 !
 ! Calculate X(w) include phonon assisted transitions
 !
 call EXCPH_X_phass(Epsilon_ii,PL_ii,wv)
 !
 do iw=1,wv%n_freqs
     !
     ! Eps
     !
     output_data(1:3)=(/real(wv%p(iw))*HA2EV,aimag(Epsilon_ii(iw)),real(Epsilon_ii(iw))/)
     call msg('o eps_q','',output_data(:3),INDENT=-2,USE_TABS=.true.)
     !
     ! Luminescence
     !
     output_data=(/real(wv%p(iw))*HA2EV,aimag(PL_ii(iw,2)),real(PL_ii(iw,2)),&
&                  aimag(PL_ii(iw,1)),real(PL_ii(iw,1))/)
     !
     call msg('o pl_q','',output_data(:5),INDENT=-2,USE_TABS=.true.)
     !
 enddo
 !
 ! Clsoe files
 !
 YAMBO_FREE(Epsilon_ii)
 YAMBO_FREE(PL_ii)
 call of_open_close(eps_file_name)
 call of_open_close(pl_file_name)
 call FREQUENCIES_reset(wv,"all")
 !
end subroutine
