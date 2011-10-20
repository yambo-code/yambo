!
! Copyright (C) 2000-2011 A. Marini and the YAMBO team
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
logical function stop_now(DO_IT_NOW)
 !
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,E_reset
 use com,            ONLY:repfile,file_exists,report_file_fix
 use frequency,      ONLY:w_samp,W_reset
 use X_m,            ONLY:X_t,X_reset
 use it_m,           ONLY:it_reset
 use par_proc_m,     ONLY:p_finalize
 use parser_m,       ONLY:parser
 implicit none
 !
 logical :: DO_IT_NOW
 ! 
 ! Work Space
 !
 type(levels)      ::en
 type(bz_samp)     ::q,k
 type(X_t)         ::X
 type(w_samp)      ::Xw
 integer           ::ierr
 integer, external ::init
 !
 call it_reset(0)
 call parser('STOP_NOW',stop_now)
 !
 if (.not.stop_now) return
 if (.not.DO_IT_NOW) return
 !
 call E_reset(en)
 call bz_samp_reset(q)
 call bz_samp_reset(k)
 call W_reset(Xw)
 call X_reset(X,type=1)
 !
 if (file_exists(repfile) ) call report_file_fix
 !
 call section('X','USER STOP signal')
 !
 ! Update init defs
 !
 ierr=init(en,q,k,X,Xw," "," ",.false.,.true.)
 !
 ! Finalize
 !
 call it_reset(-1)
 !
 call p_finalize
 !
 stop
 !
end function
