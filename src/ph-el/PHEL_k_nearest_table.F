!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): FP
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
subroutine PHEL_k_nearest_table(k)
 !
 ! In the case of custom/random qpoints, prepare a table of 
 ! the k-q indices as ik_m_q ~ ikp where ikp is the closest point
 ! in the k grid.
 !
 use R_lattice,      ONLY:bz_samp,k_the_nearest
 use ELPH,           ONLY:PH_qpt
 use PHEL,           ONLY:KpQ_IO_map,PH_Q
 use parallel_m,     ONLY:PARs_PH_K_bz,PARs_PH_Q_ibz
 use parallel_int,   ONLY:PP_redux_wait
 use LIVE_t,         ONLY:live_timing
 use timing_m,       ONLY:timing
 !
 implicit none
 !
 type(bz_samp), intent(in)    :: k
 !
 ! Work Space ...
 !
 integer :: iq,ikbz,i_err
 !
 call live_timing('Finding K-Q_custom indices',PARs_PH_Q_ibz%N_par*PARs_PH_K_bz%N_par)
 !
 call timing("K-Q indexing",OPR='start')
 !
 KpQ_IO_map=0
 !
 do iq=PH_Q(1),PH_Q(2)
   !
   if (.not.PARs_PH_Q_ibz%IND%element_1D(iq)) cycle
   !
   do ikbz=1,k%nbz
     !
     if (.not.PARs_PH_K_bz%IND%element_1D(ikbz)) cycle
     !
     ! Notice that k-q_yambo ==> k+q_PH because of the MinusQ convention
     KpQ_IO_map(iq,ikbz) = k_the_nearest(k%ptbz(ikbz,:)+PH_qpt(iq,:),k%ptbz,k%nbz,i_err=i_err)
     !
     call live_timing( steps=1 )
     !
   enddo
   !
 enddo
 !
 ! Only PARALLEL_wait in k and q, NOT in bands
 call PP_redux_wait(KpQ_IO_map,COMM=PARs_PH_K_bz%COMM_i%COMM)
 call PP_redux_wait(KpQ_IO_map,COMM=PARs_PH_Q_ibz%COMM_i%COMM)
 !
 call live_timing( )
 !
 call timing("K-Q indexing",OPR='stop')
 !
end subroutine PHEL_k_nearest_table
