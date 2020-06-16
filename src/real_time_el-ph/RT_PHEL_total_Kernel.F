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
subroutine RT_PHEL_total_Kernel(iq,E,k,q)
 !
 ! EULER:
 !
 !  In the EULER scheme everything is extremely easy. G_lesser is G(TIME) and
 !  I use the saved THETA_matrix (that in EULER is saved at TIME-RT_step) and the newly calculated
 !  DELTA matrix to get the new THETA_matrix.
 !
 ! RK2:
 !
 !  In RK2 everything is much more complicated because RT_elph_kernel is called at NE_time and
 !  at NE_time+RT_step/2 and in the two cases a different strategy ***should*** be applied.
 !
 ! Iteration: in this case I ***think*** (even if I am not completely sure) that everything is ok.
 !
 use pars,           ONLY:SP,rZERO,cZERO
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,qindx_X
 use real_time,      ONLY:RT_bands
 use PH_real_time,   ONLY:phel_gkkp_expanded_sq
 use ELPH,           ONLY:ph_freqs_sq,elph_branches,MAX_gkkp
 use functions,      ONLY:bose_f
 use parallel_m,     ONLY:PAR_IND_Q,PAR_Q_index,PAR_Xk_ibz_index
 use parser_m,       ONLY:parser
 !
 implicit none
 !
 type(levels),  intent(in) :: E
 type(bz_samp), intent(in) :: k,q
 integer,       intent(in) :: iq
 !
 ! Work Space ...
 !
 integer               :: il
 real(SP)              :: ph_freq
 real(SP), parameter   :: cutoff= 5.E-3
 !
 real(SP)              :: Gkkp_sq(RT_bands(1):RT_bands(2),RT_bands(1):RT_bands(2))
 !
 ! ... band index ...
 integer               :: i_n,i_np
 ! ... BZ index ...
 integer               :: iqbz,ikbz,ik,ik_m_q,ik_mem,iq_mem
 !
 iq_mem=PAR_Q_index(iq)
 !
 do ikbz=1,k%nbz
   !
   ! Obtain k-q in the ibz from k in the bz and q in the ibz
   ik_m_q =k%sstar(qindx_X(iq_mem,ikbz,1),1)
   ! Obtain k in the ibz from k in the bz
   ik = k%sstar(ikbz,1)
   !
   ik_mem=PAR_Xk_ibz_index(ik)
   !
   do il=elph_branches(1),elph_branches(2)
     !
     ! Given iq in the ibz, get corresponding index in the bz [NOTE] NOT USED
     iqbz = q%k_table(iq_mem,1)
     !
     ph_freq=abs(sqrt(ph_freqs_sq(iq_mem,il)))
     !
     if ( (iq_mem==1 .and. il<=3) ) cycle
     if ( abs(ph_freq)<3.E-5)     cycle
     !
     ! [NOTE] the m.e. in elph_gkkp DO NOT have indices order exchanged (unlike RT_ELPH case)
     !
     Gkkp_sq=rZERO
     do i_n=RT_bands(1),RT_bands(2)
       Gkkp_sq(i_n,i_n)=phel_gkkp_expanded_sq(il,i_n,i_n,iq_mem,ik_mem)
       if(sqrt(Gkkp_sq(i_n,i_n))<MAX_gkkp*cutoff) Gkkp_sq(i_n,i_n)=rZERO
       !
       do i_np=RT_bands(1),RT_bands(2)
         !
         if (i_n==i_np) cycle
         Gkkp_sq(i_n,i_np)=phel_gkkp_expanded_sq(il,i_n,i_np,iq_mem,ik_mem)
         if (sqrt(Gkkp_sq(i_n,i_np))<MAX_gkkp*cutoff) Gkkp_sq(i_n,i_np)=rZERO
         !
       enddo
       !
     enddo
     !
     ! Time Scales separation (TS) and quasiparticle approximation (QP)
     !=======================================
     !
     call RT_PHEL_TSQP_Kernel(ik,ik_m_q,iq_mem,il,E,ph_freq,Gkkp_sq)
     !
   enddo ! ph branch
   !
 enddo ! ikbz
 !
end subroutine RT_PHEL_total_kernel
