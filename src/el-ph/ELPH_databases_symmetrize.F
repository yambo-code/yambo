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
subroutine ELPH_databases_symmetrize(mode,E,k,q,ik_bz,iq_bz,n_bands,GKKP)
 ! 
 use pars,           ONLY:SP
 use units,          ONLY:HA2EV
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:qindx_S,bz_samp
 use ELPH,           ONLY:ph_modes,PH_freqs_sq
 use vec_operate,    ONLY:degeneration_finder
 !
 implicit none
 ! 
 character(1), intent(in)    :: mode
 integer,      intent(in)    :: ik_bz,iq_bz,n_bands(2)
 real(SP),     intent(inout) :: GKKP(ph_modes,n_bands(2),n_bands(2))
 type(levels), intent(in)    :: E
 type(bz_samp),intent(in)    :: k,q
 !
 ! Work Space
 integer             ::ik,i_n,i_np,ik_m_q,first_el(2,n_bands(2)),n_of_el(2,n_bands(2)),n_deg_grp(2),i_g,i_c,iq
 real(SP)            ::K_average(ph_modes)
 !
 if (mode=="K") then
   !
   ! Degeneration Finder
   !=====================
   ! Feb 2014. Here I symmetrize the gkkp m.e. with respect to denerated bands. This procedure is not
   ! needed in the case the Double Grid is not used. But when the Double Grid is used gkkp m.e. corresponding to transitions between degenerate
   ! states can be exported to non-degenerate bands belonging to a k of the small grid. In this way
   ! the detailed balance condition is not satisfied anymore.
   !
   ik       =k%sstar(ik_bz,1)
   ik_m_q   =k%sstar(qindx_S(ik,iq_bz,1),1)
   !
   call degeneration_finder(E%E(:n_bands(2),ik,1),    n_bands(2),first_el(1,:),n_of_el(1,:),n_deg_grp(1),0.0001/HA2EV)
   call degeneration_finder(E%E(:n_bands(2),ik_m_q,1),n_bands(2),first_el(2,:),n_of_el(2,:),n_deg_grp(2),0.0001/HA2EV)
   !
   do i_g=1,n_deg_grp(1)
     do i_np=n_bands(1),n_bands(2)
       do i_c=1,2
         if (i_c==1) K_average=0._SP
         do i_n=first_el(1,i_g),first_el(1,i_g)+n_of_el(1,i_g)-1
           if (i_c==1) K_average(:)=K_average(:)+GKKP(:,i_n,i_np)
           if (i_c==2) GKKP(:,i_n,i_np)=K_average(:)/real(n_of_el(1,i_g))
         enddo
       enddo
     enddo
   enddo
   do i_g=1,n_deg_grp(2)
     do i_n=n_bands(1),n_bands(2)
       do i_c=1,2
         if (i_c==1) K_average=0._SP
         do i_np=first_el(2,i_g),first_el(2,i_g)+n_of_el(2,i_g)-1
           if (i_c==1) K_average(:)=K_average(:)+GKKP(:,i_n,i_np)
           if (i_c==2) GKKP(:,i_n,i_np)=K_average(:)/real(n_of_el(2,i_g))
         enddo
       enddo
     enddo
   enddo
   !
 endif
 !
 if (mode=="Q") then
   !
   iq=q%sstar(iq_bz,1)
   !
   call degeneration_finder(PH_freqs_sq(iq,:),ph_modes,first_el(1,:ph_modes),n_of_el(1,:ph_modes),n_deg_grp(1),0.0001/HA2EV)
   !
 endif
 !
end subroutine ELPH_databases_symmetrize
