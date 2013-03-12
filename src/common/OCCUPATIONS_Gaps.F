!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
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
subroutine OCCUPATIONS_gaps(E,E_g_dir,E_g_ind,N_f,N_m)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_sp_pol,n_spin,filled_tresh
 !
 implicit none
 type(levels)       ::E
 real(SP), optional ::E_g_dir(n_sp_pol,2) ! min - max
 real(SP), optional ::E_g_ind(n_sp_pol,2) ! min - max
 integer , optional ::N_f(n_sp_pol)
 integer , optional ::N_m(n_sp_pol)
 ! 
 ! Work Space
 ! 
 integer  ::i1,i_s
 real(SP) ::r_nk
 real(SP), parameter ::tresh=epsilon(1._SP)
 ! 
 ! Shadow variables 
 ! 
 real(SP) ::E_g_dir_(n_sp_pol,2)
 real(SP) ::E_g_ind_(n_sp_pol,2) 
 integer  ::N_f_(n_sp_pol)
 integer  ::N_m_(n_sp_pol)
 !
 ! Evaluate Metallic/Filled bands
 !
 N_m_=0
 N_f_=0
 do i_s=1,n_sp_pol
   do i1=1,E%nb
     !
     ! A single state (n k) weights 2 only when there is no spin
     ! components 
     !
     r_nk=sum(E%f(i1,:,i_s))
     !
     if (n_spin==1) r_nk=r_nk/2.
     !
     if (r_nk<=tresh) cycle
     if (abs(r_nk-real(E%nk,SP))<=filled_tresh) then
       N_f_(i_s)=i1
       cycle
     endif
     N_m_(i_s)=i1
   enddo
   if (N_m_(i_s)==0) N_m_(i_s)=N_f_(i_s)
 enddo
 !
 E%nbf=minval(N_f_)
 E%Nbm=maxval(N_m_)
 !
 if (E%Nbm+1 > E%nb) call error(' Too few states. Include more states in the DFT run.')
 !
 ! E%Efermi(1) = Fermi Level
 ! E%Efermi(2) = VB max (with resepect to Efermi(1) )
 ! E%Efermi(3) = CB min (with resepect to Efermi(1) )
 !
 E%Efermi(2:)=E%Efermi(1)+(/maxval(E%E(E%nbf,:,:)),minval(E%E(E%nbf+1,:,:))/)
 if (E%nbf/=E%nbm) E%Efermi(2:)=0.
 !
 ! [1] Indirect Gaps (min/max)
 !
 do i_s=1,n_sp_pol
   !
   if (N_f_(i_s)/=N_m_(i_s)) cycle
   E_g_ind_(i_s,1)=minval(E%E(N_f_(i_s)+1,:,i_s))-maxval(E%E(N_f_(i_s),:,i_s))
   E_g_ind_(i_s,2)=maxval(E%E(N_f_(i_s)+1,:,i_s))-maxval(E%E(N_f_(i_s),:,i_s))
   !
 enddo
 !
 E%E_ind_gap(:n_sp_pol)=E_g_ind_(:n_sp_pol,1)
 !
 ! [2] Direct Gaps (min/max)
 !
 do i_s=1,n_sp_pol
   !
   if (N_f_(i_s)/=N_m_(i_s)) cycle
   E_g_dir_(i_s,:)=(/100.,-100./)
   do i1=1,E%nk
     E_g_dir_(i_s,1)=min(E_g_dir_(i_s,1),E%E(N_f_(i_s)+1,i1,i_s)-E%E(N_f_(i_s),i1,i_s) )
     E_g_dir_(i_s,2)=max(E_g_dir_(i_s,2),E%E(N_f_(i_s)+1,i1,i_s)-E%E(N_f_(i_s),i1,i_s) )
   enddo
   !
 enddo
 !
 E%E_dir_gap(:n_sp_pol)=E_g_dir_(:n_sp_pol,1)
 !
 ! From Shadow variables to optional variables
 !
 if (present(E_g_dir)) E_g_dir=E_g_dir_
 if (present(E_g_ind)) E_g_ind=E_g_ind_
 if (present(N_f)) N_f=N_f_
 if (present(N_m)) N_m=N_m_
 !
end subroutine
