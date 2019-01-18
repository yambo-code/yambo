
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine OCCUPATIONS_Gaps(E,E_g_dir,E_g_ind,Nbf,Nbm,I_dir)
 !
 use pars,          ONLY:SP
 use electrons,     ONLY:levels,n_sp_pol,n_spin,filled_tresh
 use com,           ONLY:error,warning
 !
 implicit none
 !
 type(levels)       ::E
 real(SP), optional ::E_g_dir(1+(n_sp_pol-1)*2,2) ! min - max
 real(SP), optional ::E_g_ind(1+(n_sp_pol-1)*2,2) ! min - max
 integer , optional ::Nbf(n_sp_pol)
 integer , optional ::Nbm(n_sp_pol)
 integer , optional ::I_dir(2)
 ! 
 ! Work Space
 ! 
 integer :: ib,ik,i_sp_pol,j_sp_pol,i_sp_ref,j_sp_ref
 !
 real(SP):: E_g_dir_old
 real(SP):: E_vb_min(n_sp_pol),E_vb_max(n_sp_pol)
 real(SP):: E_cb_min(n_sp_pol),E_cb_max(n_sp_pol)
 real(SP):: r_nk
 real(SP), parameter ::tresh=epsilon(1._SP)
 ! 
 ! Shadow variables 
 ! 
 real(SP) ::E_g_dir_(1+(n_sp_pol-1)*2,2)
 real(SP) ::E_g_ind_(1+(n_sp_pol-1)*2,2) 
 integer  ::Nbf_(n_sp_pol),I_dir_(2),Nbm_(n_sp_pol)
 !
 if(sum(E%f(:,:,:))<1._SP-filled_tresh) call error('The system contains less than 1 electron')
 !
 ! Initialization
 !
 E_g_dir_=0
 E_g_ind_=0
 Nbm_=0
 Nbf_=0
 !
 ! Evaluate Metallic/Filled bands
 !
 do i_sp_pol=1,n_sp_pol
   do ib=1,E%nb
     !
     ! A single state (n k) weights 2 only when there is no spin
     ! components 
     !
     r_nk=sum(E%f(ib,:,i_sp_pol))
     !
     if (n_spin==1) r_nk=r_nk/2._SP
     !
     if (r_nk<=tresh) cycle
     if (abs(r_nk-real(E%nk,SP))<=filled_tresh) then
       Nbf_(i_sp_pol)=ib
       cycle
     endif
     Nbm_(i_sp_pol)=ib
   enddo
   if (Nbm_(i_sp_pol)==0) Nbm_(i_sp_pol)=Nbf_(i_sp_pol)
 enddo
 !
 E%nbf=minval(Nbf_)
 E%nbm=maxval(Nbm_)
 !
 if (E%Nbm+1 > E%nb) call error(' Too few states. Include more states in the DFT run.')
 !
 ! VBM/CBM
 !
 E_vb_min=-100._SP
 E_vb_max=-100._SP
 E_cb_min=0._SP
 E_cb_max=0._SP
 do i_sp_pol=1,n_sp_pol
   if(Nbf_(i_sp_pol)>0) then
     E_vb_min(i_sp_pol)=minval(E%E(Nbf_(i_sp_pol),:,i_sp_pol))
     E_vb_max(i_sp_pol)=maxval(E%E(Nbf_(i_sp_pol),:,i_sp_pol))
   endif
   E_cb_min(i_sp_pol)=minval(E%E(Nbf_(i_sp_pol)+1,:,i_sp_pol))
   E_cb_max(i_sp_pol)=minval(E%E(Nbf_(i_sp_pol)+1,:,i_sp_pol))
 enddo
 !
 E%E_VBM(:n_sp_pol)=E_vb_max
 E%E_CBm(:n_sp_pol)=E_cb_min
 !
 ! [1] Indirect Gaps (min/max)
 !
 if(all(Nbf_==Nbm_)) then
   E_g_ind_(1,1)=minval(E_cb_min)-maxval(E_vb_max)
   E_g_ind_(1,2)=minval(E_cb_max)-maxval(E_vb_min)
   ! This should never happen
   if(E_g_ind_(1,1)<0.and.n_sp_pol==2) call warning('Merged spin levels are metallic')
   !
 endif
 !
 if(n_sp_pol==2.and.any(Nbf_==Nbm_)) then
   do i_sp_pol=1,n_sp_pol
     !
     if (Nbf_(i_sp_pol)/=Nbm_(i_sp_pol)) cycle
     E_g_ind_(i_sp_pol+1,1)=E_cb_min(i_sp_pol)-E_vb_max(i_sp_pol) 
     E_g_ind_(i_sp_pol+1,2)=E_cb_max(i_sp_pol)-E_vb_min(i_sp_pol)
     !
   enddo
 endif
 !
 if(n_sp_pol==1) E%E_ind_gap(1)  =E_g_ind_(1,1)
 if(n_sp_pol==2) E%E_ind_gap(1:2)=E_g_ind_(2:3,1)
 ! 
 ! [2] Direct Gaps (min/max)
 !
 if (all(Nbf_==Nbm_)) then
   E_g_dir_(1,:)=(/100._SP,-100._SP/)
   i_sp_ref=1
   j_sp_ref=1
   do ik=1,E%nk
     do i_sp_pol=1,n_sp_pol
       do j_sp_pol=1,n_sp_pol
         if(Nbf_(j_sp_pol)==0) cycle
         E_g_dir_old=E_g_dir_(1,1)
         E_g_dir_(1,1)=min(E_g_dir_(1,1),&
&                      E%E(Nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(Nbf_(j_sp_pol),ik,j_sp_pol) )
         if(E_g_dir_(1,1)<E_g_dir_old) then
           i_sp_ref=i_sp_pol
           j_sp_ref=j_sp_pol
           I_dir_=(/Nbf_(i_sp_pol),ik/)
         endif
       enddo
     enddo
     E_g_dir_(1,2)=max(E_g_dir_(1,2),E%E(Nbf_(i_sp_ref)+1,ik,i_sp_ref)-E%E(Nbf_(j_sp_ref),ik,j_sp_ref) )
   enddo
 endif
 !
 if (n_sp_pol==2.and.any(Nbf_==Nbm_)) then
   do i_sp_pol=1,n_sp_pol
     !
     if (Nbf_(i_sp_pol)/=Nbm_(i_sp_pol)) cycle
     if (Nbf_(i_sp_pol)==0) cycle
     !
     E_g_dir_(i_sp_pol+1,:)=(/100._SP,-100._SP/)
     do ik=1,E%nk
       E_g_dir_(i_sp_pol+1,1)=min(E_g_dir_(i_sp_pol+1,1),&
&                      E%E(Nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(Nbf_(i_sp_pol),ik,i_sp_pol) )
       E_g_dir_(i_sp_pol+1,2)=max(E_g_dir_(i_sp_pol+1,2),&
&                      E%E(Nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(Nbf_(i_sp_pol),ik,i_sp_pol) )
     enddo
     !
   enddo
 endif
 !
 ! If the system is metallic VBM and CBm are set to zero, i.e. equal to the Fermi level
 ! so that stretching corrections are applied properly
 !
 do i_sp_pol=1,n_sp_pol
   if(Nbf_(i_sp_pol)/=Nbm_(i_sp_pol)) then
     E%E_VBM(i_sp_pol)=E%E_Fermi
     E%E_CBm(i_sp_pol)=E%E_Fermi
   endif
 enddo
 !
 if (n_sp_pol==1) E%E_dir_gap(1)=E_g_dir_(1,1)
 if (n_sp_pol==2) E%E_dir_gap(1:2)=E_g_dir_(2:3,1)
 !
 ! From Shadow variables to optional variables
 !
 if (present(I_dir))   I_dir=I_dir_
 if (present(E_g_dir)) E_g_dir=E_g_dir_
 if (present(E_g_ind)) E_g_ind=E_g_ind_
 if (present(Nbf))     Nbf=Nbf_
 if (present(Nbm))     Nbm=Nbm_
 !
end subroutine
