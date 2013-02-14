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
subroutine REPORT_Occupations(E,mode)
 !
 use pars,          ONLY:SP,lchlen
 use R_lattice,     ONLY:nkibz
 use units,         ONLY:HA2EV,BO2ANG,Bohr_magneton
 use com,           ONLY:msg,warning,error
 use electrons,     ONLY:levels,n_sp_pol,n_spin,filled_tresh,nel
 !
 implicit none
 type(levels)::E
 integer     ::mode
 ! 
 ! Work Space
 ! 
 logical :: merged_metallic
 integer :: ib,ik,i_sp_pol,j_sp_pol,i_sp_ref,j_sp_ref
 integer :: nbm_(n_sp_pol),nbf_(n_sp_pol),first_occ_state
 real(SP):: r_nk,ind_gaps(2),dir_gaps(2),N_met,dir_gap_old
 real(SP):: E_vb_min(n_sp_pol),E_vb_max(n_sp_pol)
 real(SP):: E_cb_min(n_sp_pol),E_cb_max(n_sp_pol)
 real(SP),parameter:: tresh=epsilon(1._SP)
 character(lchlen) :: ch_
 character(23)     :: prefix_
 !
 ! Evaluate Metallic/Filled bands
 !
 if(sum(E%f(:,:,:))<1._SP-filled_tresh) call error('The system contains less than 1 electron')
 !
 nbm_=0
 nbf_=0
 do i_sp_pol=1,n_sp_pol
   do ib=1,E%nb
     !
     ! A single state (n k) weights 2 only when there is no spin
     ! components 
     !
     r_nk=sum(E%f(ib,:,i_sp_pol))
     !
     if (n_spin==1) r_nk=r_nk/2.
     !
     if (r_nk<=tresh) cycle
     if (abs(r_nk-real(E%nk,SP))<=filled_tresh) then
       nbf_(i_sp_pol)=ib
       cycle
     endif
     nbm_(i_sp_pol)=ib
   enddo
   if (nbm_(i_sp_pol)==0) nbm_(i_sp_pol)=nbf_(i_sp_pol)
 enddo
 !
 E%nbf=minval(nbf_)
 E%nbm=maxval(nbm_)
 !
 ! No Report
 !
 if (mode<=1) return
 !
 ! Report ...
 !
 ! E%Efermi(1) = Fermi Level
 ! E%Efermi(2) = VB max (with resepect to Efermi(1) )
 ! E%Efermi(3) = CB min (with resepect to Efermi(1) )
 !
 E_vb_min=-100._SP
 E_vb_max=-100._SP
 E_cb_min=0._SP
 E_cb_max=0._SP
 do i_sp_pol=1,n_sp_pol
   if(nbf_(i_sp_pol)>0) then
     E_vb_min(i_sp_pol)=minval(E%E(nbf_(i_sp_pol),:,i_sp_pol))
     E_vb_max(i_sp_pol)=maxval(E%E(nbf_(i_sp_pol),:,i_sp_pol))
   endif
   E_cb_min(i_sp_pol)=minval(E%E(nbf_(i_sp_pol)+1,:,i_sp_pol))
   E_cb_max(i_sp_pol)=minval(E%E(nbf_(i_sp_pol)+1,:,i_sp_pol))
 enddo
 E%Efermi(2:)=E%Efermi(1)+(/maxval(E_vb_max),minval(E_cb_min)/)
 if (E%nbf/=E%nbm) E%Efermi(2:)=0.
 !
 call msg('r','States summary         : Full        Metallic    Empty')
 !
 ! [1] States occupations
 !
 do i_sp_pol=1,n_sp_pol
   first_occ_state=1
   !
   prefix_=''
   if (n_sp_pol>1.and.i_sp_pol==1) prefix_='             [spin up]'
   if (n_sp_pol>1.and.i_sp_pol==2) prefix_='                  [dn]'
   !
   if (nbf_(i_sp_pol)==0) first_occ_state=0
   if (nbf_(i_sp_pol)/=nbm_(i_sp_pol)) then
     write (ch_,'(3(i4.4,a,i4.4,3x))') &
&          first_occ_state,'-',nbf_(i_sp_pol),&
&          nbf_(i_sp_pol)+1,'-',nbm_(i_sp_pol),nbm_(i_sp_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   else
     write (ch_,'(2(i4.4,a,i4.4,15x))') &
&          first_occ_state,'-',nbf_(i_sp_pol),nbf_(i_sp_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   endif
   !
 enddo
 !
 ! [2] Indirect Gaps (min/max)
 !
 merged_metallic=.false.
 if(all(nbf_==nbm_)) then
   prefix_='Indirect Gaps      [ev]'
   ind_gaps(1)=minval(E_cb_min)-maxval(E_vb_max)
   ind_gaps(2)=minval(E_cb_max)-maxval(E_vb_min)
   if(ind_gaps(1)>0) call msg('r',prefix_//':',ind_gaps*HA2EV)
   if(ind_gaps(1)<0.and.n_sp_pol==2) merged_metallic=.true.
 endif
 !
 if(n_sp_pol==2.and.any(nbf_==nbm_)) then
   do i_sp_pol=1,n_sp_pol
     if (nbf_(i_sp_pol)/=nbm_(i_sp_pol)) cycle
     if (nbf_(i_sp_pol)==0) cycle
     if (i_sp_pol==1) prefix_='              [spin up]'
     if (i_sp_pol==2) prefix_='              [spin dn]'
     ind_gaps(1)=E_cb_min(i_sp_pol)-E_vb_max(i_sp_pol)
     ind_gaps(2)=E_cb_max(i_sp_pol)-E_vb_min(i_sp_pol)
     call msg('r',prefix_//':',ind_gaps*HA2EV)
   enddo
 endif
 !
 ! [3] Direct Gaps (min/max)
 !
 if(all(nbf_==nbm_)) then
   prefix_='Direct Gaps        [ev]'
   dir_gaps=(/100.,-100./)
   do ik=1,E%nk
     do i_sp_pol=1,n_sp_pol
       do j_sp_pol=1,n_sp_pol
         if(nbf_(j_sp_pol)==0) cycle
         dir_gap_old=dir_gaps(1)
         dir_gaps(1)=min(dir_gaps(1),&
&                        E%E(nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(nbf_(j_sp_pol),ik,j_sp_pol) )
         if(dir_gaps(1)<dir_gap_old) then
           i_sp_ref=i_sp_pol
           j_sp_ref=j_sp_pol
         endif
       enddo
     enddo
     dir_gaps(2)=max(dir_gaps(2),E%E(nbf_(i_sp_ref)+1,ik,i_sp_ref)-E%E(nbf_(j_sp_ref),ik,j_sp_ref) )
   enddo
   call msg('r',prefix_//':',dir_gaps*HA2EV)
 endif
 !
 if(n_sp_pol==2.and.any(nbf_==nbm_)) then
   do i_sp_pol=1,n_sp_pol
     !
     if (nbf_(i_sp_pol)/=nbm_(i_sp_pol)) cycle
     if (nbf_(i_sp_pol)==0) cycle
     !
     if (i_sp_pol==1) prefix_='              [spin up]'
     if (i_sp_pol==2) prefix_='              [spin dn]'
     dir_gaps=(/100.,-100./)
     do ik=1,E%nk
       dir_gaps(1)=min(dir_gaps(1),&
&                      E%E(nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(nbf_(i_sp_pol),ik,i_sp_pol) )
       dir_gaps(2)=max(dir_gaps(2),&
&                      E%E(nbf_(i_sp_pol)+1,ik,i_sp_pol)-E%E(nbf_(i_sp_pol),ik,i_sp_pol) )
     enddo
     call msg('r',prefix_//':',dir_gaps*HA2EV)
     !
   enddo
 endif
 !
 ! [3] Report if the system is treated as a metal
 !
 if (.not.all(nbf_==nbm_)) then
   if(n_sp_pol==1) call warning('Metallic system')
   if(n_sp_pol==2) then
     if(all(nbf_/=nbm_)) call warning('Metallic system')
     if(any(nbf_==nbm_)) call warning('System is an half-metal')
   endif
 endif
 !
 ! This should never happen
 if(merged_metallic) call warning('Merged spin levels are metallic')
 !
 if (.not.all(nbf_==nbm_)) then
   N_met=0
   do i_sp_pol=1,n_sp_pol
     N_met= N_met+sum(E%f(nbf_(i_sp_pol)+1:nbm_(i_sp_pol),:,i_sp_pol))/real(E%nk) !/(sum(nbm_(:)-nbf_(:)))) old f_met
   enddo
   call msg('r','N of e- / Metallic e-:',(/nel,N_met/))
 endif
 !
end subroutine
