!
! Copyright (C) 2000-2010 A. Marini and the YAMBO team 
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
 use units,         ONLY:HARTREE,BOHR,Bohr_magneton
 use com,           ONLY:msg,warning
#if defined _BOLTZMANN
 use com,           ONLY:write_to_report
#endif
 use electrons,     ONLY:levels,n_sp_pol,n_spin,filled_tresh
#if defined _SC
 use magnetic,      ONLY:average_el_mag,average_el_mag_dim
 use drivers,       ONLY:l_sc_magnetic
#endif
 implicit none
 type(levels)::E
 integer     ::mode
 ! 
 ! Work Space
 ! 
 integer :: i1,nbm_(n_sp_pol),nbf_(n_sp_pol),i_s_pol
 real(SP):: r_nk,ind_gaps(2),dir_gaps(2)
 real(SP),parameter:: tresh=epsilon(1._SP)
 character(lchlen) :: ch_
 character(23)     :: prefix_
 !
 ! Evaluate Metallic/Filled bands
 !
 nbm_=0
 nbf_=0
 do i_s_pol=1,n_sp_pol
   do i1=1,E%nb
     !
     ! A single state (n k) weights 2 only when there is no spin
     ! components 
     !
     r_nk=sum(E%f(i1,:,i_s_pol))
     !
     if (n_spin==1) r_nk=r_nk/2.
     !
     if (r_nk<=tresh) cycle
     if (abs(r_nk-real(E%nk,SP))<=filled_tresh) then
       nbf_(i_s_pol)=i1
       cycle
     endif
     nbm_(i_s_pol)=i1
   enddo
   if (nbm_(i_s_pol)==0) nbm_(i_s_pol)=nbf_(i_s_pol)
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
 E%Efermi(2:)=E%Efermi(1)+(/maxval(E%E(E%nbf,:,:)),minval(E%E(E%nbf+1,:,:))/)
 if (E%nbf/=E%nbm) E%Efermi(2:)=0.
 !
#if defined _BOLTZMANN
 if (.not.write_to_report) then
   call msg('s',':: Full/Metallic bands :',(/E%nbf,E%nbm-E%nbf/))
 endif
#endif
 !
 call msg('r','States summary         : Full        Metallic    Empty')
 !
 ! [1] States occupations
 !
 do i_s_pol=1,n_sp_pol
   i1=1
   !
   prefix_=''
   if (n_sp_pol>1.and.i_s_pol==1) prefix_='             [spin up]'
   if (n_sp_pol>1.and.i_s_pol==2) prefix_='                  [dn]'
   !
   if (nbf_(i_s_pol)==0) i1=0
   if (nbf_(i_s_pol)/=nbm_(i_s_pol)) then
     write (ch_,'(3(i4.4,a,i4.4,3x))') &
&          i1,'-',nbf_(i_s_pol),&
&          nbf_(i_s_pol)+1,'-',nbm_(i_s_pol),nbm_(i_s_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   else
     write (ch_,'(2(i4.4,a,i4.4,15x))') i1,'-',nbf_(i_s_pol),nbf_(i_s_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   endif
   !
 enddo
 !
 ! [2] Indirect Gaps (min/max)
 !
 do i_s_pol=1,n_sp_pol
   !
   if (nbf_(i_s_pol)/=nbm_(i_s_pol)) cycle
   !
   prefix_=                               'Indirect Gaps      [ev]:'
   if (n_sp_pol>1.and.i_s_pol==1) prefix_='Indirect Gaps [ev] [up]'
   if (n_sp_pol>1.and.i_s_pol==2) prefix_='                   [dn]'
   ind_gaps(1)=minval(E%E(nbf_(i_s_pol)+1,:,i_s_pol))-maxval(E%E(nbf_(i_s_pol),:,i_s_pol))
   ind_gaps(2)=maxval(E%E(nbf_(i_s_pol)+1,:,i_s_pol))-maxval(E%E(nbf_(i_s_pol),:,i_s_pol))
   call msg('r',prefix_//':',ind_gaps*HARTREE)
   !
 enddo
 !
 ! [3] Direct Gaps (min/max)
 !
 do i_s_pol=1,n_sp_pol
   !
   if (nbf_(i_s_pol)/=nbm_(i_s_pol)) cycle
   !
   prefix_=                               'Direct Gaps        [ev]:'
   if (n_sp_pol>1.and.i_s_pol==1) prefix_='Direct Gaps   [ev] [up]:'
   if (n_sp_pol>1.and.i_s_pol==2) prefix_='                   [dn]:'
   dir_gaps=(/100.,-100./)
   do i1=1,E%nk
     dir_gaps(1)=min(dir_gaps(1),&
&                    E%E(nbf_(i_s_pol)+1,i1,i_s_pol)-E%E(nbf_(i_s_pol),i1,i_s_pol) )
     dir_gaps(2)=max(dir_gaps(2),&
&                    E%E(nbf_(i_s_pol)+1,i1,i_s_pol)-E%E(nbf_(i_s_pol),i1,i_s_pol) )
   enddo
   call msg('r',prefix_//':',dir_gaps*HARTREE)
   !
 enddo
 !
 ! [3] Report if the system is treated as a metal
 !
 if (E%nbf/=E%nbm.and..not.any(nbf_/=nbm_)) call warning('Merged spin levels are metallic')
 !
#if defined _SC
 ! [4] Report the value of the mean magnetization
 !
 if(n_sp_pol>1 .and. l_sc_magnetic) then
   call msg('r','Mean magnetization (x,y,z) ['//trim(average_el_mag_dim)//'] :',real(average_el_mag(:),SP))
 endif
#endif
 !
end subroutine
