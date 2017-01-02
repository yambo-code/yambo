!
!        Copyright (C) 2000-2017 the YAMBO team
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
subroutine REPORT_Occupations(E)
 !
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:HA2EV
 use com,           ONLY:msg,warning
 use electrons,     ONLY:levels,n_sp_pol,nel,spin_occ
 use interfaces,    ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 type(levels)::E
 ! 
 ! Work Space
 ! 
 integer           ::i_sp_pol
 integer           ::first_occ_state
 integer           ::Nbf(n_sp_pol)
 integer           ::Nbm(n_sp_pol)
 character(lchlen) ::ch_
 character(23)     ::prefix_
 real(SP)          ::N_met
 real(SP)          ::f_met
 real(SP)          ::E_g_dir(1+(n_sp_pol-1)*2,2) ! min - max
 real(SP)          ::E_g_ind(1+(n_sp_pol-1)*2,2) ! min - max
 !
 call OCCUPATIONS_Gaps(E,E_g_dir,E_g_ind,Nbf,Nbm)
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
   if (Nbf(i_sp_pol)==0) first_occ_state=0
   if (Nbf(i_sp_pol)/=Nbm(i_sp_pol)) then
     write (ch_,'(3(i4.4,a,i4.4,3x))') first_occ_state,'-',Nbf(i_sp_pol),&
&          Nbf(i_sp_pol)+1,'-',Nbm(i_sp_pol),Nbm(i_sp_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   else
     write (ch_,'(2(i4.4,a,i4.4,15x))') first_occ_state,'-',Nbf(i_sp_pol),Nbf(i_sp_pol)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   endif
   !
 enddo
 !
 ! [2] Indirect Gaps (min/max)
 !
 if(all(Nbf==Nbm)) then
   prefix_=                               'Indirect Gaps      [ev]'
   call msg('r',prefix_//':',E_g_ind(1,:)*HA2EV)
 endif
 !
 if(n_sp_pol==2.and.any(Nbf==Nbm)) then 
   do i_sp_pol=1,n_sp_pol
     if (Nbf(i_sp_pol)/=Nbm(i_sp_pol)) cycle
     if (Nbf(i_sp_pol)==0) cycle
     if (i_sp_pol==1) prefix_='              [spin up]'
     if (i_sp_pol==2) prefix_='              [spin dn]'
     call msg('r',prefix_//':',E_g_ind(i_sp_pol+1,:)*HA2EV)
   enddo
 endif
 !
 ! [3] Direct Gaps (min/max)
 !
 if(all(Nbf==Nbm)) then
   prefix_='Direct Gaps        [ev]'
   call msg('r',prefix_//':',E_g_dir(1,:)*HA2EV)
 endif
 !
 if(n_sp_pol==2.and.any(Nbf==Nbm)) then 
   do i_sp_pol=1,n_sp_pol
     if (Nbf(i_sp_pol)/=Nbm(i_sp_pol)) cycle
     if (Nbf(i_sp_pol)==0) cycle
     if (i_sp_pol==1) prefix_='              [spin up]'
     if (i_sp_pol==2) prefix_='              [spin dn]'
     call msg('r',prefix_//':',E_g_dir(i_sp_pol+1,:)*HA2EV)
   enddo
 endif
 !
 !
 ! [3] Report if the system is treated as a metal
 !
 if (.not.all(Nbf==Nbm)) then
   !
   if(n_sp_pol==1) call warning('Metallic system')
   !
   if(n_sp_pol==2) then
     if(all(Nbf/=Nbm)) call warning('Metallic system')
     if(any(Nbf==Nbm)) call warning('System is an half-metal')
   endif
   !
   call msg('r',' ')
   !
   N_met=0
   f_met=0
   do i_sp_pol=1,n_sp_pol
     f_met=f_met+sum(E%f(Nbf(i_sp_pol)+1:Nbm(i_sp_pol),:,i_sp_pol))/sum(spin_occ*real(E%nk*(Nbm(:)-Nbf(:))))
     N_met=N_met+sum(E%f(Nbf(i_sp_pol)+1:Nbm(i_sp_pol),:,i_sp_pol))/real(E%nk)
   enddo
   !
   call msg('r','N of el / N of met el: ',       (/nel,N_met/))
   call msg('r','Average metallic occ.:            ',(/f_met/))
   !
 endif
 !
end subroutine
