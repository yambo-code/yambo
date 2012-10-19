!
! Copyright (C) 2000-2012 A. Marini and the YAMBO team 
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
subroutine REPORT_Occupations(E)
 !
 use pars,          ONLY:SP,lchlen
 use units,         ONLY:HA2EV
 use com,           ONLY:msg,warning
 use electrons,     ONLY:levels,n_sp_pol
 use interfaces,    ONLY:OCCUPATIONS_gaps
 !
 implicit none
 type(levels)::E
 ! 
 ! Work Space
 ! 
 integer           ::i1,i_s
 character(lchlen) ::ch_
 character(23)     ::prefix_
 real(SP)          ::E_g_dir(n_sp_pol,2) ! min - max
 real(SP)          ::E_g_ind(n_sp_pol,2) ! min - max
 integer           ::N_f(n_sp_pol)
 integer           ::N_m(n_sp_pol)
 !
 call OCCUPATIONS_gaps(E,E_g_dir,E_g_ind,N_f,N_m)
 !
 call msg('r','States summary         : Full        Metallic    Empty')
 !
 ! [1] States occupations
 !
 do i_s=1,n_sp_pol
   i1=1
   !
   prefix_=''
   if (n_sp_pol>1.and.i_s==1) prefix_='             [spin up]'
   if (n_sp_pol>1.and.i_s==2) prefix_='                  [dn]'
   !
   if (N_f(i_s)==0) i1=0
   if (N_f(i_s)/=N_m(i_s)) then
     write (ch_,'(3(i4.4,a,i4.4,3x))') i1,'-',N_f(i_s),&
&          N_f(i_s)+1,'-',N_m(i_s),N_m(i_s)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   else
     write (ch_,'(2(i4.4,a,i4.4,15x))') i1,'-',N_f(i_s),N_f(i_s)+1,'-',E%nb
     call msg('r',prefix_//'  '//trim(ch_))
   endif
   !
 enddo
 !
 ! [2] Indirect Gaps (min/max)
 !
 do i_s=1,n_sp_pol
   !
   if (N_f(i_s)/=N_m(i_s)) cycle
   !
   prefix_=                               'Indirect Gaps      [ev]:'
   if (n_sp_pol>1.and.i_s==1) prefix_='Indirect Gaps [ev] [up]'
   if (n_sp_pol>1.and.i_s==2) prefix_='                   [dn]'
   call msg('r',prefix_//':',E_g_ind(i_s,:)*HA2EV)
   !
 enddo
 !
 ! [3] Direct Gaps (min/max)
 !
 do i_s=1,n_sp_pol
   !
   if (N_f(i_s)/=N_m(i_s)) cycle
   !
   prefix_=                               'Direct Gaps        [ev]:'
   if (n_sp_pol>1.and.i_s==1) prefix_='Direct Gaps   [ev] [up]:'
   if (n_sp_pol>1.and.i_s==2) prefix_='                   [dn]:'
   call msg('r',prefix_//':',E_g_dir(i_s,:)*HA2EV)
   !
 enddo
 !
 ! [3] Report if the system is treated as a metal
 !
 if (E%nbf/=E%nbm.and..not.any(N_f/=N_m)) call warning('Merged spin levels are metallic')
 !
end subroutine
