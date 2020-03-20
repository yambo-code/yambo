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
subroutine REPORT_Occupations(what,E)
 !
 use pars,          ONLY:SP,lchlen,schlen
 use units,         ONLY:HA2EV,BO2ANG,HA2EV,HA2KEL
 use D_lattice,     ONLY:Tel,T_holes,T_elecs,Bose_Temp,DL_vol
 use drivers,       ONLY:Finite_Tel
 use com,           ONLY:msg
 use electrons,     ONLY:levels,n_sp_pol,nel,spin_occ
 use interfaces,    ONLY:OCCUPATIONS_Gaps
 !
 implicit none
 !
 character(*)::what
 type(levels)::E
 ! 
 ! Work Space
 ! 
 integer           ::i_sp_pol,ival
 integer           ::first_occ_state
 integer           ::Nbf(n_sp_pol)
 integer           ::Nbm(n_sp_pol)
 character(lchlen) ::ch_,prefix_
 character(schlen) ::title
 real(SP)          ::N_met
 real(SP)          ::f_met,val(1+(n_sp_pol-1)*2)
 real(SP)          ::E_g_dir(1+(n_sp_pol-1)*2,2) ! min gap - max gap
 real(SP)          ::E_g_ind(1+(n_sp_pol-1)*2,3) ! min - val. band width - cond. band width
 !
 ! Title
 !
 title=""
 !if (len_trim(what)>0) title="["//trim(what)//"]"
 !
 ! Gaps, VBM/CBM
 !
 call OCCUPATIONS_Gaps(E,E_g_dir,E_g_ind,Nbf,Nbm)
 !
 ! Initial Report
 !
 if(T_elecs==T_holes) call msg('r',trim(title)//'Electronic Temp.          ',(/Tel*HA2EV,Tel*HA2KEL/),'[eV K]')
 if(T_elecs/=T_holes) call msg('r',trim(title)//'Holes / Elecs Temp       ',(/T_holes*HA2KEL,T_elecs*HA2KEL/),'[K K]')
 call msg('r',trim(title)//'Bosonic    Temp.          ',(/Bose_Temp*HA2EV,Bose_Temp*HA2KEL/),'[eV K]')
 call msg('r',trim(title)//'Finite Temperature mode   ',Finite_Tel)
 call msg('r',trim(title)//'El. density               ',(/nel/(DL_vol*BO2ANG**3*1.E-24)/),'[cm-3]')
 !
 call msg('nr',trim(title)//'Fermi Level               ',E%E_Fermi*HA2EV,'[eV]')
 do i_sp_pol=1,n_sp_pol
   call msg('r',trim(title)//'Valence Band Max          ',E%E_VBM(i_sp_pol)*HA2EV,'[eV]')
   call msg('r',trim(title)//'Conduc. Band Min          ',E%E_CBm(i_sp_pol)*HA2EV,'[eV]')
 enddo
 !call msg('r',trim(title)//'States summary')
 !
 ! [1] States occupations
 !
 do i_sp_pol=1,n_sp_pol
   if (n_sp_pol>1)                  call msg('r','spin channel              ',i_sp_pol)
   if (Nbf(i_sp_pol)> 0)            call msg('r','Fully Occ. States         ',(/              1,Nbf(i_sp_pol)/))
   if (Nbm(i_sp_pol)>Nbf(i_sp_pol)) call msg('r','Part. Occ. States         ',(/Nbf(i_sp_pol)+1,Nbm(i_sp_pol)/))
   if (E%nb         >Nbm(i_sp_pol)) call msg('r','Empty      States         ',(/Nbm(i_sp_pol)+1,E%nb         /))
 enddo
 !
 do ival=1,5
   !
   ! [2] Indirect Gap
   !
   if(ival==1) then
     prefix_= trim(title)//'Indirect Gap    '
     val    =E_g_ind(:,1)*HA2EV
   endif
   !
   ! [3] Indirect Gap Min
   !
   if(ival==2) then
     prefix_= trim(title)//'Direct Gap Min  '
     val    =E_g_dir(:,1)*HA2EV
   endif
   !
   ! [4] Indirect Gap Max
   !
   if(ival==3) then
     prefix_= trim(title)//'Direct Gap Max  '
     val    =E_g_dir(:,2)*HA2EV
   endif
   !
   ! [5] Valence Band Width
   !
   if(ival==4) then
     prefix_= trim(title)//'Val. Band Width '
     val    =E_g_ind(:,2)*HA2EV
   endif
   !
   ! [6] Indirect Gap
   !
   if(ival==5) then
     prefix_= trim(title)//'Con. Band Width '
     val    =E_g_ind(:,3)*HA2EV
   endif
   !
   if(all(Nbf==Nbm)) call msg('r',prefix_(1:25+len_trim(what))//'',val(1),' [eV] ')
   !
   if(n_sp_pol==2.and.any(Nbf==Nbm)) then 
     do i_sp_pol=1,n_sp_pol
       if (Nbf(i_sp_pol)/=Nbm(i_sp_pol)) cycle
       if (Nbf(i_sp_pol)==0) cycle
       if (i_sp_pol==1) prefix_=trim(title)//'              [spin up]'
       if (i_sp_pol==2) prefix_=trim(title)//'              [spin dn]'
       call msg('r',prefix_(1:25+len_trim(what))//'',val(i_sp_pol+1),' [eV] ')
     enddo
   endif
   !
 enddo
 !
 ! [3] Report if the system is treated as a metal
 !
 if (.not.all(Nbf==Nbm)) then
   !
   N_met=0
   f_met=0
   do i_sp_pol=1,n_sp_pol
     f_met=f_met+sum(E%f(Nbf(i_sp_pol)+1:Nbm(i_sp_pol),:,i_sp_pol))/sum(spin_occ*real(E%nk*(Nbm(:)-Nbf(:))))
     N_met=N_met+sum(E%f(Nbf(i_sp_pol)+1:Nbm(i_sp_pol),:,i_sp_pol))/real(E%nk)
   enddo
   !
   call msg('r',trim(title)//'N of el / N of met el   ',       (/nel,N_met/))
   call msg('r',trim(title)//'Average metallic occ.  :            ',(/f_met/))
   !
   if (n_sp_pol==1) call warning(trim(title)//' Metallic system')
   if(n_sp_pol==2) then
     if(all(Nbf/=Nbm)) call warning(trim(title)//' Metallic system')
     if(any(Nbf==Nbm)) call warning(trim(title)//' System is an half-metal')
   endif
   !
 endif
 !
end subroutine
