!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine OUTPUT_messages(iq,GreenF,Vnl_included,ordering)
 !
 use pars,      ONLY:schlen
 use drivers,   ONLY:l_chi
 use com,       ONLY:msg,com_compose_msg
 use X_m,       ONLY:global_gauge
 use global_XC, ONLY:X_E_xc_string,X_WF_xc_string
 use DIPOLES,   ONLY:DIP_desc
 use interfaces,ONLY:DESC_write
 use R_lattice, ONLY:FineGd_desc
 !
 implicit none
 !
 integer      :: iq
 logical      :: Vnl_included,GreenF
 character(2) :: ordering
 ! WS
 character(schlen) :: files,message
 !
 ! T-ordering
 !
 files='o eps refl eel alpha off beta moke jdos Spm Smp CD'
 if (l_chi) files= 'o eps refl eel alpha'
 if (GreenF) then
   call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] GF poles"),trim(X_E_xc_string(4)),INDENT=0)
 else
   call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] GF Energies"),trim(X_E_xc_string(4)),INDENT=0)
 endif
 call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] Wavefunctions"),trim(X_WF_xc_string(4)),INDENT=0)
 message='T-ordered'
 if (trim(ordering)=='Ra') message='Retarded-antiresonant'
 if (trim(ordering)=='Ta') message='T-ordered-antiresonant'
 if (trim(ordering)=='r')  message='Resonant'
 if (trim(ordering)=='R')  message='Retarded'
 call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] Green`s Function"),trim(message),INDENT=0)
 !
 ! Gauges
 !
 if (iq==1) then
   if (trim(global_gauge)=='length')         message='Length'
   if (trim(global_gauge)=='velocity')       message='Velocity'
   call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] Gauge"),trim(message),INDENT=0)
   call msg(trim(files),com_compose_msg(pre="# ",body="[GEN] [r,Vnl] included"),Vnl_included,INDENT=0)
 endif
 !
 ! Fine Grid
 !
 call DESC_write(trim(files),'#',DIP_desc)
 call DESC_write(trim(files),'#',FineGd_desc)
 !
end subroutine

