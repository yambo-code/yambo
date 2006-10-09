!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 subroutine TDDFT_descriptions(W)
!================================
 use pars,          ONLY:HARTREE
 use TDDFT,         ONLY:FXC_n_descs,FXC_description,FXC_n_g_corr,&
&                        FXC_is_causal
 use stderr,        ONLY:string_pack
 use com,           ONLY:gen_fmt
 use frequency,     ONLY:w_samp
 implicit none
 type(w_samp)::W

 FXC_n_descs=1
 write(FXC_description(FXC_n_descs),&
&      trim(string_pack('(a,',gen_fmt((/FXC_n_g_corr/)),')'))) &
&      '# Fxc|RL components  [disk]:',FXC_n_g_corr
!
 FXC_n_descs=FXC_n_descs+1
 if (FXC_is_causal) then
   FXC_description(FXC_n_descs)='#    |Kernel is causal     : yes'
 else
   FXC_description(FXC_n_descs)='#    |Kernel is causal     : no'
 endif

 FXC_n_descs=FXC_n_descs+1
 write (FXC_description(FXC_n_descs),trim(&
&       string_pack('(a,2(',gen_fmt(r_v=W%er*HARTREE),'))'))) &
&       '#    |Energy range     [ev]:',W%er*HARTREE

 FXC_n_descs=FXC_n_descs+1
 write (FXC_description(FXC_n_descs),trim(&
&       string_pack('(a,2(',gen_fmt(r_v=W%dr*HARTREE),'))'))) &
&       '#    |Damping range    [ev]:',W%dr*HARTREE

 FXC_n_descs=FXC_n_descs+1
 write(FXC_description(FXC_n_descs),&
&      trim(string_pack('(a,2(',gen_fmt(W%n),'))'))) &
&      '#    |Energy steps         :',W%n

 end subroutine
!
