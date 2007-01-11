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
 subroutine K_descriptions(iq,q,X)
!=================================
 use pars,          ONLY:SP,HARTREE
 use X_m,           ONLY:X_t,alpha_dim
 use IO_m,          ONLY:ver_is_gt_or_eq
 use BS,            ONLY:BSS_n_descs,BSS_description,&
&                        BS_eh_win,BS_n_g_exch,BS_eh_en,&
&                        BS_n_g_W,BS_bands,BS_K_dim,BS_res_mode,BS_res_K_exchange,&
&                        BS_res_K_corr,BSS_q0,BS_cpl_mode
 use stderr,        ONLY:string_pack
 use R_lattice,     ONLY:bz_samp,RIM_qpg,RIM_n_rand_pts,RIM_ng
 use com,           ONLY:gen_fmt
 implicit none
 integer      ::iq
 type(X_t)    ::X
 type(bz_samp)::q
!ws
 real(SP)     ::v(3)

 if (iq==1) v=BSS_q0
 if (iq/=1) v=q%pt(iq,:)
 BSS_n_descs=BSS_n_descs+1
 if (trim(alpha_dim)/='adim') then
   write (BSS_description(BSS_n_descs),trim(string_pack(&
&        '(3a,i3,a,3(',gen_fmt(r_v=v),',1x))'))) '# Polarization (-<<X>>) [',&
&        trim(alpha_dim),'] @ IQ',iq,' : ',v
 else
   write (BSS_description(BSS_n_descs),trim(string_pack(&
&        '(a,i3,a,3(',gen_fmt(r_v=v),',1x))'))) '# Absorption @ IQ',iq,' : ',v
 endif

!
 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='#'
!
 BSS_n_descs=BSS_n_descs+1
 write(BSS_description(BSS_n_descs),'(2a)') '# BSK|Resonant Mode        :',BS_res_mode
!
 BSS_n_descs=BSS_n_descs+1
 write(BSS_description(BSS_n_descs),'(2a)') '#    |Coupling Mode        :',BS_cpl_mode
! 
 BSS_n_descs=BSS_n_descs+1
 write(BSS_description(BSS_n_descs),trim(&
&      string_pack( '(a,',gen_fmt((/BS_K_dim/)),')' ))) &
&      '#    |Dimension            :',BS_K_dim
!
 BSS_n_descs=BSS_n_descs+1
 write(BSS_description(BSS_n_descs),trim(&
&      string_pack('(a,2(',gen_fmt(BS_bands),'))'))) '#    |Bands                :',BS_bands
!
 if (BS_res_K_corr) then
   BSS_n_descs=BSS_n_descs+1
   write(BSS_description(BSS_n_descs),&
&        trim(string_pack('(a,',gen_fmt((/BS_n_g_W/)),')'))) &
&        '#    |RL vecs [correlation]:',BS_n_g_W
 endif
!
 if (BS_res_K_exchange) then
   BSS_n_descs=BSS_n_descs+1
   write(BSS_description(BSS_n_descs),&
&        trim(string_pack('(a,',gen_fmt((/BS_n_g_exch/)),')'))) &
&        '#    |RL vecs    [exchange]:',BS_n_g_exch
 endif
!
 BSS_n_descs=BSS_n_descs+1
 write (BSS_description(BSS_n_descs),trim(&
&       string_pack('(a,2(',gen_fmt(r_v=BS_eh_en*HARTREE),'))'))) &
&       '#    |E/h energy range [ev]:',BS_eh_en*HARTREE
!
 BSS_n_descs=BSS_n_descs+1
 write (BSS_description(BSS_n_descs),trim(&
&       string_pack('(a,2(',gen_fmt(r_v=BS_eh_win*HARTREE),'))'))) &
&       '#    |Coupling range  [o/o]:',BS_eh_win
!
 if (BS_res_K_corr) then
   BSS_n_descs=BSS_n_descs+1
   write (BSS_description(BSS_n_descs),trim(&
&         string_pack('(a,2(',gen_fmt(X%ib),'))'))) '#  X |Bands                :',X%ib

   BSS_n_descs=BSS_n_descs+1
   write(BSS_description(BSS_n_descs),trim(&
&        string_pack('(a,',gen_fmt((/X%ng_db/)),')'))) '#    |RL vecs              :',X%ng_db

   BSS_n_descs=BSS_n_descs+1
   write (BSS_description(BSS_n_descs),trim(&
&         string_pack('(a,2(',gen_fmt(r_v=X%ehe*HARTREE),'))'))) &
&         '#    |E/h energy range [ev]:',X%ehe*HARTREE

   BSS_n_descs=BSS_n_descs+1
   write (BSS_description(BSS_n_descs),&
&         trim(string_pack('(a,',gen_fmt(r_v=(/X%cg_percentual/)),')'))) &
&         '#    |Poles           [o/o]:',X%cg_percentual

   BSS_n_descs=BSS_n_descs+1
   write (BSS_description(BSS_n_descs),trim(&
&         string_pack('(a,3(',gen_fmt(r_v=X%q0),'))'))) &
&         '#    |Field direction  [cc]:',X%q0

   if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
     BSS_n_descs=BSS_n_descs+1
     write (BSS_description(BSS_n_descs),'(2a)') &
&          '#    |xc-Kernel            :',trim(X%f_xc)
   endif 

   if (allocated(RIM_qpg)) then
     BSS_n_descs=BSS_n_descs+1
     write (BSS_description(BSS_n_descs),trim(&
&          string_pack('(a,',gen_fmt( (/RIM_ng /) ),')'))) &
&          '# RIM|RL components   [col]:',RIM_ng
     BSS_n_descs=BSS_n_descs+1
     write (BSS_description(BSS_n_descs),trim(&
&          string_pack('(a,',gen_fmt( (/RIM_n_rand_pts /) ),')'))) &
&          '#    |Random points   [col]:',RIM_n_rand_pts
   endif
 endif

 BSS_n_descs=BSS_n_descs+1
 BSS_description(BSS_n_descs)='#'

 end subroutine
!
