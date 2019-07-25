!
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
subroutine QP_descriptions(qp,X,Xw,Update)
 ! 
 ! WARNING ! Changes here must be updated in io_QP_and_GF as well.
 !
 use units,         ONLY:HA2EV
 use X_m,           ONLY:X_t,use_X_DbGd
 use QP_m,          ONLY:QP_t,QP_ng_Sx,QP_n_G_bands,QP_cg_percent,&
&                        QP_dSc_delta,QP_G_damp,QP_dSc_steps,GWo_iterations,&
&                        COHSEX_use_empties,QP_Sc_steps,QP_G_er,QP_G_dr,QP_solver,&
&                        use_GreenF_to_eval_QP,GF_is_causal,QP_G_Zoom_treshold,&
&                        GW_terminator_E,l_GW_terminator,GW_terminator_Kind
 use stderr,        ONLY:string_pack,gen_fmt
 use drivers,       ONLY:l_life,l_ppa,l_elel_corr,l_cohsex,&
&                        l_HF_and_locXC
 use R_lattice,     ONLY:RIM_qpg,RIM_n_rand_pts,RIM_ng
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:BZ_DbGd_tot_nkpts
 use IO_m,          ONLY:ver_is_gt_or_eq
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr 
 use ELPH,          ONLY:QP_PH_n_G_bands,elph_branches,elph_nDBs_used,&
&                        FAN_deltaE_treshold,DW_deltaE_treshold
#endif
 implicit none
 !
 type(QP_t)  ::qp
 type(X_t)   ::X
 type(w_samp)::Xw
 logical     ::Update
 ! 
 ! Work Space
 !
 integer           :: i4(4)
 integer, save     :: n_descs_save
 integer, external :: QP_state_extract
 !
 if (.not.Update) n_descs_save=qp%n_descs
 if (     Update) qp%n_descs  =n_descs_save
 !
 if (allocated(RIM_qpg)) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( (/RIM_ng /) ),')'))) &
&        ' RIM G`s                :',RIM_ng
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( (/RIM_n_rand_pts /) ),')'))) &
&        ' RIM random pts         :',RIM_n_rand_pts
 endif
 !
 if (.not.l_life) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( i_v=(/GWo_iterations /) ),')'))) &
&        ' GW SC iterations       :',GWo_iterations
   !
   if (.not.l_cohsex.and.trim(QP_solver)/='g'.and..not.use_GreenF_to_eval_QP) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,',gen_fmt( i_v=(/QP_dSc_steps /) ),')'))) &
&          ' dS/dw steps            :',QP_dSc_steps
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,',gen_fmt( r_v=(/QP_dSc_delta*HA2EV /) ),')'))) &
&          ' dS/dw step         [ev]:',QP_dSc_delta*HA2EV
   endif
 endif 
 !
 if (l_elel_corr) then
   !
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( (/X%ng /) ),')'))) &
&        ' X G`s            [used]:',X%ng
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( (/X%ng_db /) ),')'))) &
&        ' X G`s            [disk]:',X%ng_db
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,2(',gen_fmt( X%ib  ),',1x))'))) &
&        ' X bands                :',X%ib
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( r_v=(/X%cg_percentual /) ),')'))) &
&        ' X poles           [o/o]:',X%cg_percentual
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&       string_pack('(a,2(',gen_fmt( r_v=X%ehe*HA2EV ),',1x))'))) &
&       ' X e/h E range      [ev]:',X%ehe*HA2EV
   !
   if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),'(2a)') ' X xc-Kernel            : ',trim(X%f_xc)
   endif
   !
   if (.not.l_ppa.and..not.l_cohsex) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,2(',gen_fmt( r_v = Xw%dr*HA2EV ),',1x))')))&
&          ' X damping range    [ev]:',Xw%dr*HA2EV
     if (.not.l_life) then
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,(',gen_fmt( i_v=(/Xw%n_freqs/) ),',1x))'))) &
&            ' X Steps                :',Xw%n_freqs
     endif
   endif
   if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
     qp%n_descs=qp%n_descs+1
                    qp%description(qp%n_descs)=' X BZ energy Double Grid: no'
     if (use_X_DbGd) qp%description(qp%n_descs)=' X BZ energy Double Grid: yes'
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,',gen_fmt( (/BZ_DbGd_tot_nkpts/) ),')'))) &
&          ' X BZ Double Grid points:',BZ_DbGd_tot_nkpts
   endif
 endif
 !
 if (l_life.and.l_elel_corr) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( r_v=(/QP_cg_percent /) ),')'))) &
&        ' Transitions       [o/o]:',QP_cg_percent
 endif
 if (.not.l_life) then 
   if ( (.not.l_cohsex) .or. (l_cohsex.and.COHSEX_use_empties) ) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,2(',gen_fmt( QP_n_G_bands ),',1x))')))&
&         ' Sc/G bands             :',QP_n_G_bands
   endif
   if (.not.l_cohsex) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,',gen_fmt( r_v=(/QP_G_damp*HA2EV /) ),')')))&
&         ' Sc/G damping       [ev]:',QP_G_damp*HA2EV
   endif
   !
   if (l_GW_terminator) then
     qp%n_descs=qp%n_descs+1
     qp%description(qp%n_descs)=' Sc bands terminator    : yes'
     qp%n_descs=qp%n_descs+1
     qp%description(qp%n_descs)=' Sc terminator kind     : '//trim(GW_terminator_Kind)    
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&           string_pack('(a,',gen_fmt( r_v=(/GW_terminator_E*HA2EV /) ),')'))) &
&           ' Sc Terminator pole     :',GW_terminator_E*HA2EV
   else
     qp%n_descs=qp%n_descs+1
     qp%description(qp%n_descs)=' Sc bands terminator    : no'
   endif
   !
   if (l_HF_and_locXC) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,',gen_fmt( (/QP_ng_Sx /) ),')'))) &
&         ' Sx RL components       :',QP_ng_Sx
   endif
   !
 endif
 !
 !
#if defined _ELPH 
 !
 ! El-Ph
 !
 if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
   !
   if (l_elph_corr) then
     !
     qp%n_descs=qp%n_descs+1
     if (.not.l_elel_corr) qp%n_descs=n_descs_save+1
     !
     if (trim(QP_solver)/='g') then
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( i_v=(/QP_dSc_steps /) ),')'))) &
&            ' [ph] dS/dw steps        :',QP_dSc_steps
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( r_v=(/QP_dSc_delta*HA2EV /) ),')'))) &
&            ' [ph] dS/dw step     [ev]:',QP_dSc_delta*HA2EV
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&           string_pack('(a,',gen_fmt( r_v=(/QP_G_damp*HA2EV /) ),')')))&
&           ' [ph] Sc/G damping   [ev]:',QP_G_damp*HA2EV
     endif
     !
     qp%n_descs=qp%n_descs+1
     qp%description(qp%n_descs)=' El-Ph correlation      : yes'
     !
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,2',gen_fmt( (/QP_PH_n_G_bands/) ),')'))) &
&          ' El-Ph Sc. G bands      :',(/1,QP_PH_n_G_bands/)
     !
     if (ver_is_gt_or_eq(-1,(/3,2,2/))) then
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,2',gen_fmt( elph_branches ),')'))) &
&            ' El-Ph phonon branches  :',(/elph_branches/)
     endif
     !
     if (ver_is_gt_or_eq(-1,(/4,0,0/))) then
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( i_v=(/elph_nDBs_used/) ),')'))) &
&            ' El-Ph momenta used     :',elph_nDBs_used
     endif
     !
     if (ver_is_gt_or_eq(-1,revision=4400)) then
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( r_v=(/FAN_deltaE_treshold*HA2EV /) ),')')))&
&            ' Energy treshold   [FAN]:',FAN_deltaE_treshold*HA2EV
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( r_v=(/DW_deltaE_treshold*HA2EV /) ),')')))&
&            ' Energy treshold    [DW]:',DW_deltaE_treshold*HA2EV
     endif
     !
   endif
   !
 endif
 !
#endif
 !
 ! Green's Functions
 !
 if (ver_is_gt_or_eq(-1,revision=452).and.trim(QP_solver)=='g') then
   qp%n_descs=qp%n_descs+1
   qp%description(qp%n_descs)=' [GF] GF is causal       : no'
   if (GF_is_causal) qp%description(qp%n_descs)=' [GF] GF is causal       : yes'
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( i_v=(/QP_Sc_steps /) ),')'))) &
&        ' [GF] Real-axis steps    :',QP_Sc_steps
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&     string_pack('(a,2(',gen_fmt( r_v=QP_G_er*HA2EV ),',1x))'))) &
&      ' [GF] Initial E range  [eV]:',QP_G_er*HA2EV
   if (ver_is_gt_or_eq(-1,revision=530)) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,1(',gen_fmt( r_v=(/QP_G_Zoom_treshold/)),',1x))'))) &
&          ' [GF] Zoom treshold   [o/o]:',QP_G_Zoom_treshold
   endif
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&       string_pack('(a,2(',gen_fmt( r_v=QP_G_dr*HA2EV ),',1x))'))) &
&        ' [GF] Damping          [eV]:',QP_G_dr*HA2EV
 endif
 !
 ! QP states
 !
 i4=(/1,1,0,0/)
 do while(QP_state_extract(i4)>0)
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),&
&        '(4(a,i3.3))') ' QP @ K ',i4(1),' - ',i4(2),' : b ',i4(3),' - ',i4(4)
 enddo
 !
end subroutine
