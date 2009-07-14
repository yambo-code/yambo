!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
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
subroutine QP_descriptions(qp,X,Xw)
 ! 
 ! WARNING ! Changes here must be updated in ioQP as well.
 !
 use units,         ONLY:HARTREE
 use X_m,           ONLY:X_t,use_X_RIM
 use QP_m,          ONLY:QP_t,QP_ng_Sx,QP_n_G_bands,QP_cg_percent,&
&                        QP_dSc_delta,QP_G_damp,QP_dSc_steps,GWo_iterations,&
&                        CHOSEX_use_empties,QP_Sc_steps,QP_G_er,QP_G_dr,QP_solver
 use stderr,        ONLY:string_pack,gen_fmt
 use drivers,       ONLY:l_life,l_ppa,l_ph_corr,l_el_corr,l_chosex
 use R_lattice,     ONLY:RIM_qpg,RIM_n_rand_pts,RIM_ng
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:BZ_RIM_tot_nkpts
 use IO_m,          ONLY:ver_is_gt_or_eq
#if defined _ELPH 
 use ELPH,          ONLY:QP_PH_n_G_bands
#endif
 implicit none
 !
 type(QP_t)::qp
 type(X_t) :: X
 type(w_samp)::Xw
 ! 
 ! Work Space
 !
 integer           :: i4(4),n_descs_save
 integer, external :: QP_state_extract
 !
 n_descs_save=qp%n_descs
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
   if (.not.l_chosex) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,',gen_fmt( i_v=(/QP_dSc_steps /) ),')'))) &
&          ' dS/dw steps            :',QP_dSc_steps
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,',gen_fmt( r_v=(/QP_dSc_delta*HARTREE /) ),')'))) &
&          ' dS/dw step         [ev]:',QP_dSc_delta*HARTREE
   endif
 endif
 !
 qp%n_descs=qp%n_descs+1
 write (qp%description(qp%n_descs),trim(&
&      string_pack('(a,',gen_fmt( (/X%ng /) ),')'))) &
&      ' X G`s            [used]:',X%ng
 qp%n_descs=qp%n_descs+1
 write (qp%description(qp%n_descs),trim(&
&      string_pack('(a,',gen_fmt( (/X%ng_db /) ),')'))) &
&      ' X G`s            [disk]:',X%ng_db
 qp%n_descs=qp%n_descs+1
 write (qp%description(qp%n_descs),trim(&
&      string_pack('(a,2(',gen_fmt( X%ib  ),',1x))'))) &
&      ' X bands                :',X%ib
 qp%n_descs=qp%n_descs+1
 write (qp%description(qp%n_descs),trim(&
&      string_pack('(a,',gen_fmt( r_v=(/X%cg_percentual /) ),')'))) &
&      ' X poles           [o/o]:',X%cg_percentual
 qp%n_descs=qp%n_descs+1
 write (qp%description(qp%n_descs),trim(&
&     string_pack('(a,2(',gen_fmt( r_v=X%ehe*HARTREE ),',1x))'))) &
&     ' X e/h E range      [ev]:',X%ehe*HARTREE
 !
 if (ver_is_gt_or_eq(-1,(/3,0,5/))) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),'(2a)') ' X xc-Kernel            :',trim(X%f_xc)
 endif
 !
 if (.not.l_ppa.and..not.l_chosex) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,2(',gen_fmt( r_v = Xw%dr*HARTREE ),',1x))')))&
&        ' X damping range    [ev]:',Xw%dr*HARTREE
   if (.not.l_life) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,2(',gen_fmt( Xw%n ),',1x))'))) &
&          ' X Steps                :',Xw%n(1)
   endif
 endif
 if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
   qp%n_descs=qp%n_descs+1
                  qp%description(qp%n_descs)=' X BZ energy RIM        : no'
   if (use_X_RIM) qp%description(qp%n_descs)=' X BZ energy RIM        : yes'
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( (/BZ_RIM_tot_nkpts/) ),')'))) &
&        ' X BZ RIM points        :',BZ_RIM_tot_nkpts
 endif
 !
 if (l_life) then
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( r_v=(/QP_cg_percent /) ),')'))) &
&        ' Transitions       [o/o]:',QP_cg_percent
 else
   if ( (.not.l_chosex) .or. (l_chosex.and.CHOSEX_use_empties) ) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,2(',gen_fmt( QP_n_G_bands ),',1x))')))&
&         ' Sc/G bands             :',QP_n_G_bands
   endif
   if (.not.l_chosex) then
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&         string_pack('(a,',gen_fmt( r_v=(/QP_G_damp*HARTREE /) ),')')))&
&         ' Sc/G damping       [ev]:',QP_G_damp*HARTREE
   endif
   !
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&       string_pack('(a,',gen_fmt( (/QP_ng_Sx /) ),')'))) &
&       ' Sx RL components       :',QP_ng_Sx
 endif
 !
#if defined _ELPH 
 !
 ! El-Ph
 !
 if (ver_is_gt_or_eq(-1,(/3,0,1/))) then
   !
   if (l_ph_corr) then
     !
     qp%n_descs=qp%n_descs+1
     if (.not.l_el_corr) qp%n_descs=n_descs_save+1
     !
     if (trim(QP_solver)/='g') then
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( i_v=(/QP_dSc_steps /) ),')'))) &
&            ' [ph] dS/dw steps        :',QP_dSc_steps
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&            string_pack('(a,',gen_fmt( r_v=(/QP_dSc_delta*HARTREE /) ),')'))) &
&            ' [ph] dS/dw step     [ev]:',QP_dSc_delta*HARTREE
       qp%n_descs=qp%n_descs+1
       write (qp%description(qp%n_descs),trim(&
&           string_pack('(a,',gen_fmt( r_v=(/QP_G_damp*HARTREE /) ),')')))&
&           ' [ph] Sc/G damping   [ev]:',QP_G_damp*HARTREE
     endif
     !
     qp%n_descs=qp%n_descs+1
     qp%description(qp%n_descs)=' El-Ph correlation      : yes'
     !
     qp%n_descs=qp%n_descs+1
     write (qp%description(qp%n_descs),trim(&
&          string_pack('(a,2',gen_fmt( (/QP_PH_n_G_bands/) ),')'))) &
&          ' El-Ph Sc. G bands      :',(/1,QP_PH_n_G_bands/)
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
   write (qp%description(qp%n_descs),trim(&
&        string_pack('(a,',gen_fmt( i_v=(/QP_Sc_steps /) ),')'))) &
&        ' S(E) real-axis steps    :',QP_Sc_steps
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&       string_pack('(a,2(',gen_fmt( r_v=QP_G_er*HARTREE ),',1x))'))) &
&        ' S(E) E displacement   [eV]:',QP_G_er*HARTREE
   qp%n_descs=qp%n_descs+1
   write (qp%description(qp%n_descs),trim(&
&       string_pack('(a,2(',gen_fmt( r_v=QP_G_dr*HARTREE ),',1x))'))) &
&        ' S(E) damping          [eV]:',QP_G_dr*HARTREE
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
