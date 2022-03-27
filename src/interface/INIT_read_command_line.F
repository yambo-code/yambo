!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM DS
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
subroutine INIT_read_command_line(rstr,init_)
 !
 use pars,           ONLY:schlen
 use drivers,        ONLY:infile_editing,l_bse,l_alda_fxc,l_chi,l_elel_corr,l_nl_p_and_p,&
&                         l_elel_scatt,l_elph_corr,l_elph_scatt,l_elphoton_corr,&
&                         l_elphoton_scatt,l_em1s,l_em1d,l_ppa,l_phel_scatt,l_phel_corr,l_dipoles
 use BS,             ONLY:BSK_mode
 use BS_solvers,     ONLY:BSS_mode
 use X_m,            ONLY:Chi_mode
 use QP_m,           ONLY:QP_solver
 use memory,         ONLY:USER_MEM_limit_string,GET_user_MEM_limit
 use LIVE_t,         ONLY:USER_wall_time_string,GET_user_WALL_time
 use stderr,         ONLY:STRING_split,STRING_match,STRING_same
 use it_tools,       ONLY:runlevel_is_on,switch_off_runlevel
 use it_m,           ONLY:V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,      &
&                         V_real_time,V_nl_optics,V_all,V_parallel,V_ph,   &
&                         infile_verbosity,nrnlvls,rnlvls
#if defined _SC || defined _RT
 use hamiltonian,    ONLY:H_potential
#endif
#if defined _SC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_pauli,MAG_landau
#endif
#if defined _RT
 use fields,         ONLY:n_ext_fields,n_ext_fields_max
 use stderr,         ONLY:intc
#endif
 !
 implicit none
 !
 integer     :: init_
 character(*):: rstr
 !
 ! Work Space
 !
 integer            ::i1,i2,i3,i_field,n_pieces,i_cycle
 logical            ::l_user_kernel,V_is_defined
 logical, external  ::INIT_read_command_line_potentials
 integer, parameter ::n_strings_not_allowed=10
 character(schlen)  ::rstr_piece(2*nrnlvls),strings_to_not_use_as_runlevels(n_strings_not_allowed),V_string,scattp_piece(10)
 !
 ! Input file editing mode is by default off
 !
 infile_editing=.FALSE.
 !
 ! Keep track if the user provided a kernel option
 !
 l_user_kernel=.FALSE.
 !
 ! Bug fix (17/9/2012). If any string following a -### identifier contains
 ! a string related to a runlevel this is erronously switched on.
 !
 strings_to_not_use_as_runlevels=" "
 strings_to_not_use_as_runlevels( 1)="jobstr"
 strings_to_not_use_as_runlevels( 2)="ifile"
 strings_to_not_use_as_runlevels( 3)="idir"
 strings_to_not_use_as_runlevels( 4)="odir"
 strings_to_not_use_as_runlevels( 5)="cdir"
 strings_to_not_use_as_runlevels( 6)="com_path"
 strings_to_not_use_as_runlevels( 7)="more_io_path"
 strings_to_not_use_as_runlevels( 8)="core_io_path"
 strings_to_not_use_as_runlevels( 9)="wallt"
 strings_to_not_use_as_runlevels(10)="memorylimit"
 !
 ! Split the string in pieces
 !
 call STRING_split(rstr,rstr_piece)
 n_pieces=0
 do i1=1,2*nrnlvls
   if (len_trim(rstr_piece(i1))>0) n_pieces=n_pieces+1
 enddo
 if (n_pieces==0) return
 !
 do i_cycle=1,2
   !
   INPUT_strings_loop: do i1=1,n_pieces
     !
     if (trim(rstr_piece(i1))=='ifile') cycle
     !
     if ( trim(rstr_piece(i1)) == 'wallt' ) then
       USER_wall_time_string=trim(rstr_piece(i1+1))
       if (i_cycle==1) call GET_user_WALL_time()
     endif
     !
     if ( trim(rstr_piece(i1)) == 'memorylimit' ) then
       USER_MEM_limit_string=trim(rstr_piece(i1+1))
       if (i_cycle==1) call GET_user_MEM_limit()
     endif
     !
     ! String suitable for runlevel switching?
     !
     do i2=1,n_strings_not_allowed
       if (trim(rstr_piece(i1)) == trim(strings_to_not_use_as_runlevels(i2))) cycle INPUT_strings_loop
     enddo
     !
     ! Input File editing?
     !
     do i2=1,nrnlvls
       if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i1))) infile_editing=.true.
     enddo
     !
     ! Run Levels
     !
     call initactivate(1, trim(rstr_piece(i1)) )
     !
     ! Verbosity
     ! V_RL=1
     ! V_kpt=2
     ! V_sc=3
     ! V_qp=4
     ! V_io=5
     ! V_general=6
     ! V_resp=7
     ! V_real_time=8
     ! V_parallel=9
     ! V_nl_optics=10
     ! V_ph=11
     ! V_all=99
     !
     if ( trim(rstr_piece(i1)) == 'infver' ) then
       if (STRING_same( trim(rstr_piece(i1+1)) , "rl" )) infile_verbosity=V_RL
       if (STRING_same( trim(rstr_piece(i1+1)) , "kpt").or.STRING_same( trim(rstr_piece(i1+1)) , "k")) infile_verbosity=V_kpt
       if (STRING_same( trim(rstr_piece(i1+1)) , "sc" )) infile_verbosity=V_sc
       if (STRING_same( trim(rstr_piece(i1+1)) , "qp" )) infile_verbosity=V_qp
       if (STRING_same( trim(rstr_piece(i1+1)) , "io" )) infile_verbosity=V_io
       if (STRING_same( trim(rstr_piece(i1+1)) , "gen")) infile_verbosity=V_general
       if (STRING_same( trim(rstr_piece(i1+1)) , "resp").or.STRING_same( trim(rstr_piece(i1+1)) , "x")) infile_verbosity=V_resp
       if (STRING_same( trim(rstr_piece(i1+1)) , "rt")) infile_verbosity=V_real_time
       if (STRING_same( trim(rstr_piece(i1+1)) , "nl")) infile_verbosity=V_nl_optics
       if (STRING_same( trim(rstr_piece(i1+1)) , "ph")) infile_verbosity=V_ph
       if (STRING_same( trim(rstr_piece(i1+1)) , "par")) infile_verbosity=V_parallel
       if (STRING_same( trim(rstr_piece(i1+1)) , "all")) infile_verbosity=V_all
     endif
     !
     if ( trim(rstr_piece(i1)) == 'dipoles' )  then
       call initactivate(1,'dipoles')
     endif
     !
     if ( trim(rstr_piece(i1)) == 'screen') then
       l_em1s= STRING_same(trim(rstr_piece(i1+1)),'s')
       l_em1d= (STRING_same(trim(rstr_piece(i1+1)),'f') )
       l_ppa = (STRING_same(trim(rstr_piece(i1+1)),'p') )
       if (l_em1s) call initactivate(1,'em1s')
       if (l_em1d) call initactivate(1,'em1d')
       if (l_ppa)  call initactivate(1,'ppa')
     endif
     !
     ! BSE/LLR
     !
     if (i_cycle==2.and.trim(rstr_piece(i1)) == 'optics' )  then
       !
       l_chi= (STRING_same(trim(rstr_piece(i1+1)),'g') ).or.(STRING_same(trim(rstr_piece(i1+1)),'c'))
       l_bse= (STRING_same(trim(rstr_piece(i1+1)),'eh')).or.(STRING_same(trim(rstr_piece(i1+1)),'b'))
       !
       if (.not.l_chi.and..not.l_bse) l_chi=.true.
       !
       call initactivate(1,'optics')
       if (l_chi) call initactivate(1,'chi')
       if (l_bse) call initactivate(1,'bse')
       !
       if (l_chi.and.trim(Chi_mode)==' ') Chi_mode='IP'
       if (l_bse.and.trim(BSK_mode)==' ') BSK_mode='IP'
       !
     endif
     !
     ! Approximation used for the BSE/LLR kernel
     !
     if ( i_cycle==2 .and. trim(rstr_piece(i1)) == 'kernel' )  then
       !
       BSK_mode='HARTREE'
       Chi_mode='HARTREE'
       !
       if(l_bse)  then
         if(STRING_same(trim(rstr_piece(i1+1))  , 'hartree'))  BSK_mode='HARTREE'
         if(STRING_same(trim(rstr_piece(i1+1))  , 'hf'))       BSK_mode='HF'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'alda'))     BSK_mode='ALDA'
         if(STRING_same(trim(rstr_piece(i1+1))  , 'sex'))      BSK_mode='SEX'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'bsfxc'))    BSK_mode='BSfxc'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'ip'))       BSK_mode='IP'
       else if(l_chi.or.l_em1s.or.l_em1d.or.l_ppa) then
         if(STRING_same(trim(rstr_piece(i1+1))  , 'hartree'))  Chi_mode='HARTREE'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'alda'))     Chi_mode='ALDA'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'lrc'))      Chi_mode='LRC'
         if( STRING_same(trim(rstr_piece(i1+1)) , 'bsfxc'))    Chi_mode='BSfxc'
       endif
       !
       if((STRING_same(trim(rstr_piece(i1+1)) , 'alda')).or.&
&             (STRING_same(trim(rstr_piece(i1+1)) , 'lrc')).or.&
&             (STRING_same(trim(rstr_piece(i1+1)) , 'pf')).or.&
&             (STRING_same(trim(rstr_piece(i1+1)) , 'bsfxc'))) call initactivate(1,'tddft')
       !
       l_user_kernel=.TRUE.
       !
     endif
     !
     ! BSE Solver
     !
     if ( trim(rstr_piece(i1)) == 'bss' )  then
       BSS_mode=trim(rstr_piece(i1+1))
       if (.not.STRING_match(BSS_mode,'h').and..not.STRING_match(BSS_mode,'d').and.&
&          .not.STRING_match(BSS_mode,'i').and..not.STRING_match(BSS_mode,'t').and.&
&          .not.STRING_match(BSS_mode,'s')) BSS_mode='h'
       !
       ! With and ALDA Fxc the t solver is not permitted
       !
       if (l_alda_fxc.and.STRING_match(BSS_mode,'t')) BSS_mode='h'
       if (BSK_mode=='IP') call switch_off_runlevel('bss',on_name=' ')
       if (BSK_mode==' ') then
         BSK_mode="SEX"
         Chi_mode="HARTREE"
       endif
       !
     endif
     !
     ! Dyson Solver
     !
     if ( trim(rstr_piece(i1)) == 'dyson' ) then
       QP_solver=trim(rstr_piece(i1+1))
       if (.not.any((/trim(QP_solver)=='n',trim(QP_solver)=='s',trim(QP_solver)=='g',trim(QP_solver)=='r'/)))  QP_solver='n'
#if !defined  _ELPH && !defined _QED
       l_elel_corr=.true.
#endif
       !
       if ( i_cycle == 2 .and. trim(Chi_mode)==' ') Chi_mode='HARTREE'
       !
     endif
     !
     ! Lifetimes
     !
     if ( i_cycle==2 .and. trim(rstr_piece(i1)) == 'life' )  then
#if defined _ELPH || defined _QED || defined _PHEL
       if ( (.not.l_elph_corr.and..not.l_elphoton_corr.and..not.l_phel_corr) .or. l_elel_corr) then
         call initactivate(1,'em1d')
         call initactivate(1,'el_el_corr')
       endif
#else
       call initactivate(1,'em1d')
       l_elel_corr=.true.
#endif
#if defined _QED
       if (l_elphoton_corr) then
         l_dipoles=.TRUE.
         call initactivate(1,'dipoles')
       endif
#endif
     endif
     !
     ! GW approximation
     !
     if ( trim(rstr_piece(i1)) == 'gw0' ) then
       !
       call initactivate(1,'gw0')
       !
       if (trim(rstr_piece(i1+1))=='p') then
         infile_editing=.true.
         l_ppa=.TRUE.
         if (i_cycle==1) QP_solver="n"
         call initactivate(1,'ppa')
       else if (trim(rstr_piece(i1+1))=='c') then
         infile_editing=.true.
         if (i_cycle==1) QP_solver="n"
         call initactivate(1,'cohsex dyson')
       else if (trim(rstr_piece(i1+1))=='r') then
         infile_editing=.true.
         if (i_cycle==1) QP_solver="s"
         call initactivate(1,'em1d')
#if defined  _ELPH 
       else if (trim(rstr_piece(i1+1))=='fan') then
         infile_editing=.true.
         if (i_cycle==1) QP_solver="n"
         call initactivate(1,'el_ph_corr')
         l_elph_corr=.TRUE.
#endif
#if defined _PHEL
       else if (trim(rstr_piece(i1+1))=='X') then
         infile_editing=.true.
         if (i_cycle==1) QP_solver="n"
         call initactivate(1,'ph_el_corr')
         l_phel_corr=.TRUE.
#endif
       endif
       !
     endif
     !
#if defined _SC || defined _RT
     !
     if ( trim(rstr_piece(i1)) == 'potential' )  then
       !
       H_potential=""
       V_string=trim(rstr_piece(i1+1))
       P_loop: do i3=i1+2,n_pieces
         do i2=1,nrnlvls
           if ( trim(rnlvls(i2,1)) == trim(rstr_piece(i3))) exit P_loop
         enddo
         V_string=trim(V_string)//" "//trim(rstr_piece(i3))
       enddo P_loop
     endif
     !
#endif
     !
#if defined _SC
     !
     ! SC
     !
     if ( trim(rstr_piece(i1)) == 'potential' )  then
       !
       V_is_defined=INIT_read_command_line_potentials(V_string,"exx")
       if (i_cycle==2.and..not.runlevel_is_on('negf').and.V_is_defined) call initactivate(1,'em1s')
       V_is_defined=INIT_read_command_line_potentials(V_string,"exxc")
       V_is_defined=INIT_read_command_line_potentials(V_string,"srpa")
       V_is_defined=INIT_read_command_line_potentials(V_string,"coh")
       if (i_cycle==2.and..not.runlevel_is_on('negf').and.V_is_defined) call initactivate(1,'em1s')
       V_is_defined=INIT_read_command_line_potentials(V_string,"sex")
       if (i_cycle==2.and..not.runlevel_is_on('negf').and.V_is_defined) call initactivate(1,'em1s')
       V_is_defined=INIT_read_command_line_potentials(V_string,"ldax")
       V_is_defined=INIT_read_command_line_potentials(V_string,"pz")
       V_is_defined=INIT_read_command_line_potentials(V_string,"gs")
       V_is_defined=INIT_read_command_line_potentials(V_string,"d")
       V_is_defined=INIT_read_command_line_potentials(V_string,"ip")
       V_is_defined=INIT_read_command_line_potentials(V_string,"f")
       V_is_defined=INIT_read_command_line_potentials(V_string,"h")
       V_is_defined=INIT_read_command_line_potentials(V_string,"cvonly")
       !
     endif
#endif
     !
#if defined _RT
     !
     ! NEGF
     !
     if ( trim(rstr_piece(i1)) == 'potential' )  then
       !
       V_is_defined=INIT_read_command_line_potentials(V_string,"coh")
       if (i_cycle==2.and..not.runlevel_is_on('negf').and.V_is_defined) call initactivate(1,'em1s')
       V_is_defined=INIT_read_command_line_potentials(V_string,"sex")
       if (i_cycle==2.and..not.runlevel_is_on('negf').and.V_is_defined) call initactivate(1,'em1s')
       V_is_defined=INIT_read_command_line_potentials(V_string,"gs")
       V_is_defined=INIT_read_command_line_potentials(V_string,"d")
       V_is_defined=INIT_read_command_line_potentials(V_string,"ip")
       V_is_defined=INIT_read_command_line_potentials(V_string,"f")
       V_is_defined=INIT_read_command_line_potentials(V_string,"h")
       V_is_defined=INIT_read_command_line_potentials(V_string,"cvonly")
       !
     endif
#endif
     !
#if defined _SC
     if ( trim(rstr_piece(i1)) == 'magnetic' )  then
       if ( STRING_match(rstr_piece(i1+1),'p') ) then
         MAG_hamiltonian_type='pauli'
         MAG_pauli=.true.
       else if ( STRING_match(rstr_piece(i1+1),'l') ) then
         MAG_hamiltonian_type='landau'
         MAG_landau=.true.
       else if ( STRING_match(rstr_piece(i1+1),'a') ) then
         MAG_hamiltonian_type='all'
         MAG_pauli=.true.
         MAG_landau=.true.
       endif
     endif
#endif
     !
     ! Correlation(s)
     call INIT_read_command_line_corr_scatt("corrtp",rstr_piece,i1,l_elel_corr,l_elph_corr,l_phel_corr,l_elphoton_corr)
     !
     ! Scattering(s)
     call INIT_read_command_line_corr_scatt("scattp",rstr_piece,i1,l_elel_scatt,l_elph_scatt,l_phel_scatt,l_elphoton_scatt)
     !
#if defined _RT
     !
     ! RT
     !
     if ( trim(rstr_piece(i1)) == 'negf' ) then
       !
       if ( trim(rstr_piece(i1+1)) == 'p'    ) n_ext_fields=1
       if ( trim(rstr_piece(i1+1)) == 'pp'   ) n_ext_fields=2
       !
       do i_field=1,n_ext_fields_max
         if ( index(rstr_piece(i1+1),'p'//trim(intc(i_field))) > 0 ) n_ext_fields=i_field
       enddo
       !
       if( n_ext_fields==0) n_ext_fields=1
       !
       if ( i_cycle == 2 .and. .not. V_is_defined ) H_potential='IP'
       !
     endif
#endif
     !
#if defined _NL
     !
     ! NL
     !
     if ( trim(rstr_piece(i1)) == 'nloptics' ) then
       !
       if ( trim(rstr_piece(i1+1)) == 'n'   ) n_ext_fields=1
       if ( trim(rstr_piece(i1+1)) == 'p'   ) then
          n_ext_fields=2
          l_nl_p_and_p=.TRUE.
       endif
       !
       if( n_ext_fields==0) n_ext_fields=1
       !
     endif
     !
#endif
     !
#if defined _SC || defined _RT
     !
     ! COLLISIONS
     !
     if (runlevel_is_on('collisions')) then
       if ( l_elel_scatt.or.trim(H_potential)=='COH+SEX'.or.&
&               trim(H_potential)=='COH'.or.trim(H_potential)=='SEX' ) call initactivate(1,'em1s')
     endif
     !
#endif
     !
   enddo INPUT_strings_loop
   !
 enddo
 !
 if (.not.l_user_kernel.and.len_trim(Chi_mode)==0) Chi_mode='HARTREE'
 !
 if (infile_editing) init_=1
 !
end subroutine
