!
!        Copyright (C) 2000-2019 the YAMBO team
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
 use drivers,        ONLY:infile_editing,l_bse,l_alda_fxc,l_chi,l_elel_corr,&
&                         l_elel_scatt,l_elph_corr,l_elph_scatt,l_elphoton_corr,l_elphoton_scatt
 use BS,             ONLY:BSK_mode
 use BS_solvers,     ONLY:BSS_mode
 use X_m,            ONLY:Chi_mode
 use QP_m,           ONLY:QP_solver
 use LIVE_t,         ONLY:USER_wall_time_string,GET_user_WALL_time
 use stderr,         ONLY:STRING_split
 use it_m,           ONLY:V_RL,V_kpt,V_sc,V_qp,V_io,V_general,V_resp,      &
&                         V_real_time,V_nl_optics,V_all,V_parallel,        &
&                         runlevel_is_on,switch_off_runlevel,initactivate, &
&                         infile_verbosity,nrnlvls,rnlvls
#if defined _SC || defined _RT
 use hamiltonian,    ONLY:H_potential
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
 integer          ::i1,i2,i_field,n_pieces,i_cycle
 character(schlen)::rstr_piece(2*nrnlvls),strings_to_not_use_as_runlevels(nrnlvls)
 !
 ! Bug fix (17/9/2012). If any string following a -### identifier contains 
 ! a string related to a runlevel this is erronously switched on. 
 !
 strings_to_not_use_as_runlevels=" "
 strings_to_not_use_as_runlevels(1)="jobstr"
 strings_to_not_use_as_runlevels(2)="ifile"
 strings_to_not_use_as_runlevels(3)="idir"
 strings_to_not_use_as_runlevels(4)="odir"
 strings_to_not_use_as_runlevels(5)="cdir"
 strings_to_not_use_as_runlevels(6)="com_path"
 strings_to_not_use_as_runlevels(7)="more_io_path"
 strings_to_not_use_as_runlevels(8)="core_io_path"
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
     ! String suitable for runlevel switching?
     !
     if (i1>1) then
       do i2=1,nrnlvls
         if (trim(rstr_piece(i1-1)) == trim(strings_to_not_use_as_runlevels(i2))) cycle INPUT_strings_loop
       enddo
     endif
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
     ! V_all=99
     !
     if ( trim(rstr_piece(i1)) == 'infver' ) then
       select case (trim(rstr_piece(i1+1)))
         case ('RL','rl')
           infile_verbosity=V_RL
         case ('kpt','k')
           infile_verbosity=V_kpt
         case ('sc','SC')
           infile_verbosity=V_sc
         case ('QP','qp')
           infile_verbosity=V_qp
         case ('IO','io')
           infile_verbosity=V_io
         case ('gen')
           infile_verbosity=V_general
         case ('resp','X')
           infile_verbosity=V_resp
         case ('rt')
           infile_verbosity=V_real_time
         case ('nl')
           infile_verbosity=V_nl_optics
         case ('par')
           infile_verbosity=V_parallel
         case ('all')
           infile_verbosity=V_all
       end select
     endif
     !
     if ( trim(rstr_piece(i1)) == 'wallt' ) then
       USER_wall_time_string=trim(rstr_piece(i1+1))
       if (i_cycle==1) call GET_user_WALL_time()
     endif
     !
     if ( trim(rstr_piece(i1)) == 'dipoles' )  then
       call initactivate(1,'dipoles')
     endif
     !
     if ( trim(rstr_piece(i1)) == 'em1s' .or.  trim(rstr_piece(i1)) == 'em1d' .or.&
&             runlevel_is_on('em1s')         .or.  runlevel_is_on('em1d')  .or. &
&             runlevel_is_on('life')         .or.  runlevel_is_on('cohsex') .or. &
&             runlevel_is_on('ppa')) &
&           Chi_mode='HARTREE'
     !
     ! BSE/LLR
     !
     if (i_cycle==2.and.trim(rstr_piece(i1)) == 'optics' )  then
       !
       l_chi= (trim(rstr_piece(i1+1))=='g' ).or.(trim(rstr_piece(i1+1))=='c')
       l_bse= (trim(rstr_piece(i1+1))=='eh').or.(trim(rstr_piece(i1+1))=='b')
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
       ! if the optics option is not present define defaults
       ! since kernel does not correspond to a runlevel, set 
       ! infile_editing=.true.           
       !
       if (.not.l_bse.and..not.l_chi) then
         l_chi =((trim(rstr_piece(i1+1)) == 'hartree').or.&
                &( trim(rstr_piece(i1+1)) == 'lrc'))
         l_bse =((trim(rstr_piece(i1+1)) == 'alda').or.&
                &(trim(rstr_piece(i1+1)) == 'sex').or.&
                &(trim(rstr_piece(i1+1)) == 'hf').or.&
                &( trim(rstr_piece(i1+1)) == 'bsfxc'))
         infile_editing=.true.           
         call initactivate(1,'optics')
         if (l_chi) call initactivate(1,'chi')
         if (l_bse) call initactivate(1,'bse')
       end if
       !
       BSK_mode='HARTREE'
       Chi_mode='HARTREE'
       !
       if(l_bse)  then
         if(trim(rstr_piece(i1+1))  == 'hartree')  BSK_mode='HARTREE'
         if(trim(rstr_piece(i1+1))  == 'hf')       BSK_mode='HF'
         if( trim(rstr_piece(i1+1)) == 'alda')     BSK_mode='ALDA'
         if(trim(rstr_piece(i1+1))  == 'sex')      BSK_mode='SEX'
         if( trim(rstr_piece(i1+1)) == 'bsfxc')    BSK_mode='BSfxc'
       else if(l_chi) then
         if(trim(rstr_piece(i1+1))  == 'hartree')  Chi_mode='HARTREE'
         if( trim(rstr_piece(i1+1)) == 'alda')     Chi_mode='ALDA'
         if( trim(rstr_piece(i1+1)) == 'lrc')      Chi_mode='LRC'
         if( trim(rstr_piece(i1+1)) == 'bsfxc')    Chi_mode='BSfxc'
       endif
       !
       if((trim(rstr_piece(i1+1)) == 'alda').or.&
&             (trim(rstr_piece(i1+1)) == 'lrc').or.&
&             (trim(rstr_piece(i1+1)) == 'pf').or.&
&             (trim(rstr_piece(i1+1)) == 'bsfxc')) call initactivate(1,'tddft')
       !
       if(trim(BSK_mode)=="SEX" .and. .not.(runlevel_is_on('ppa'))) call initactivate(1,'em1s')
       !
     endif
     !
     ! BSE Solver
     !
     if ( trim(rstr_piece(i1)) == 'bss' )  then
       BSS_mode=trim(rstr_piece(i1+1))
       if (index(BSS_mode,'h')==0.and.index(BSS_mode,'d')==0.and.&
&              index(BSS_mode,'i')==0.and.index(BSS_mode,'t')==0.and.&
&              index(BSS_mode,'s')==0) BSS_mode='h'
       !
       ! With and ALDA Fxc the t solver is not permitted
       !
       if (l_alda_fxc.and.index(BSS_mode,'t')/=0) BSS_mode='h'
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
     if ( trim(rstr_piece(i1)) == 'gw0' ) then
       QP_solver=trim(rstr_piece(i1+1))
       if (trim(QP_solver)/='n'.and.trim(QP_solver)/='s'.and.&
&              trim(QP_solver)/='g') QP_solver='n'
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
#if defined  _ELPH || defined _QED
       if ( (.not.l_elel_corr.and..not.l_elphoton_corr) .or. l_elel_corr) then
         call initactivate(1,'em1d')
         call initactivate(1,'el_el_corr')
       endif
#else
       call initactivate(1,'em1d')
       l_elel_corr=.true.
#endif
     endif
     !
     ! GW approximation 
     !
     if ( trim(rstr_piece(i1)) == 'gwapprx' ) then
       !
       if (trim(rstr_piece(i1+1))=='p') then
         infile_editing=.true.
         call initactivate(1,'ppa')
       else if (trim(rstr_piece(i1+1))=='c') then
         !
         infile_editing=.true.
         call initactivate(1,'cohsex gw0') 
         !
       endif
       !
     endif
     !
     !
#if defined _RT
     !
     ! NEGF
     !
     if ( trim(rstr_piece(i1)) == 'potential' )  then
       !
       H_potential=""
       if ( index(trim(rstr_piece(i1+1)),'cohsex')>0) then
         H_potential='COH+SEX'
         if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
       else if ( index(trim(rstr_piece(i1+1)),'sex')>0 ) then
         H_potential='SEX'
         if (i_cycle==2.and..not.runlevel_is_on('negf')) call initactivate(1,'em1s')
       else if ( index(trim(rstr_piece(i1+1)),'f')>0) then
         H_potential='FOCK'
       else if ( trim(rstr_piece(i1+1)) == 'ip') then
         H_potential='IP'
       endif
       !
       if ( index(trim(rstr_piece(i1+1)),'h')==1 ) then
         if(trim(H_potential)/="") H_potential='HARTREE+'//trim(H_potential)
         if(trim(H_potential)=="") H_potential='HARTREE'
       endif
       !
       if ( trim(rstr_piece(i1+1)) == 'd') then
         H_potential='DEFAULT'
       endif
       !
       if ( trim(rstr_piece(i1+1)) == 'ip') then
         H_potential='IP'
       endif
       !
     endif
#endif
     !
     !
#if defined  _ELPH || defined _QED
     !
     ! ELPH/QED
     !
     if ( trim(rstr_piece(i1)) == 'corrtp' ) then
       if ( index(rstr_piece(i1+1),'e') >0 ) l_elel_corr=.true.
       if ( index(rstr_piece(i1+1),'p') >0 ) l_elph_corr=.true.
       if ( index(rstr_piece(i1+1),'h') >0 ) l_elphoton_corr=.true.
       if ( trim(rstr_piece(i1+1)) == 'a') then
         l_elel_corr=.true.
         l_elph_corr=.true.
         l_elphoton_corr=.true.
       endif
       if (.not.l_elph_corr.and..not.l_elel_corr.and..not.l_elphoton_corr) l_elel_corr=.true.
       if (l_elph_corr)     call initactivate(1,'el_ph_corr')
       if (l_elel_corr)     call initactivate(1,'el_el_corr')
       if (l_elphoton_corr) call initactivate(1,'el_photon_corr')
     endif
#endif
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
     endif
     !
     if ( trim(rstr_piece(i1)) == 'scattp' ) then
       if ( index(rstr_piece(i1+1),'e') > 0 .or. trim(rstr_piece(i1+1)) == 'a') l_elel_scatt=.true.
       if ( index(rstr_piece(i1+1),'p') > 0 .or. trim(rstr_piece(i1+1)) == 'a') l_elph_scatt=.true.
#endif
#if defined _RT 
       if ( index(rstr_piece(i1+1),'h') > 0 .or. trim(rstr_piece(i1+1)) == 'a') l_elphoton_scatt=.true.
       if (l_elphoton_scatt) call initactivate(1,'el_photon_scatt')
#endif
#if defined _RT 
       if (l_elph_scatt)     call initactivate(1,'el_ph_scatt')
       if (l_elel_scatt)     call initactivate(1,'el_el_scatt')
       if (.not.l_elph_scatt.and..not.l_elel_scatt.and..not.l_elphoton_scatt) l_elph_scatt=.true.
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
 if (infile_editing) init_=1
 !
end subroutine
