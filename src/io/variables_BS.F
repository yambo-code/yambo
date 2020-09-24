!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function variables_BS(ID,iq,desc,CLOSE_the_menu,X)
 !
 use pars,          ONLY:SP,IP,LP
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_td_hf
 use X_m,           ONLY:X_t
 use R_lattice,     ONLY:RIM_ng,RIM_n_rand_pts,cut_geometry
 use BS,            ONLY:BS_identifier,BS_K_dim,BS_bands,&
&                        BS_K_coupling,BS_res_K_exchange,BS_res_K_corr,&
&                        BS_n_g_exch,BS_n_g_W,BS_eh_en,BS_eh_win,BS_W_is_diagonal,&
&                        BS_K_is_ALDA,BS_cpl_K_exchange,&
&                        BS_cpl_K_corr,BSqpts,BS_perturbative_SOC
 use IO_m,          ONLY:io_status,ver_is_gt_or_eq,variable_exist,write_is_on,&
&                        def_variable_elemental,io_variable_elemental
 use descriptors,   ONLY:IO_desc,IO_desc_reset
#if defined _PAR_IO
 use BS,            ONLY:BSK_IO_mode
#endif
 !
 implicit none
 !
 integer            ::ID,iq
 type(IO_desc)      ::desc
 logical            ::CLOSE_the_menu
 type(X_t), optional::X
 !
 ! Work Space
 !
 integer             ::var_size
 logical             ::res_corr_disk,res_exch_disk,cpl_disk,&
&                      cpl_corr_disk,cpl_exch_disk,td_hf_disk,local_long_gauge
 !
 ! PARS1
 !
 var_size=29
 if (ver_is_gt_or_eq(ID,(/3,0,9/)))      var_size=30
 if (ver_is_gt_or_eq(ID,revision=16339)) var_size=33
 if (iq>1)                               var_size=35
 !
 call IO_desc_reset(desc)
 !
 call def_variable_elemental(ID,"PARS1",var_size,SP,0)
 call io_variable_elemental(ID,&
&     VAR="[K] Identifier               ",I0=BS_identifier,DESCRIPTOR=desc)
 if(iq>1) then
   call io_variable_elemental(ID,VAR="[K] Q-point",&
&                             R1=BSqpts(:,iq),CHECK=.true.,OP=(/"==","==","=="/),DESCRIPTOR=desc)
 endif
 call io_variable_elemental(ID,VAR="[K] Dimension",I0=BS_K_dim,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 call io_variable_elemental(ID,VAR="[K] Bands",I1=BS_bands,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=desc)
 call io_variable_elemental(ID,DB_L0=res_exch_disk,VAR="[K] Exchange",&
&                           TERMINATOR="res",L0=BS_res_K_exchange,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)

 call io_variable_elemental(ID,DB_L0=res_corr_disk,&
&     VAR="[K] Correlation",TERMINATOR="res",L0=BS_res_K_corr,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 call io_variable_elemental(ID,DB_L0=cpl_disk,&
&     VAR="[K] Kernel`s coupling        ",L0=BS_K_coupling,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 call io_variable_elemental(ID,DB_L0=cpl_exch_disk,&
&     VAR="[K] Exchange",TERMINATOR="cpl",L0=BS_cpl_K_exchange,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 !
 if (ver_is_gt_or_eq(ID,revision=4888)) then
   call io_variable_elemental(ID,DB_L0=cpl_corr_disk,&
&       VAR="[K] Correlation",TERMINATOR="cpl",L0=BS_cpl_K_corr,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,DB_L0=td_hf_disk,&
&       VAR="[K] W interaction is bare    ",L0=l_td_hf,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 else
   call io_variable_elemental(ID,DB_L0=td_hf_disk,&
&       VAR="[K] W interaction is bare    ",L0=l_td_hf,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,DB_L0=cpl_corr_disk,&
&       VAR="[K] Correlation",TERMINATOR="cpl",L0=BS_cpl_K_corr,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 endif
 !
 call io_variable_elemental(ID,&
&     VAR="[K] ALDA kernel in R-space   ",L0=BS_K_is_ALDA,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 !
 if (res_exch_disk.or.cpl_exch_disk) then
     call io_variable_elemental(ID,&
&      VAR="[K] RL vectors",TERMINATOR="exchange",I0=BS_n_g_exch,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 endif
 if (res_corr_disk.or.cpl_corr_disk) then
     call io_variable_elemental(ID,&
&     VAR="[K] RL vectors",TERMINATOR="correlation",I0=BS_n_g_W,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
 endif
 call io_variable_elemental(ID,UNIT=HA2EV,&
&     VAR="[K] E/h energy range",TERMINATOR="ev",R1=BS_eh_en,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=desc)
 call io_variable_elemental(ID,&
&     VAR="[K] Coupling range",TERMINATOR="o/o",R1=BS_eh_win,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=desc)
 !
 ! Interaction
 !
 if (res_corr_disk.or.cpl_corr_disk) then
     call io_variable_elemental(ID,&
&       VAR="[W] Interaction is diagonal  ",L0=BS_W_is_diagonal,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,&
&       VAR="[W] Matrix size              ",I0=X%ng,WARN=.true.,OP=(/"=="/),DESCRIPTOR=desc)
   !
   ! Only if full screened interaction is used
   !
   if (.not.td_hf_disk) then
     !
     call io_variable_elemental(ID,&
&         VAR="[W] Bands                    ",I1=X%ib,CHECK=.true.,OP=(/"==","=="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,UNIT=HA2EV,&
&         VAR="[W] e/h energy range",TERMINATOR="ev",R1=X%ehe,CHECK=.true.,OP=(/">=","<="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,&
&         VAR="[W] Poles",TERMINATOR="o/o",R0=X%cg_percentual,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,&
&         VAR="[W] RL vectors in the sum    ",I0=X%ngostnts,WARN=.true.,OP=(/"<="/),DESCRIPTOR=desc)
     call io_variable_elemental(ID,&
&         VAR="[W] [r,Vnl] included         ",L0=X%Vnl_included,WARN=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     !
     if (ver_is_gt_or_eq(ID,(/3,0,9/)).and..not.ver_is_gt_or_eq(ID,revision=626)) then
       !
       ! v 3.0.8: Inclusion of %long_gauge 
       !
       call io_variable_elemental(ID,&
&           VAR="[W] Longitudinal Gauge       ",L0=local_long_gauge,DESCRIPTOR=desc)
       !
     endif
     !
     call io_variable_elemental(ID,&
&         VAR="[W] Field direction          ",R1=X%q0,CHECK=.true.,OP=(/"==","==","=="/),DESCRIPTOR=desc)
     call def_variable_elemental(ID,"",0,0,0)
     !
     if (ver_is_gt_or_eq(ID,(/3,0,11/))) then
       !
       ! v 3.0.11: Inclusion of cut_geometry
       !
       call def_variable_elemental(ID,"W_Cutoff",1,0,0)
       call io_variable_elemental(ID,&
&           VAR="[W] Coulomb Cutoff           ",CH0=cut_geometry,CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc)
     endif
     !
     ! W_TDDFT_KERNEL
     !
     call def_variable_elemental(ID,"W_TDDFT_KERNEL",1,0,0)
     call io_variable_elemental(ID,&
&       VAR='[W] xc-Kernel               ',CH0=X%f_xc,CHECK=.true.,&
&       DESCRIPTOR=desc)
     !
   endif
   !
   ! W_RIM
   !
   ! Here the name W_RIM is correct. 
   ! However the ndb.BS_PAR_Q1 used for restart in the test-suite should
   ! be updated, otherwise the hBN/GW-OPTICS/ corresponding test fails
   !call def_variable_elemental(ID,"W_RIM",2,IP+100,0) 
   call def_variable_elemental(ID,"W_DbGd",2,IP+100,0)
     call io_variable_elemental(ID,I0=RIM_ng,&
&     VAR='[RIM] RL components',TERMINATOR='col',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc,DO_NOT_DUMP=.TRUE.)
     call io_variable_elemental(ID,I0=RIM_n_rand_pts,&
&     VAR='[RIM] Random points',TERMINATOR='col',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc,DO_NOT_DUMP=.TRUE.)
   !
   !
   ! Perturbative SOC
   !
   if (write_is_on(ID).or.variable_exist(ID,"PERT_SOC")) then
     call def_variable_elemental(ID,"PERT_SOC",1,LP+100,MENU=0)
     call io_variable_elemental(ID,L0=BS_perturbative_SOC,&
&       VAR='[SOC] Perturbative SOC',CHECK=.true.,OP=(/"=="/),DESCRIPTOR=desc,DO_NOT_DUMP=.TRUE.)
   endif
   !
 endif
 !
#if defined _PAR_IO
 !
 ! Check if BSE kernel is written in triangular form
 !
 if (write_is_on(ID).or.variable_exist(ID,"BSE_KERNEL_IO_MODE")) then
   call def_variable_elemental(ID,"BSE_KERNEL_IO_MODE",1,0,0)
   call  io_variable_elemental(ID,VAR="[IO] Kernel IO mode",CH0=BSK_IO_mode)
 else
   BSK_IO_mode="2D_standard"
 endif
 !
#endif
 !
 if (     CLOSE_the_menu) call def_variable_elemental(ID,"",0,0,1)
 if (.not.CLOSE_the_menu) call def_variable_elemental(ID,"",0,0,0)
 !
 variables_BS=io_status(ID)
 !
end function
!
!
#if defined _PAR_IO
!
integer function variables_BS_more(ID,mode)
 !
 use pars,       ONLY:SP,LP
 use parallel_m, ONLY:master_cpu
 use BS,         ONLY:BS_K_cutoff,BS_max_val,l_BSE_kernel_complete
 use IO_m,       ONLY:def_variable_elemental,io_variable_elemental,&
&                     write_is_on,variable_exist,io_BS_K
 !
 implicit none
 !
 integer,      intent(in) :: ID
 character(*), intent(in) :: mode
 !
 logical :: def_var,io_var
 !
 if (write_is_on(ID).or.variable_exist(ID,"BSE_KERNEL_CUTOFF")) then
   call def_variable_elemental(ID,"BSE_KERNEL_CUTOFF",1,SP,0)
   call  io_variable_elemental(ID,VAR=" Kernel cutoff",R0=BS_K_cutoff)
 endif
 !
 if (write_is_on(ID).or.variable_exist(ID,"BSE_KERNEL_MAXVAL")) then
   call def_variable_elemental(ID,"BSE_KERNEL_MAXVAL",1,SP,0)
   call  io_variable_elemental(ID,VAR=" Kernel maxval",R0=BS_max_val)
 endif
 !
 ! Check if BSE kernel is complete in reading mode, set the variable to false in writing mode
 !
 if( (trim(mode)=="start".or.(trim(mode)=="end".and.l_BSE_kernel_complete.and.write_is_on(ID)) ) .and. io_BS_K ) then
   def_var=                write_is_on(ID) .or.variable_exist(ID,"BSE_KERNEL_COMPLETE")
   io_var =(master_cpu.and.write_is_on(id)).or.variable_exist(ID,"BSE_KERNEL_COMPLETE")
   if(def_var) call def_variable_elemental(ID,"BSE_KERNEL_COMPLETE",1,LP+100,0,par_io_kind="independent")
   if(io_var)  call io_variable_elemental(ID,VAR=" Kernel complete",&
               &                          DB_L0=l_BSE_kernel_complete,L0=l_BSE_kernel_complete)
 endif
 !
end function
!
#endif
