!
!        Copyright (C) 2000-2023 the YAMBO team
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
integer function io_PH(ID)
 !
 use units,               ONLY:HA2EV
 use pars,                ONLY:SP,schlen,CR_YIO
 use R_lattice,           ONLY:nqbz
 use QP_m,                ONLY:QP_G_damp,l_extended_output,QP_solver,On_Mass_Shell_approx
 use X_m,                 ONLY:X_DbGd_percentual
 use PHEL,                ONLY:PH_DbGd_points,l_PH_uses_E_FineGd,PH_Self_Energy_mode,PHEL_qp,&
&                              PHEL_alloc,QP_Sc_steps,PH_Q,PHEL_broad_factor
 use ELPH,                ONLY:QP_PH_n_G_bands,elph_branches,PH_freqs_sq,ph_modes,PHEL_desc
 use IO_int,              ONLY:io_connect,io_disconnect,io_header,def_variable_elemental,&
&                              io_variable_elemental,def_variable_bulk,io_variable_bulk
 use IO_m,                ONLY:io_sec,io_status,read_is_on,DB_is_OK
 use RT_lifetimes,        ONLY:RT_PE_life,RT_lifetime_alloc,Life_MEM_steps
 use descriptors,         ONLY:IO_desc_reset
 !
#include<memory.h>
 !
 integer           ::ID
 character(schlen) ::QP_solver_disk
 !
 io_PH=io_connect(desc="ph",type=2,ID=ID)
 !
 if (io_PH/=0) then
   call io_disconnect(ID=ID)
   return
 endif
 !
 io_PH=io_header(ID,TEMP=.TRUE.,D_LATT=.TRUE.,R_LATT=.TRUE.,QPTS=.TRUE.,XC_KIND="G")
 !
 if (any((/io_sec(ID,:)==1/))) then
   call IO_desc_reset(PHEL_desc)
   call def_variable_elemental(ID,"CH_PHEL_PARAMETERS",1,CR_YIO,0)
   call io_variable_elemental(ID,VAR="[PHEL] Self-Energy mode",CH0=PH_Self_Energy_mode,DESCRIPTOR=PHEL_desc,&
&                                CHECK=.TRUE.,OP=(/"=="/))
   call def_variable_elemental(ID,"CH_PHEL_SOLVER",1,CR_YIO,0)
   call io_variable_elemental(ID,VAR="[PHEL] Solver",CH0=QP_solver,DB_CH0=QP_solver_disk,DESCRIPTOR=PHEL_desc,&
&                                CHECK=.TRUE.,OP=(/"=="/))
   call def_variable_elemental(ID,"PHEL_PARAMETERS",13,SP,0)
   if (trim(QP_solver_disk)=="n") then
     call io_variable_elemental(ID,VAR="[PH  ] On-the-Mass-Shell approximation",L0=On_Mass_Shell_approx,&
&                               TERMINATOR="[all]",CHECK=.TRUE.,OP=(/"=="/),DESCRIPTOR=PHEL_desc)
   endif
   if (trim(QP_solver_disk)=="n".or.trim(QP_solver_disk)=="g") then
     call io_variable_elemental(ID,VAR="[PHEL] \Pi Energy points",I0=QP_Sc_steps,DESCRIPTOR=PHEL_desc,CHECK=.TRUE.,OP=(/"=="/))
   endif
   if (len_trim(QP_solver_disk)==0) then
     call io_variable_elemental(ID,VAR="[PHEL] Transitions energy window",R0=PHEL_broad_factor,&
&                               DESCRIPTOR=PHEL_desc,CHECK=.TRUE.,OP=(/"=="/))
   endif
   call io_variable_elemental(ID,VAR="[PH  ] Branches",I0=ph_modes,TERMINATOR="[all]",&
&                                CHECK=.TRUE.,OP=(/"=="/),DESCRIPTOR=PHEL_desc)
   call io_variable_elemental(ID,VAR="[PHEL]",I1=elph_branches,DESCRIPTOR=PHEL_desc,&
&                                TERMINATOR="[used]",CHECK=.TRUE.,OP=(/"==","=="/))
   call io_variable_elemental(ID,VAR="[PHEL] Q-points",I1=PH_Q,DESCRIPTOR=PHEL_desc,&
&                                TERMINATOR="[used]",CHECK=.TRUE.,OP=(/"==","=="/))
   call io_variable_elemental(ID,VAR="[PHEL] G Damping",R0=QP_G_damp,DESCRIPTOR=PHEL_desc,&
&                                TERMINATOR="eV",UNIT=HA2EV,CHECK=.TRUE.,OP=(/"=="/))
   call io_variable_elemental(ID,VAR="[PHEL] X Bands",I1=QP_PH_n_G_bands,DESCRIPTOR=PHEL_desc,&
&                                TERMINATOR="[used]",CHECK=.TRUE.,OP=(/"==","=="/))
   call io_variable_elemental(ID,VAR="[PHEL] X DbGd points",I0=PH_DbGd_points,DESCRIPTOR=PHEL_desc,CHECK=.TRUE.,OP=(/"=="/))
   call io_variable_elemental(ID,VAR="[PHEL] X DbGd support",L0=l_PH_uses_E_FineGd,DESCRIPTOR=PHEL_desc,CHECK=.TRUE.,OP=(/"=="/))
   call io_variable_elemental(ID,VAR="[PHEL] X DbGd percentual",R0=X_DbGd_percentual,&
&                             DESCRIPTOR=PHEL_desc,CHECK=.TRUE.,OP=(/"=="/),TERMINATOR="[o/o]")
   call def_variable_elemental(ID,"",0,0,1)
   io_PH=io_status(ID)
   if (.not.DB_is_OK(ID)) then
     call io_disconnect(ID=ID)
     return
   endif
 endif
 !
 if (any((/io_sec(ID,:)==2/))) then
   !
   if (read_is_on(ID)) then
     Life_MEM_steps=1
     call RT_lifetime_alloc("ph-el",RT_PE_life,elph_branches,PH_Q(2))
     call PHEL_alloc( )
     if (.not.allocated(PH_freqs_sq)) then
       YAMBO_ALLOC(PH_freqs_sq,(nqbz,ph_modes))
     endif
   endif
   !
   call def_variable_bulk(ID,"PH_FREQ_SQ",1,(/nqbz,ph_modes/),SP)
   call io_variable_bulk(ID,1,R2=PH_freqs_sq)
   !
   if (trim(QP_solver)=="n") then
     call def_variable_bulk(ID,"PH_LIFE_EMIT",2,(/elph_branches(2)-elph_branches(1)+1,PH_Q(2)/),SP)
     call io_variable_bulk(ID,2,R2=RT_PE_life%emit(:,:))
     !
     call def_variable_bulk(ID,"PH_LIFE_ABS",3,(/elph_branches(2)-elph_branches(1)+1,PH_Q(2)/),SP)
     call io_variable_bulk(ID,3,R2=RT_PE_life%abs(:,:))
     !
   endif
   !
   call def_variable_bulk(ID,"PH_PI",4,(/2,elph_branches(2)-elph_branches(1)+1,PH_Q(2)-PH_Q(1)+1,QP_Sc_steps/),SP)
   call io_variable_bulk(ID,4,C3=PHEL_qp%PI(:,:,:))
   !
   if (l_extended_output.and.trim(QP_solver)=="n") then
     !
     call def_variable_bulk(ID,"PH_LIFE_INTERBAND",5,(/elph_branches(2)-elph_branches(1)+1,PH_Q(2)-PH_Q(1)+1/),SP)
     call io_variable_bulk(ID,5,R2=PHEL_qp%interband(:,:))
     !
     call def_variable_bulk(ID,"PH_LIFE_INTRABAND",6,(/elph_branches(2)-elph_branches(1)+1,PH_Q(2)-PH_Q(1)+1/),SP)
     call io_variable_bulk(ID,6,R2=PHEL_qp%intraband(:,:))
     !
     call def_variable_bulk(ID,"PH_LIFE_EQ_ALLEN",7,(/elph_branches(2)-elph_branches(1)+1,PH_Q(2)-PH_Q(1)+1/),SP)
     call io_variable_bulk(ID,7,R2=PHEL_qp%allen(:,:))    
     !
   endif
   !
 endif
 !
 call io_disconnect(ID=ID)
 !
end function
