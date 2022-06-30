!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AC
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
!> @brief Calculate phonon-assisted absorption and emission (see PRL 122, 187401 )
!
subroutine EXCPH_double_grid_setup(q)
 !       
 use pars,           ONLY:schlen,SP
 use vec_operate,    ONLY:c2a
 use units,          ONLY:HA2EV
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use IO_int,         ONLY:io_control
 use IO_m,           ONLY:manage_action,OP_RD,REP,RD_CL_IF_END,DUMP
 use ELPH,           ONLY:PH_freqs,use_PH_DbGd,ph_modes,E_kpq_sh_fact
 use interpolate,    ONLY:INTERPOLATE_is_serial,INTERPOLATION_coefficients,INTERP_obj,INTERP_shell_factor,&
&                        INTERPOLATE_is_quiet,INTERPOLATION_BZ
 use EXCPH,          ONLY:min_EXC_E_DbGd,ID_EXC,ID_EXC_obj,EXC_E_DbGd,EXCPH_sum,BS_all_E
 use com,            ONLY:msg
 !
#include<memory.h>
 !
 type(bz_samp), intent(in) :: q
 !
 ! Work space
 !
 type(bz_samp) :: q_DbGd
 integer  :: io_err,ID_DG
 integer  :: min_pos_E_DbGd(1)
 real(SP) :: v(3)
 character(schlen) :: q_point
 integer, external   :: io_Double_Grid
 !
 ! Fill the PH_freqs array
 ! This structure will contains the phonon frequencies and the
 ! double-grid phonon frequencies calculated with matdyn.x
 !
 PH_freqs%nk=q%nibz
 PH_freqs%nb=ph_modes
 YAMBO_ALLOC(PH_freqs%E,(q%nibz,ph_modes,1))
 !
 ! Check if double-grid is present and use it
 ! load the double-grid phonon energies in PH_freqs
 !
 call io_control(ACTION=OP_RD,COM=REP,MODE=DUMP,SEC=(/1,2/),ID=ID_DG)
 io_err=io_Double_Grid(PH_freqs,q,ID_DG,'phonons')
 !
 if(io_err/=0) return
 !     
 use_PH_DbGd=.TRUE.
 !
 call msg('sr','Using PH-double-grid ')
 call msg('rs','Double grid number of q-points ',q%FGbz%N)
 !
 call msg('sr','EXC_E(k+q) calculated using a smooth Fourier interpolation')
 INTERPOLATE_is_serial=.TRUE.
 INTERPOLATE_is_quiet =.TRUE.
 INTERP_shell_factor  =E_kpq_sh_fact
 ID_EXC_obj=1
 ID_EXC    =1
 call INTERPOLATION_BZ_setup(q)
 INTERP_obj(ID_EXC_obj)%what="EXC_E(k+q)"
 call INTERPOLATION_coefficients(R1D=BS_all_E(:EXCPH_sum(2),:),k=q,NK=q%nibz,ID=ID_EXC,ID_obj=ID_EXC_obj)
 !
 call bz_samp_reset(q_DbGd)
 q_DbGd%nibz=q%FGbz%N
 YAMBO_ALLOC(q_DbGd%pt,(q_DbGd%nibz,3))
 q_DbGd%pt=q%FGbz%pt
 !
 YAMBO_ALLOC(EXC_E_DbGd,(EXCPH_sum(2),q_DbGd%nibz))
 call INTERPOLATION_BZ(K=q_DbGd,NK=q_DbGd%nibz,ID=ID_EXC,R1D=EXC_E_DbGd)
 !
 min_EXC_E_DbGd=minval(EXC_E_DbGd(1,:))
 min_pos_E_DbGd=minloc(EXC_E_DbGd(1,:))
 call c2a(v_in=q_DbGd%pt(min_pos_E_DbGd,:),v_out=v,mode="ki2a")
 call msg('rs','Minimum excitonic energy with DbGd: ',min_EXC_E_DbGd*HA2EV,'eV')
 write(q_point,'(3f12.6,a)') v(:),' [rlu] '
 call msg('rs','Minimum q-point in the DbGrid : '//q_point)
 !
end subroutine
