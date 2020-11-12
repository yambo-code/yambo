!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA
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
subroutine excitons_interpolate(k,Xk,en,Xen,q)
 !
 use pars,          ONLY:SP,DP,schlen,pi,cZERO,cONE
 use LIVE_t,        ONLY:live_timing
 use R_lattice,     ONLY:bz_samp,bz_samp_reset,bz_map
 use interpolate,   ONLY:INTERPOLATION_BZ,INTERP_obj,INTERPOLATION_coefficients
 use electrons,     ONLY:levels,n_sp_pol
 use units,         ONLY:HA2EV
 use BS_solvers,    ONLY:BSS_n_eig,BS_mat,BSS_eh_table,BS_H_dim
 use YPPm,          ONLY:BS_E,EXCITONS_user_indexes,EXCITONS_n_user_states,coo_in,coo_out, &
&                        BANDS_steps,INTERP_grid,K_transform,CIRCUIT_k     
 use interpolate,   ONLY:nshells
 use parser_m,      ONLY:parser
 use com,           ONLY:msg,of_open_close
 use stderr,        ONLY:intc
 use vec_operate,   ONLY:iku_v_norm,k_periodic_idx
 use wrapper_omp,   ONLY:M_by_V_omp,Vstar_dot_V_omp
 !
#include<memory.h> 
 !
 type(bz_samp) ::Xk,k,q
 type(levels)  ::Xen,en
 !
 ! Work space
 !
 integer :: iq,i_c,i_l,ID_INTERP,ik,ib,ibp,iqbz,iqbzp,iqp
 integer :: iqm, iqm_ibz, iqp_ibz
 integer :: id,idxp(3),idx(3),max_step,istep
 real(SP), allocatable    :: BS_all_E(:,:),BSE_interp_E(:,:),values(:)
 real(SP)                 :: deltaQ
 complex(SP) ,allocatable :: BS_mat_all(:,:,:),BSS_eh_table_all(:,:,:),EXC_OVLP(:,:,:,:),V_tmp(:,:,:)
 type(bz_samp)            :: GRID_q
 character(schlen)        :: coo_out_,file_name
 character(schlen), allocatable :: headings(:)
 logical                  :: GRID_interpolate
 real(SP)                 :: real_ctl,tmp_q(3)
 type(bz_map)             :: q_map
 logical                  :: l_interp_dip,l_interp_epc,l_interp_wfc
 !
 ! Parse the different interpolations
 !
 call parser('InterpDIP',l_interp_dip)
 call parser('InterpEPC',l_interp_epc)
 call parser('InterpWFC',l_interp_wfc)
 !
 if(l_interp_epc) l_interp_wfc=.true.
 !
 ! Allocate arrays for all eigenvalues/eigenvectors
 ! 
 YAMBO_ALLOC(BS_all_E,(EXCITONS_n_user_states,q%nibz))
 YAMBO_ALLOC(values,(EXCITONS_n_user_states+4))
 allocate(headings(EXCITONS_n_user_states+4))
 !
 !
 call section('*','Excitons Interpolation')
 !
 call excitons_read(k,Xk,en,Xen,1,"clean")
 !
 ! Read all eigenvalues and eigenvectors
 !
 do iq=1,q%nibz
   call msg('s','Reading excitons at @ Q-index #',iq)
   call excitons_read(k,Xk,en,Xen,iq,"check")
   call excitons_read(k,Xk,en,Xen,iq,"eigenvalues")
   do i_c=1,EXCITONS_n_user_states
     i_l=EXCITONS_user_indexes(i_c)
     BS_all_E(i_c,iq)=BS_E(i_l)
   enddo
   call excitons_read(k,Xk,en,Xen,iq,"clean")
   !
 enddo
 !
 ! Write in the report the exciton energies
 !
 call msg('nr','Excitons Energies:')
 headings(1:3)=(/"q_x (rlu)","q_y (rlu)","q_z (rlu)"/)
 do i_c=1,EXCITONS_n_user_states
    i_l=EXCITONS_user_indexes(i_c)
    headings(i_c+3)=' e'//trim(intc(i_l))//' [eV]'
 enddo
 call msg('r',"#",headings(1:EXCITONS_n_user_states+3))
 do iq=1,q%nibz
   tmp_q=q%pt(iq,:3)
   call K_transform(tmp_q,'iku')
   values(1:3)=tmp_q
   do i_c=1,EXCITONS_n_user_states
      i_l=EXCITONS_user_indexes(i_c)
      values(i_c+3)=BS_all_E(i_c,iq)*HA2EV
   enddo
   call msg('r',' ',values(1:EXCITONS_n_user_states+3))
 enddo
 !
 ! Fourier interpolation of exciton dispersion
 !
 call INTERPOLATION_BZ_setup(q)
 ID_INTERP=1
 INTERP_obj(ID_INTERP)%what="excitons"
 call INTERPOLATION_coefficients(R1D=BS_all_E,k=q,NK=q%nibz,ID=ID_INTERP,ID_obj=ID_INTERP)
 !
 ! Excitons Interpolation on GRID or CIRCUIT
 !
 if(BANDS_steps>0) call k_circuit_made_of_user_points( )
 !
 GRID_interpolate    = all(INTERP_grid>0)
 !
 ! Interpolate and write on disk
 !
 if(BANDS_steps>0)  then
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,CIRCUIT_k%nibz))      
   call INTERPOLATION_BZ(K=CIRCUIT_k,NK=CIRCUIT_k%nbz,R1D=BSE_interp_E,ID=ID_INTERP)
   !
   headings(1)=" |q| (a.u.) "
   do i_c=1,EXCITONS_n_user_states
      i_l=EXCITONS_user_indexes(i_c)
      headings(i_c+1)=' e'//intc(i_l)
   enddo
   headings(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=&
&           (/"q_x ("//trim(coo_out)//")","q_y ("//trim(coo_out)//")","q_z ("//trim(coo_out)//")"/)
   !
   file_name="excitons_interpolated"
   call of_open_close(trim(file_name),'ot')
   call msg('o '//trim(file_name),"#",headings,INDENT=0,USE_TABS=.true.)
   call msg('o '//trim(file_name),"#")
   !
   values=0._SP
   !
   do iq=1,CIRCUIT_k%nibz
     !
     if (iq>1) values(1)=values(1)+iku_v_norm(CIRCUIT_k%pt(iq,:3)-CIRCUIT_k%pt(iq-1,:3))
     !
     tmp_q=CIRCUIT_k%pt(iq,:3)
     !
     call K_transform(tmp_q,'iku')
     values(1+1:EXCITONS_n_user_states+1)=BSE_interp_E(:,iq)*HA2EV
     values(EXCITONS_n_user_states+2:EXCITONS_n_user_states+4)=tmp_q
     call msg('o '//trim(file_name),' ',values,INDENT=0,USE_TABS=.true.)
   enddo
   call of_open_close(trim(file_name))
 endif
 !
 YAMBO_FREE(values)
 deallocate(headings)
 !  
 ! Interpolate and write on database
 !
 if(GRID_interpolate) then
   call INTERPOLATION_create_the_grid(GRID_q,'i',INTERP_grid)
   YAMBO_ALLOC(BSE_interp_E,(EXCITONS_n_user_states,GRID_q%nibz))      
   call INTERPOLATION_BZ(K=GRID_q,NK=GRID_q%nbz,R1D=BSE_interp_E,ID=ID_INTERP)
   !
 endif
 !
end subroutine
