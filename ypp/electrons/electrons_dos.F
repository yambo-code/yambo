! 
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
subroutine electrons_dos(Xk,Xen,mode)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,schlen,rZERO
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use electrons,      ONLY:levels,spin,n_spinor,n_spin,BZ_DbGd_nbands,n_sp_pol
 use YPP,            ONLY:output_fname,DOS_broadening,DOS_bands,&
&                         DOS_E_range,DOS_E_steps,INTERP_grid,INTERP_k
 use com,            ONLY:msg,of_open_close
 use QP_CTL_m,       ONLY:QP_apply
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:PARALLEL_index,el_density_of_states
 use parallel_m,     ONLY:PP_indexes,myid,PP_redux_wait,PP_indexes_reset
 use interpolate,    ONLY:INTERPOLATION_coefficients,INTERPOLATION_BZ,&
&                         INTERP_E_id
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 character(*)  ::mode
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(DOS_E_steps,n_spin,2),dos_E,delta_E
 real(SP),allocatable  :: WF_fac(:,:,:)
 integer               :: i_E,i_f,N_files
 character(schlen)     :: titles(4)
 logical               :: l_DbGd
 type(PP_indexes)      :: px
 type(levels)          :: INTERP_E
 !
 if (mode=="NORMAL") call section('*','Electronic DOS')
 if (mode=="INTERP") call section('=','Electronic DOS')
 !
 ! E Double Grid
 !===============
 !
 l_DbGD=BZ_DbGd_nbands>0
 !
 if (n_spinor==2.and.mode=="NORMAL") then
   YAMBO_ALLOC(WF_fac,(DOS_bands(1):DOS_bands(2),Xk%nibz,2))
   call electrons_spin_factors((/1,Xk%nibz/),DOS_bands,WF_fac)
 endif
 !
 if (l_DbGd) DOS_bands(2)=min(DOS_bands(2),BZ_DbGd_nbands)
 !
 if (mode=="NORMAL") then
   call QP_state_table_setup(Xen)
   call QP_apply(DOS_bands,Xen,Xk,'G',msg_fmt='s')
 endif
 !
 if (DOS_E_range(1)>DOS_E_range(2)) then
   DOS_E_range(1)=minval(Xen%E(DOS_bands(1):DOS_bands(2),:,:))-5._SP*DOS_broadening
   DOS_E_range(2)=maxval(Xen%E(DOS_bands(1):DOS_bands(2),:,:))+5._SP*DOS_broadening
 endif
 delta_E=(DOS_E_range(2)-DOS_E_range(1))/DOS_E_steps
 !
 ! Define the regural grid for interpolation if (all(interp_grid>0))
 !===================================================================
 N_files=1
 !
 if(all(INTERP_grid>0)) then
   !
   N_files=2
   !
   call bz_samp_reset(INTERP_k)
   INTERP_k%description="Interp"
   call INTERPOLATION_grid(INTERP_k,'i',INTERP_grid)
   !
   !... Brillouin zone setup
   call INTERPOLATION_BZ_setup(Xk)
   !
   !... Interpolation coeff
   call INTERPOLATION_coefficients(R2D=Xen%E,k=Xk,Nk=Xk%nibz,ID=INTERP_E_id)
   !
   !... Interpolation
   YAMBO_ALLOC(INTERP_E%E,(DOS_bands(1):DOS_bands(2),INTERP_k%nibz,n_sp_pol))
   call INTERPOLATION_BZ(K=INTERP_k,R2D=INTERP_E%E,ID=INTERP_E_id)
   !
 endif
 !
 ! Parallel setup and live_timing
 !================================
 !
 call PP_indexes_reset(px)
 call PARALLEL_index(px,(/DOS_E_steps/))
 call live_timing('DOS',px%n_of_elements(myid+1))
 !
 el_dos=rZERO
 !
 do i_E=1,DOS_E_steps
   !
   if (.not.px%element_1D(i_E)) cycle
   !
   dos_E=DOS_E_range(1)+i_E*delta_E
   !
   if(     allocated(WF_fac)) then
     call el_density_of_states(Xk,Xen,dos_E,DOS_broadening,DOS_bands,el_dos(i_E,:,1),USE_the_DbGd=l_DbGd,WF_fac=WF_fac)
     if (all(INTERP_grid>0)) then
       call el_density_of_states(INTERP_k,INTERP_E,dos_E,DOS_broadening,DOS_bands,el_dos(i_E,:,2),WF_fac=WF_fac)
     endif
   else
     call el_density_of_states(Xk,Xen,dos_E,DOS_broadening,DOS_bands,el_dos(i_E,:,1),USE_the_DbGd=l_DbGd)
     if (all(INTERP_grid>0)) then
       call el_density_of_states(INTERP_k,INTERP_E,dos_E,DOS_broadening,DOS_bands,el_dos(i_E,:,2))
     endif
   endif
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 call PP_redux_wait(el_dos)
 !
 YAMBO_FREE(WF_fac)
 !
 ! Output file(s)
 !================
 !
 ! ...headers
 !
 do i_f=1,N_files 
   !
   if (i_f==1) output_fname='el_dos'
   if (i_f==2) output_fname='el_dos_INTERPOLATED'
   call of_open_close(trim(output_fname),'ot')
   if (n_spin==1) then
     titles(1)='E[eV]'
     titles(2)='DOS'
     call msg('o dos','#',titles(:2),INDENT=0,USE_TABS=.true.)    
   else
     titles(1)='E[eV]'
     titles(2)='DOS [up]'
     titles(3)='DOS [dn]'
     titles(4)='DOS [up+dn]'
     call msg('o dos','#',titles(:4),INDENT=0,USE_TABS=.true.)    
   endif
   call msg('o dos',"#")
   !
   ! ...data
   !
   do i_E=1,DOS_E_steps
     !
     dos_E=DOS_E_range(1)+i_E*delta_E
     !
     if (n_spin==1) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,1,i_f)/),INDENT=-2,USE_TABS=.true.)
     if (n_spin==2) call msg('o dos','',(/dos_E*HA2EV,el_dos(i_E,:,i_f),  &
&                                         el_dos(i_E,1,i_f)+el_dos(i_E,2,i_f)/),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
 enddo
 !
end subroutine
