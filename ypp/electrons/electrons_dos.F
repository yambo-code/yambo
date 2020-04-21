! 
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine electrons_dos(Xk,Xen)
 !
 use units,          ONLY:HA2EV
 use pars,           ONLY:SP,schlen,rZERO
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use pointers,       ONLY:linkM2M
 use electrons,      ONLY:levels,spin,n_spinor,n_spin,BZ_DbGd_nbands,n_sp_pol,E_reset
 use LIVE_t,         ONLY:live_timing
 use YPP_interfaces, ONLY:electrons_dos_elemental,PROJECTION_k_on_the_path_or_plane,PROJECTION_plot,PROJECTION_setup
 use YPPm,           ONLY:output_fname,DOS_bands,DOS_E_steps,K_transform,&
&                         INTERP_grid,l_PROJECT_atom,l_PROJECT_line,l_PROJECT_plane,&
&                         PROJECT_depth,SLICE_E,SLICE_k
 use com,            ONLY:msg,of_open_close
 use QP_CTL_m,       ONLY:QP_apply
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,GRID_k,INTERP_obj
 use stderr,         ONLY:intc
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 ! 
 real(SP)              :: dos_E(DOS_E_steps),values(DOS_bands(2)+4),dos_integral(2)
 real(SP), external    :: RIntegrate
 real(SP),allocatable,target  :: SPIN_fac_v(:,:,:)
 real(SP),allocatable,target  :: PDOS_fac_v(:,:,:)
 real(SP),pointer, contiguous :: SPIN_fac_p(:,:,:) => null()
 real(SP),pointer, contiguous :: PDOS_fac_p(:,:,:) => null()
 real(SP),        allocatable :: el_dos(:,:,:)
 !
 integer               :: i_E,i_f,N_files,is,ib,ik,ikbz,IDe,IDf,IDs,IDp,ik_path,n_t
 character(schlen)     :: titles(DOS_bands(2)+4)
 logical               :: l_DbGd
 type(levels)          :: GRID_E
 !
 call section('*','Electronic DOS')
 !
 ! E Double Grid
 !===============
 !
 l_DbGD=BZ_DbGd_nbands>0
 !
 if (l_DbGd) DOS_bands(2)=min(DOS_bands(2),BZ_DbGd_nbands)
 !
 call QP_state_table_setup(Xen)
 call QP_apply(DOS_bands,Xen,Xk,'G',msg_fmt='rs')
 !
 ! Setup
 !=======
 call electrons_dos_setup( Xen, dos_E, DOS_bands )  
 !
 if (l_PROJECT_plane) then
   call warning("Plane Projection not possible in DOS mode. Switched off")
   l_PROJECT_plane=.FALSE.
 endif
 !
 if (l_PROJECT_line) then
   call PROJECTION_setup( "DOS", DOS_bands )
   YAMBO_ALLOC(el_dos,(GRID_k%nbz/PROJECT_depth,DOS_E_steps,n_spin))
 else
   YAMBO_ALLOC(el_dos,(DOS_E_steps,n_spin,2))
 endif
 el_dos=rZERO
 !
 ! DOS on the built-in grid
 !===================================================================
 !
 ! Load the PDOS coefficient
 !-----------------------------
 if (l_PROJECT_atom) then
   YAMBO_ALLOC(PDOS_fac_v,(DOS_bands(1):DOS_bands(2),Xk%nibz,n_sp_pol))
   call electrons_pdos_factors((/1,Xk%nibz/),DOS_bands,PDOS_fac_v)
   PDOS_fac_p(DOS_bands(1):DOS_bands(2),1:Xk%nibz,1:n_spinor) => PDOS_fac_v
 endif
 !
 if (n_spinor==2) then
   YAMBO_ALLOC(SPIN_fac_v,(DOS_bands(1):DOS_bands(2),Xk%nibz,n_spinor))
   call electrons_spin_factors((/1,Xk%nibz/),DOS_bands,SPIN_fac_v)
   SPIN_fac_p(DOS_bands(1):DOS_bands(2),1:Xk%nibz,1:n_spinor) => SPIN_fac_v
 endif
 !
 if (.not.l_PROJECT_line) then
   call electrons_dos_elemental(Xk,Xen,DOS_bands,el_dos(:,:,1),dos_E,&
&                               SPIN_fac_p,PDOS_fac_p,USE_the_DbGd=l_DbGD)
 endif
 !
 !
 ! Define the regular grid for interpolation if (all(interp_grid>0))
 !===================================================================
 N_files=1
 !
 if (all(INTERP_grid>0).or.l_PROJECT_line) then
   !
   if (l_PROJECT_atom) call warning(" PDOS and interpolation not yet implemented")
   !
   if (.not.l_PROJECT_line) then
     call INTERPOLATION_driver_create_grid(INTERP_grid)
     N_files=2
   endif
   !
   call INTERPOLATION_driver_seed("E_on_built_in",IDe,DOS_bands,Xk,SEED_E=Xen,NEAREST=.FALSE.)
   call INTERPOLATION_driver_do("E_on_built_in",IDe,GRID_k,OUT_E=GRID_E)
   !
   call INTERPOLATION_driver_seed("f_on_built_in",IDf,DOS_bands,Xk,SEED_E=Xen,QUIET=.TRUE.)
   call INTERPOLATION_driver_do("f_on_built_in",IDf,GRID_k,OUT_E=GRID_E)
   !
   if (l_PROJECT_atom) then
     call INTERPOLATION_driver_seed("SPIN_factors_on_built_in",IDp,DOS_bands,Xk,SEED_R3=PDOS_fac_v)
     call INTERPOLATION_driver_do("PDOS_factors_on_built_in",IDp,GRID_k)
     PDOS_fac_p => linkM2M(INTERP_obj(IDp)%INTERP,shape(INTERP_obj(IDp)%INTERP),&
 &                         lbound(INTERP_obj(IDp)%INTERP),ubound(INTERP_obj(IDp)%INTERP))
   endif
   !
   if (n_spinor==2) then
     call INTERPOLATION_driver_seed("SPIN_factors_on_built_in",IDs,DOS_bands,Xk,SEED_R3=SPIN_fac_v)
     call INTERPOLATION_driver_do("SPIN_factors_on_built_in",IDs,GRID_k)
     SPIN_fac_p   => linkM2M(INTERP_obj(IDs)%INTERP,shape(INTERP_obj(IDs)%INTERP),&
&                            lbound(INTERP_obj(IDs)%INTERP),ubound(INTERP_obj(IDs)%INTERP))
   endif
   !
   call INTERPOLATION_driver_end(0)
   !
   do is=1,n_sp_pol
     !
     if (DOS_bands(2)-DOS_bands(1)>10) cycle
     if (.not.allocated(GRID_k%ptbz)) cycle
     !
     output_fname="grid"
     if (n_sp_pol>1.and.is==1) output_fname="grid_spin-up"
     if (n_sp_pol>1.and.is==2) output_fname="grid_spin-db"
     !
     call of_open_close(trim(output_fname),'ot')
     !
     titles(1)  =  " ik  "
     titles(2:4)=(/" kx [iku]"," ky [iku]"," kz [iku]"/)
     do ib=DOS_bands(1),DOS_bands(2)
       titles(4+ib-DOS_bands(1)+1)=' b'//trim(intc(ib))//' [eV]'
     enddo
     call msg('o grid','#',titles(:DOS_bands(2)-DOS_bands(1)+5),INDENT=0,USE_TABS=.true.)    
     call msg('o grid',"#")
     !
     do ikbz=1,GRID_k%nbz
       values(2:4)=GRID_k%ptbz(ikbz,:)
       call K_transform(values(2:4),"iku")
       ik=GRID_k%sstar(ikbz,1)
       values(1)=real(ik,SP)
       values(1+4:DOS_bands(2)-DOS_bands(1)+5)=GRID_E%E(DOS_bands(1):DOS_bands(2),ik,is)*HA2EV
       call msg('o grid',' ',values(:DOS_bands(2)-DOS_bands(1)+5),INDENT=0,USE_TABS=.true.)
     enddo
     !
     call of_open_close(trim(output_fname))
     !
   enddo
   !
   if (l_PROJECT_line) then
     call live_timing('Projecting the DOS',GRID_k%nbz/PROJECT_depth)
     do ik=1,GRID_k%nbz
       ik_path=PROJECTION_k_on_the_path_or_plane(ik)
       if (ik_path==0) cycle
       SLICE_E%E=GRID_E%E(DOS_bands(1):DOS_bands(2),PROJECT_depth*(ik_path-1)+1:PROJECT_depth*ik_path,:)
       SLICE_E%f=GRID_E%f(DOS_bands(1):DOS_bands(2),PROJECT_depth*(ik_path-1)+1:PROJECT_depth*ik_path,:)
       call electrons_dos_elemental(SLICE_k,SLICE_E,DOS_bands,el_dos(ik_path,:,:),dos_E,&
 &                                  SPIN_fac_p,PDOS_fac_p,USE_the_DbGd=.FALSE.,QUIET=.TRUE.)
       call live_timing(steps=1)
     enddo
     call live_timing( )
     !
   else
     !
     ! DOS
     !
     call electrons_dos_elemental(GRID_k,GRID_E,DOS_bands,el_dos(:,:,2),dos_E,&
&                                 SPIN_fac_p,PDOS_fac_p,USE_the_DbGd=.FALSE.)
     !
   endif
 endif
 !
 ! Fermi Level
 !
 if (DOS_bands(1)==1) then
   !
   call section('=','Fermi Levels')
   !
   call msg("s",'Fermi Level  (BUILT-in bands) [eV]:',Xen%E_Fermi*HA2EV)
   if (all(INTERP_grid>0)) call msg("s",'Correction             (GRID) [eV]:',GRID_E%E_Fermi*HA2EV)
   !
 endif
 !
 ! Integrated DOS
 !
 if (n_spin==1) then
   dos_integral(1)=RIntegrate(el_dos(:,1,1),dos_E,DOS_E_steps)
   if (N_files==2) dos_integral(2)=RIntegrate(el_dos(:,1,2),dos_E,DOS_E_steps) 
 else
   dos_integral(1)=RIntegrate(el_dos(:,1,1),dos_E,DOS_E_steps)+RIntegrate(el_dos(:,2,1),dos_E,DOS_E_steps)
   if (N_files==2) dos_integral(2)=RIntegrate(el_dos(:,1,2),dos_E,DOS_E_steps) + RIntegrate(el_dos(:,2,2),dos_E,DOS_E_steps)
 endif
 call msg("s",'Integrated DOS                    :',dos_integral(1)*DL_vol)
 if (N_files==2) then
   call msg("s",'                    [interpolated]:',dos_integral(2)*DL_vol)
 endif
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
   if (l_PROJECT_line) output_fname='el_dos_projected_LINE'
   !
   call of_open_close(trim(output_fname),'ot')
   n_t=0
   if (l_PROJECT_line) then
     n_t=n_t+1
     titles(n_t)='|k| (a.u.)'
   endif
   n_t=n_t+1
   titles(n_t)='E[eV]'
   if (n_spin==1) then
     n_t=n_t+1
     titles(n_t)='DOS'
   else
     n_t=n_t+1
     titles(n_t)='DOS [up]'
     n_t=n_t+1
     titles(n_t)='DOS [dn]'
     n_t=n_t+1
     titles(n_t)='DOS [up+dn]'
   endif
   call msg('o dos','#',titles(:n_t),INDENT=0,USE_TABS=.true.)    
   call msg('o dos',"#")
   !
   ! ...data
   !
   if (l_PROJECT_line) then
     call PROJECTION_plot( "DOS", DOS_E_steps, DOS_bands, W=dos_E, OBS_K=el_dos ) 
   else
     do i_E=1,DOS_E_steps
       if (n_spin==1) call msg('o dos','',(/dos_E(i_E)*HA2EV,el_dos(i_E,1,i_f)/),INDENT=-2,USE_TABS=.true.)
       if (n_spin==2) call msg('o dos','',(/dos_E(i_E)*HA2EV,el_dos(i_E,:,i_f),  &
&                                           el_dos(i_E,1,i_f)+el_dos(i_E,2,i_f)/),INDENT=-2,USE_TABS=.true.)
     enddo
   endif
   !
   call of_open_close(trim(output_fname))
   !
 enddo
 !
 !
 ! CLEAN...
 !
 nullify(SPIN_fac_p)
 nullify(PDOS_fac_p)
 YAMBO_FREE(el_dos)
 YAMBO_FREE(SPIN_fac_v)
 YAMBO_FREE(PDOS_fac_v)
 call bz_samp_reset(SLICE_k)
 call E_reset(SLICE_E)
 !
end subroutine
