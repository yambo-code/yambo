! 
!        Copyright (C) 2000-2019 the YAMBO team
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
 use pars,           ONLY:SP,schlen
 use D_lattice,      ONLY:DL_vol
 use R_lattice,      ONLY:bz_samp
 use electrons,      ONLY:levels,spin,n_spinor,n_spin,BZ_DbGd_nbands,n_sp_pol
 use YPP,            ONLY:output_fname,DOS_bands,DOS_E_step,K_transform,&
&                         DOS_E_steps,INTERP_grid,electrons_dos_elemental,&
&                         l_pdos
 use com,            ONLY:msg,of_open_close,warning
 use QP_CTL_m,       ONLY:QP_apply
 use interpolate,    ONLY:INTERPOLATION_driver,GRID_k,INTERP_obj
 use stderr,         ONLY:intc
 use parser_m,       ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp) ::Xk
 type(levels)  ::Xen
 !
 ! Work Space
 ! 
 real(SP)              :: el_dos(DOS_E_steps,n_spin,2),dos_E(DOS_E_steps),values(DOS_bands(2)+4),dos_integral(2)
 real(SP), external    :: RIntegrate
 real(SP),pointer      :: WF_fac(:,:,:)       => null()
 real(SP),pointer      :: WF_fac_int(:,:,:)   => null()
 real(SP),pointer      :: PDOS_fac(:,:,:)     => null()
 !
 integer               :: i_E,i_f,N_files,is,ib,ik,ikbz,ID
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
 call parser('Proj_DOS',l_pdos)
 !
 call QP_state_table_setup(Xen)
 call QP_apply(DOS_bands,Xen,Xk,'G',msg_fmt='s')
 !
 ! Setup
 !=======
 call electrons_dos_setup( Xen, dos_E, DOS_bands )  
 !
 ! DOS on the built-in grid
 !===================================================================
 !
 ! Load the PDOS coefficient
 !-----------------------------
 if (l_pdos) then
   YAMBO_ALLOC_P(PDOS_fac,(DOS_bands(1):DOS_bands(2),Xk%nibz,n_sp_pol))
   call electrons_pdos_factors((/1,Xk%nibz/),DOS_bands,PDOS_fac)
 endif
 !
 if (n_spinor==2) then
   YAMBO_ALLOC_P(WF_fac,(DOS_bands(1):DOS_bands(2),Xk%nibz,2))
   call electrons_spin_factors((/1,Xk%nibz/),DOS_bands,WF_fac)
 endif
 !
 call electrons_dos_elemental(Xk,Xen,DOS_bands,el_dos(:,:,1),dos_E,&
&                             WF_fac=WF_fac,PDOS_fac=PDOS_fac,USE_the_DbGd=l_DbGD)
 !
 ! Define the regural grid for interpolation if (all(interp_grid>0))
 !===================================================================
 N_files=1
 !
 if (all(INTERP_grid>0)) then
   !
   if (l_pdos) call warning(" PDOS and interpolation not yet implemented")
   !
   N_files=2
   !
   call INTERPOLATION_driver(bands=DOS_bands,SEED_E=Xen,SEED_k=Xk,operation="E_on_built_in")
   !
   call INTERPOLATION_driver(bands=DOS_bands,SEED_E=Xen,SEED_k=Xk,operation="f_on_built_in")
   !
   if (n_spinor==2) call INTERPOLATION_driver(bands=DOS_bands,SEED_R3=WF_fac,SEED_k=Xk,&
&                                             operation="SPIN_factors_on_built_in",ID_op=ID)
   !
   call INTERPOLATION_driver(operation="GRID",GRID=INTERP_grid)
   !
   call INTERPOLATION_driver(OUT_k=GRID_k,OUT_E=GRID_E,operation="E_on_built_in")
   !
   call INTERPOLATION_driver(OUT_k=GRID_k,OUT_E=GRID_E,operation="f_on_built_in",QUIET=.TRUE.)
   !
   if (n_spinor==2) call INTERPOLATION_driver(OUT_k=GRID_k,operation="SPIN_factors_on_built_in")
   !
   call INTERPOLATION_driver(operation="END")
   !
   do is=1,n_sp_pol
     !
     if (DOS_bands(2)-DOS_bands(1)>10) cycle
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
   if (n_spinor==2) then
     !
     ! associate pointer to matrix
     !WF_fac_int=INTERP_obj(ID)%INTERP
     !
   endif
   ! 
   call electrons_dos_elemental(GRID_k,GRID_E,DOS_bands,el_dos(:,:,2),dos_E,&
&                               WF_fac=WF_fac_int,PDOS_fac=PDOS_fac,USE_the_DbGd=.FALSE.)
   !
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
   !
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
     if (n_spin==1) call msg('o dos','',(/dos_E(i_E)*HA2EV,el_dos(i_E,1,i_f)/),INDENT=-2,USE_TABS=.true.)
     if (n_spin==2) call msg('o dos','',(/dos_E(i_E)*HA2EV,el_dos(i_E,:,i_f),  &
&                                         el_dos(i_E,1,i_f)+el_dos(i_E,2,i_f)/),INDENT=-2,USE_TABS=.true.)
     !
   enddo
   !
   call of_open_close(trim(output_fname))
   !
 enddo
 !
 ! CLEAN...
 !
 YAMBO_FREE_P(WF_fac)
 YAMBO_FREE_P(PDOS_fac)
 !
end subroutine
