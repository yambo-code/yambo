! 
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS, AM
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
subroutine electrons_bands(Xk,Xen)
 !
 ! Levels produced by this routine:
 !
 ! CIRCUIT_E    = Eo (+delta E if QP) interpolated @ CIRCUIT_k (no GS SAVE)
 !              = Eo                  interpolated @ CIRCUIT_k (if GS SAVE)
 ! CIRCUIT_dE   = delta E (if QP) interpolated @ CIRCUIT_k (eventually read from GS SAVE)
 ! GRID_E       = interpolated @ GRID_k (if INTERP_grid>0)
 ! CIRCUIT_E_db = eventually read from GS SAVE
 !
 use pars,           ONLY:SP,schlen,lchlen,rZERO
 use units,          ONLY:HA2EV
 use YPPm,           ONLY:BANDS_steps,INTERP_grid,&
&                         BANDS_bands,CIRCUIT_E_DB_path,CIRCUIT_E_db,USER_k,CIRCUIT_k,&
&                         PtsPath,BANDS_path,n_path_pts,CIRCUIT_made_of_SPECIAL_k,&
&                         BANDS_E_and_k_db
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:lattice
 use com,            ONLY:msg
 use parser_m,       ONLY:parser
 use interpolate,    ONLY:electrons_bands_interpolate,GRID_k
 use QP_CTL_m,       ONLY:QP_apply
 use interfaces,     ONLY:OCCUPATIONS_Fermi
 !
#include<memory.h>
 !
 type(bz_samp), intent(inout) :: Xk
 type(levels),  intent(in)    :: Xen
 !
 ! Work Space
 !
 type(levels)          :: GRID_E,CIRCUIT_E,CIRCUIT_dE
 integer               :: ID_spin,ID_magn,ID_bands(3),IDs(5),nfiles(5),nqnt
 real(SP)              :: dEf
 logical               :: BANDS_internal,BANDS_interpolate,GRID_interpolate
 !
 logical, external     :: file_exists
 !
 ! Apply QP corrections
 !====================== 
 call QP_apply(BANDS_bands,Xen,Xk,'G',msg_fmt='s')
 !
 ! IBZ => BZ: need to be done here as QP_apply cleans the %ptbz allocation
 !
 call k_ibz2bz(Xk,'iku',.TRUE.)
 !
 call section('*','Interpolation tool')
 !=====================================
 call parser('BANDS_built_in',BANDS_internal)
 !
 if (.not.file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) CIRCUIT_E_DB_path="none"
 !
 BANDS_E_and_k_db    = trim(CIRCUIT_E_DB_path)/="none"
 BANDS_interpolate   = BANDS_steps>0.or.trim(CIRCUIT_E_DB_path)/="none"
 if (BANDS_E_and_k_db) BANDS_internal=.FALSE.
 GRID_interpolate    = all(INTERP_grid>0)
 !
 call section('+',"Loading special Points for the "//trim(lattice)//" lattice")
 !============================================================================= 
 if (BANDS_E_and_k_db) BANDS_path= " " 
 !
 PtsPath=BANDS_path
 n_path_pts=BANDS_steps
 !
 call k_special(.FALSE.)
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 if (BANDS_interpolate) then
   !
   ! Perform the energies interpolation
   !====================================
   if (.not.GRID_interpolate) call section('=','Interpolation@work: Circuit')
   if (     GRID_interpolate) call section('=','Interpolation@work: Circuit and Grid')
   !
   if (     GRID_interpolate) call k_define_circuit(Xk)
   !
   call electrons_bands_interpolate(Xen,Xk,BANDS_bands,ID_bands,CIRCUIT_E,GRID_E=GRID_E,CIRCUIT_dE=CIRCUIT_dE)
   !
   ! Perform interpolation of spinorial factors and magnetization
   !==============================================================
   if(n_spinor>1 .and. BANDS_steps> 0) then
     call section('=','Interpolation@work: Spin and Magnetization')
     call electrons_spin_and_magn_interpolate("S M",Xk,CIRCUIT_k,BANDS_bands,ID_spin,ID_magn)
   endif
   !
 endif
 !
 ! Fermi Levels
 !==============
 !
 call section('=','Fermi Levels')
 !
 ! AM (March 2018): The dEf must be applied in order to have consistent
 !                  shift of the BUILT-in and the INTERPOLATED grids. Thus it is non-zero
 !                  only when it can be correctly (all occupied bands) evaulated with GRID_E
 !
 if (GRID_interpolate.and.BANDS_bands(1)==1) call OCCUPATIONS_Fermi(GRID_E,GRID_k,mode="FERMI")
 !
 call msg("s",'Fermi Level  (BUILT-in bands) [eV]:',Xen%E_Fermi*HA2EV)
 !
 dEf=rZERO
 !
 if (BANDS_bands(1)==1) then
   dEf=-GRID_E%E_Fermi
   call msg("s",'Correction             (GRID) [eV]:',GRID_E%E_Fermi*HA2EV)
 endif
 !
 call section('=','Bands output')
 !
 IDs=0
 nqnt=1
 nfiles(1)=n_sp_pol
 IDs(1)=ID_bands(2)
 if(allocated(Xen%Eo).and.BANDS_E_and_k_db) IDs(1)=ID_bands(3)
 if(n_spinor==2) then
   nqnt=nqnt+2
   nfiles(nqnt-1:nqnt)=(/2,3/)
   IDs(nqnt-1:nqnt)=(/ID_spin,ID_magn/)
 endif
 !
 if (BANDS_internal.and.USER_k%nbz>0       )  call plot_interpolated_values &
 &  (Xk,Xen,dEf,USER_k,   IDs(1:nqnt),Bands_bands,nqnt,nfiles(1:nqnt),"built_in",    "bands")
 !
 if (BANDS_interpolate.and.BANDS_E_and_k_db)  call plot_interpolated_values &
 &  (Xk,Xen,dEf,CIRCUIT_k,IDs(1:1),   Bands_bands,1,   nfiles(1:1),   "from_DB",     "bands")
 !
 if(BANDS_interpolate                      ) call plot_interpolated_values &
 &  (Xk,Xen,dEf,CIRCUIT_k,IDs(1:nqnt),Bands_bands,nqnt,nfiles(1:nqnt),"interpolated","bands")
 !
 call E_reset(GRID_E)
 call E_reset(CIRCUIT_E)
 call E_reset(CIRCUIT_DE)
 call E_reset(CIRCUIT_E_db)
 !
 call INTERPOLATION_driver_end(0)
 !
end subroutine
