!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine ELPH_double_grid(k,E,q)
 !
 use pars,                ONLY:SP,pi,schlen
 use electrons,           ONLY:levels,E_reset,FineGd_E_components_reset
 use R_lattice,           ONLY:bz_samp,bz_samp_reset,BZ_FineGd_mode,nkibz,bz_samp_FineGd_reset
 use YPP_ELPH,            ONLY:ph_modes_file,ph_freqs_file
 use stderr,              ONLY:STRING_same,intc
 use ELPH,                ONLY:PH_freqs_sq,elph_use_q_grid,ph_modes,GKKP,elph_nb,PH_freqs,elph_nQ,EkplusQ_mode, &
&                              elph_grids_are_expanded         
 use com,                 ONLY:msg
 use parser_m,            ONLY:parser
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:OP_RD_CL,REP,DUMP,OP_WR_CL
 use parser_m,            ONLY:parser
 !
#include<memory.h>
 !
 type(bz_samp) ::k,q
 type(levels)  ::E
 !
 ! Work space
 !
 type(bz_samp)         :: q_fine
 logical               :: l_mix,l_expand,l_unexpand,FineGd_is_expanded,l_test_PHDGrid
 integer               :: ID,io_err(2),i_f,i_fg,iq,iq_bz,iq_fg,ib,is,iE_fg,iq_ibz,iq_db
 integer, external     :: io_ELPH,io_Double_Grid
 logical, external     :: file_exists 
 character(schlen)     :: db_name
 !
 call parser('TestPHDGrid',l_test_PHDGrid)
 !
 ! Check input files from matdyn.x
 !
 if(.not.STRING_same(ph_modes_file,'none').and..not.STRING_same(ph_freqs_file,'none')) &
    call error("Please specify the Frequency file or the Matdyn Mode file, not both of them!")
 !
 if(.not.STRING_same(ph_freqs_file,'none')) then
   if(.not.file_exists(trim(ph_freqs_file))) call error("Phonon frequencies file not found!")
 endif
 !
 if(.not.STRING_same(ph_modes_file,'none')) then
    if(.not.file_exists(trim(ph_modes_file))) call error("Phonon modes file not found!")
 endif
 !
 if(.not.STRING_same(EkplusQ_mode,'interp')) then
    call warning("Only Fourier interpolation implemented for the E(k+q) energies, EkplusQmode switched to 'interp'")
    EkplusQ_mode='interp'
 endif
 !
 call section('+','PH-Double Grid initialization')
 !
 ! Expand the Coarse Grid 
 !========================
 call k_build_up_BZ_tables(q)
 call k_ibz2bz(q,'i',.true.)
 !
 ! Read phonon-frequencies in the ELPH-DB
 ! ======================================
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err(1)=io_ELPH(ID,'gkkp no_matrix_elements')
 call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1/),ID=ID)
 io_err(2)=io_ELPH(ID,'gkkp_expanded no_matrix_elements')
 !
 if (io_err(1)==0.and.io_err(2)==0) call error("Both gkkp and gkkp_expanded are present, please remove one of the two")
 if (io_err(1)/=0.and.io_err(2)/=0) call error('El-Ph database not found')
 !
 if (io_err(1)/=0) db_name='gkkp'
 if (io_err(1)/=0) db_name='gkkp_expanded'
 !
 call msg('rs','GKKP database : '//db_name)
 !
 if(.not.elph_use_q_grid) call error('Random q-grid not implemented yet ')
 !
 if(elph_nb/=E%nb) call warning('Nypp/el-ph/ELPH_double_grid.Fumber of bands in the GKKP different from the total number of bands!')
 !
 call E_reset(PH_freqs)
 PH_freqs%nb=ph_modes
 PH_freqs%nk=elph_nQ
 YAMBO_ALLOC(PH_freqs%E,(PH_freqs%nb,PH_freqs%nk,1))
 !
 do iq_ibz=1,q%nibz
   iq_db=iq_ibz
   if(elph_grids_are_expanded) then
      if(iq_db>1) iq_db=sum(q%nstar(:iq_ibz-1))+1
   endif
   call io_control(ACTION=OP_RD_CL,COM=REP,MODE=DUMP,SEC=(/1+iq_db/),ID=ID)
   io_err=io_ELPH(ID,trim(db_name)//' no_matrix_elements')
   PH_freqs%E(:,iq_db,1)=sqrt(abs(PH_freqs_sq(iq_db,:)))
 enddo
 !
 ! Find border points and remove them if required
 ! ===============================================
 call k_map_fine_to_coarse_init(q,PH_freqs,q_fine,FineGd_is_expanded,.TRUE.)
 !
 ! Expansion mode
 ! =======================
 ! Since phonon in the ELPH_Sigma_c are in the BZ, I force
 ! this expantion for the PH-Double-Grid, it can be extended to the other cases
 !
 if(.not.STRING_same(BZ_FineGd_mode,"mixed")) &
&   call warning("Phonon double-grid tested only in 'mixed' mode")
 l_mix     =STRING_same(BZ_FineGd_mode,"mixed")
 l_expand  =STRING_same(BZ_FineGd_mode,"expanded")
 l_unexpand=STRING_same(BZ_FineGd_mode,"unexpanded")
 !
 ! FineGd E to IBZ map
 !=======================
 call section('=','Double Grid Energies-IBZ mapping and Fine Grid re-ordering')
 call k_map_fine_to_coarse('Energies-IBZ',q,q%FGbare,q_fine,PH_freqs)
 !
 ! Expand the Fine Grid 
 !========================
 !
 if (l_mix.or.l_expand) then
   !
   if (.not.FineGd_is_expanded) then
     !
     ! k_DbGd (IBZ->BZ)
     !
     call section('=','Double Grid K-points expansion ...')
     call k_ibz2bz(q_fine,'i',.true.)
     call k_map_fine_to_coarse_reduce(q_fine)
     call msg('nsr',':: Found '//trim(intc(q_fine%nbz))//' ('//trim(intc(q_fine%nibz))//') points in the BZ (IBZ)')
     !
   endif
   !
   q%FGibz%FG_mode="IBZ"
   q%FGbz%FG_mode ="BZ"
   !
 endif
 !
 if (l_expand) then
   !
   ! BZ->IBZ
   !
   YAMBO_FREE(q_fine%pt)
   YAMBO_ALLOC(q_fine%pt,(q_fine%nibz,3))
   q_fine%nibz=q_fine%nbz
   q_fine%pt  =q_fine%ptbz
   q%FGibz%FG_mode="BZ"
   !
 endif
 !
 if (l_unexpand) then
   !
   ! IBZ->BZ
   !
   q_fine%nbz=q_fine%nibz
   YAMBO_ALLOC(q_fine%ptbz,(q_fine%nbz,3))
   YAMBO_ALLOC(q_fine%weights,(q_fine%nibz))
   YAMBO_ALLOC(q_fine%sstar,(q_fine%nibz,2))
   q_fine%ptbz=q_fine%pt
   q_fine%weights=1./real(q_fine%nibz,SP)
   do i_fg=1,q_fine%nibz
     q_fine%sstar(i_fg,:)=(/i_fg,1/)
   enddo
   !
   q%FGibz%FG_mode="IBZ"
   q%FGbz%FG_mode="IBZ"
   !
 endif
 !
 ! IBZ FG_mode to IBZ map
 !========================
 call section('=','Double Grid '//trim(q%FGibz%FG_mode)//'-IBZ mapping')
 call k_map_fine_to_coarse(trim(q%FGibz%FG_mode)//'-IBZ',q,q%FGibz,q_fine,PH_freqs)
 !
 ! BZ FG_mode to BZ map
 !======================
 call section('=','Double Grid '//trim(q%FGbz%FG_mode)//'-BZ mapping')
 call k_map_fine_to_coarse(trim(q%FGbz%FG_mode)//'-BZ',q,q%FGbz,q_fine,PH_freqs)
 !
 ! Interpolation E(k+q) and fill the GKKP%E_kpq
 ! ============================================
 ! E(k+q) is constructed with k in the IBZ and q in the BZ
! if(STRING_same(EkplusQ_mode,'interp')) call En_k_plus_q_interpolate(k,q,q_fine,E,GKKP)
 !
 !
 ! Set all fine grid to the course one (ONLY FOR TESTING PURPOSE)
 ! ===================================
 if(l_test_PHDGrid) then
   !      
   ! Set all PH energies equal to the couse one
   !
   call warning(" THIS IS FICTITIOUS DOUBLE-GRID: results should be equal to the course one ")
   !
   do iq_bz=1,q%nbz        
     !
     iq_ibz =q%sstar(iq_bz,1)
     !     
     do iq_fg=q%FGbz%k_range(iq_bz,1)+1,q%FGbz%k_range(iq_bz,2),1
       iE_fg=q%FGbz%E_map(iq_fg)  
       PH_freqs%FG%E(:,iE_fg,1)=PH_freqs%E(:,iq_ibz,1) 
       q%FGbz%pt(iq_fg,:)=q%ptbz(iq_bz,:)
     enddo
     !
   enddo
   !
 endif
 !
 if(any(E%nbm/=E%nbf)) call warning("Metallic system")
 !
 ! Final I/O
 !===========
 call section('=','Double Grid Database I/O')
 ! .. restoring of built-in value of nkibz to be used in io_header 
 nkibz=q%nibz
 !
 call io_control(ACTION=OP_WR_CL,SEC=(/1/),COM=REP,ID=ID)
 io_err=io_Double_Grid(PH_freqs,q,ID,'phonons')
 !
 ! CLEAN
 !========
 !
 call bz_samp_reset(q_fine)
 call k_ibz2bz(q,'d',.true.)
 call FineGd_E_components_reset(PH_freqs,.TRUE.)
 call bz_samp_FineGd_reset(q%FGbz)
 call bz_samp_FineGd_reset(q%FGibz)
 !
end subroutine
