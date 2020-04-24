!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine Pseudo_KB_G_to_R_space(Xk,Xen)
 !
 use pars,          ONLY:SP
 use drivers,       ONLY:l_sc_magnetic
 use electrons,     ONLY:levels,n_spinor,n_sp_pol
 use X_m,           ONLY:X_t
 use wave_func,     ONLY:wf_ng
 use FFT_m,         ONLY:fft_size
 use pseudo,        ONLY:pp_kbv_dim_yambo
 use R_lattice,     ONLY:bz_samp
 use LIVE_t,        ONLY:live_timing
 use IO_m,          ONLY:io_control,manage_action,OP_WR,WR_CL_IF_END,OP_IF_START_RD_CL_IF_END
 use timing_m,      ONLY:timing
 !
#include<memory.h>
 !
 type(bz_samp) :: Xk
 type(levels)  :: Xen
 !
 ! Work Space
 !
 type(X_t)     :: X
 !
 complex(SP),allocatable :: kbv(:,:,:,:),kbv_real_space(:,:,:)
 !
 integer,    allocatable :: kbv_real_space_table(:)
 integer                 :: ik,i_sp_pol,ifrag
 !
 ! I / O
 !
 integer                 :: IO_ACT_NOW,ID,ID_R,io_err(3),io_KB_pwscf_err,io_KB_abinit_err,io_Vnl_err,io_KB_real_space_err
 integer, external       :: io_KB_abinit,io_KB_pwscf,io_KB_real_space
 !
#if defined _RT
 call timing('Pseudo kbv FFT',OPR='start')
#endif
 !
 call DIPOLE_kb_init(Xen,X,io_err,ID)
 !
 io_KB_abinit_err= io_err(1)
 io_KB_pwscf_err = io_err(2)
 io_Vnl_err     = io_err(3)
 !
 if(io_KB_abinit_err/=0.and.io_KB_pwscf_err/=0) return
 !
 ! Define pp_kbv_dim_yambo and pp_kbv_table
 !
 if (io_KB_pwscf_err==0) then
   call io_control(ACTION=RD,SEC=(/2/),ID=ID)
   io_KB_pwscf_err=io_KB_pwscf(ID)
   call DIPOLE_kb_pwscf_def_dim()
 endif
 !
 if (io_KB_abinit_err==0) then
   call io_control(ACTION=RD,SEC=(/2/),ID=ID)
   io_KB_abinit_err=io_KB_abinit(ID)
   call DIPOLE_kb_abinit_def_dim()
 endif
 !
 if(fft_size==0) call fft_setup(0,1,.not.l_sc_magnetic)
 !
 YAMBO_ALLOC(kbv,(wf_ng,n_spinor,4,pp_kbv_dim_yambo))
 YAMBO_ALLOC(kbv_real_space,(fft_size,n_spinor,pp_kbv_dim_yambo))
 YAMBO_ALLOC(kbv_real_space_table,(pp_kbv_dim_yambo))
 !
 call define_kbv_table()
 !
 call io_control(ACTION=OP_WR,SEC=(/1/),ID=ID_R)
 io_KB_real_space_err=io_KB_real_space(ID_R,kbv_real_space,kbv_real_space_table)
 !
 call live_timing('KBV-pseudo',Xk%nibz*n_sp_pol)
 !
 do i_sp_pol=1,n_sp_pol
   do ik=1,Xk%nibz
     !
     ifrag=ik+(i_sp_pol-1)*Xk%nibz
     ! 
     ! [Vnl,r]
     !
     if (io_KB_pwscf_err==0.and.i_frag>1) then
       IO_ACT_NOW=manage_action(OP_IF_START_RD_CL_IF_END,ifrag,1,Xk%nibz*n_sp_pol)
       call io_control(ACTION=IO_ACT_NOW,SEC=(/ifrag+1/),ID=ID)
       io_KB_pwscf_err=io_KB_pwscf(ID)
     endif
     !
     if (io_KB_abinit_err==0.and.i_frag>1) then
       IO_ACT_NOW=manage_action(OP_IF_START_RD_CL_IF_END,ifrag,1,Xk%nibz*n_sp_pol)
       call io_control(ACTION=IO_ACT_NOW,SEC=(/ifrag+1/),ID=ID)
       io_KB_abinit_err=io_KB_abinit(ID)
     endif
     !  
     ! Pseudo(k+G,k+G',i1) = kbv^*(k+G,i1,1) kbv(k+G',i1,1)
     ! i1 runs over atomic_species, atoms_per_specie , l_per_atom, m=-l,-l+1,...,l-1,sl
     if (io_KB_abinit_err==0) call DIPOLE_kb_abinit_comp(ik,i_sp_pol,Xk,kbv)
     if (io_KB_pwscf_err ==0) call DIPOLE_kb_pwscf_comp(ik,Xk,kbv)
     !
     call Pseudo_KB_FFT(kbv,kbv_real_space,Xk%pt(ik,:))
     !
     IO_ACT_NOW=manage_action(WR_CL_IF_END,ifrag,0,Xk%nibz*n_sp_pol)
     call io_control(ACTION=IO_ACT_NOW,SEC=(/ifrag+1/),ID=ID_R)
     io_KB_real_space_err=io_KB_real_space(ID_R,kbv_real_space,kbv_real_space_table)
     !
     call live_timing(steps=1)
     !
   enddo
 enddo
 !
 call live_timing()
 !
 ! Free mem
 YAMBO_FREE(kbv_real_space_table)
 YAMBO_FREE(kbv_real_space)
 YAMBO_FREE(kbv)
 !
#if defined _RT
 call timing('Pseudo kbv FFT',OPR='stop')
#endif
 !
 contains
   !
   subroutine define_kbv_table()
     !
     use electrons,     ONLY:n_spinor
     use pseudo,        ONLY:pp_n_l_times_proj_max,pp_table
     use D_lattice,     ONLY:n_atomic_species,n_atoms_species,n_atoms
     !
     integer :: i_atom,i1,is,ia,i_pp,il,im,i_spinor,pp_spin
     !
     i_atom=0
     i1=0
     do is = 1,n_atomic_species
       do ia = 1,n_atoms_species(is)
         i_atom=i_atom+1
         do i_pp = 1, pp_n_l_times_proj_max
           il = pp_table(1,is,i_pp)
           pp_spin = pp_table(3,is,i_pp)   ! neglect the so-term for abinit KB
           if(pp_spin>1) cycle
           do im = 1,2*(il-1)+1
             do i_spinor=1,n_spinor
               i1=i1+2-i_spinor
               kbv_real_space_table(i1)=i_atom
             enddo
           enddo
         enddo
       enddo
     enddo

   end subroutine define_kbv_table
   !
end subroutine Pseudo_KB_G_to_R_space
