!
!        Copyright (C) 2000-2017 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
!
! Copyright (C) 1992-2005, Lucia Reining, Valerio Olevano,
!   Francesco Sottile, Stefan Albrecht, Giovanni Onida,
!                    Fabien Bruneval
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
subroutine Dipole_kb_pwscf_def_dim()
 !
 use pars,          ONLY:SP,pi,cZERO
 use electrons,     ONLY:n_spinor
 use R_lattice,     ONLY:b,g_vec,bz_samp
 use vec_operate,   ONLY:v_norm,c2a
 use wave_func,     ONLY:wf_ng,wf_igk,wf_nc_k
 use D_lattice,     ONLY:n_atomic_species,atom_pos,n_atoms_species
 use pseudo,        ONLY:pp_n_l_max,pp_n_l_times_proj_max,pp_table, &
&                        pp_kbs,pp_kb,pp_kbd,pp_kbv_dim,pp_kbv_dim_yambo,   &
&                        pp_factor,pp_kbv_table
 !
 implicit none
 !
 ! Work Space
 !
 complex(SP) :: tmp_var(wf_ng,n_spinor,2),check(2)
 !
 integer     :: ic,ig,i2,i1,is,ia,i_spinor
 integer     :: im,il,i_pp,j2p1
 !
 allocate(pp_kbv_table(pp_kbv_dim))
 pp_kbv_table=0
 !
 i1=0
 i2=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     do i_pp=1,pp_n_l_times_proj_max
       !
       il      = pp_table(1,is,i_pp)
       if(il==0) cycle
       j2p1    = pp_table(2,is,i_pp)+1
       !
       do im = 1,2*(il-1)+1
         !
         i1=i1+1
         i2=i2+1
         !
         do i_spinor=1,n_spinor
           do ic=1,wf_nc_k(1)
             ig=wf_igk(ic,1)
             if(ig>wf_ng) cycle
             tmp_var(ig,n_spinor,1)= pp_kb(ic,is,i_pp,1)*pp_kbs(is,i_pp)
             tmp_var(ig,n_spinor,2)= pp_kbd(ic,is,i_pp,1)+pp_kb(ic,is,i_pp,1)
           enddo
         enddo
         !
         check(1)=sum(tmp_var(:,:,1))
         check(2)=sum(tmp_var(:,:,2))
         !
         if ( any(check==cZERO) ) then
           i1=i1-1
           cycle
         endif
         !
         pp_kbv_table(i2)=i1
         !
       enddo
     enddo
   enddo
 enddo
 !
 pp_kbv_dim_yambo=i1
 !
 return
 !
end subroutine
