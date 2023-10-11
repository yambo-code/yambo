!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine Dipole_kb_pwscf_def_dim()
 !
 use pars,          ONLY:SP,pi,cZERO
 use electrons,     ONLY:n_spinor
 use R_lattice,     ONLY:b
 use wave_func,     ONLY:wf_ncx,wf_nc_k
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species
 use pseudo,        ONLY:pp_n_l_times_proj_max,pp_table,pp_kbv_dim_atom, &
&                        pp_kbs,pp_kb,pp_kbd,pp_kbv_dim,pp_kbv_dim_yambo,   &
&                        pp_kbv_table
 !
 implicit none
 !
 ! Work Space
 !
 complex(SP) :: tmp_var(wf_ncx,n_spinor,2),check(2)
 !
 integer     :: ic,i2,i1,i3,is,ia,i_spinor
 integer     :: im,il,i_pp,j2p1
 !
 allocate(pp_kbv_table(pp_kbv_dim))
 pp_kbv_table=0
 !
 i1=0
 i2=0
 pp_kbv_dim_atom=0
 !
 do is = 1,n_atomic_species
   do ia = 1,n_atoms_species(is)
     i3=0
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
         i3=i3+1
         !
         do i_spinor=1,n_spinor
           do ic=1,wf_nc_k(1)
             tmp_var(ic,n_spinor,1)= pp_kb(ic,is,i_pp,1)*pp_kbs(is,i_pp)
             tmp_var(ic,n_spinor,2)= pp_kbd(ic,is,i_pp,1)+pp_kb(ic,is,i_pp,1)
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
     pp_kbv_dim_atom=max(pp_kbv_dim_atom,i3)
   enddo
 enddo
 !
 pp_kbv_dim_yambo=i1
 !
 return
 !
end subroutine
