!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine Dipole_kb_abinit_def_dim()
 !
 ! Here I assume the number of projectors different from zero
 ! does not depend on k-points and sp_pol components
 ! To be verified
 !
 use pars,          ONLY:SP,pi,cZERO
 use R_lattice,     ONLY:b
 use wave_func,     ONLY:wf_ncx
 use D_lattice,     ONLY:n_atomic_species,n_atoms_species
 use pseudo,        ONLY:pp_kbv_dim,pp_kbv_dim_yambo,pp_n_l_times_proj_max,pp_kbs,&
&                        pp_kb,pp_kbd,pp_table,pp_kbv_table,pp_kbv_dim_atom
 !
 implicit none
 !
 ! Work Space
 !
 complex(SP) :: tmp_var(wf_ncx,2),check(2)
 !
 integer     :: i1,i2,i3,is,ia,pp_spin
 integer     :: im,il,i_pp
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
       pp_spin = pp_table(3,is,i_pp)
       if(pp_spin==0) cycle
       if(pp_spin> 1) cycle
       !
       do im = 1,2*(il-1)+1
         !
         i1=i1+1
         i2=i2+1
         i3=i3+1
         !
         tmp_var(:,1)= pp_kbs(is,i_pp)*pp_kb(:wf_ncx,is,i_pp,1)
         tmp_var(:,2)= pp_kbd(:wf_ncx,is,i_pp,1)+pp_kb(:wf_ncx,is,i_pp,1)
         !
         check(1)=sum(tmp_var(:,1))
         check(2)=sum(tmp_var(:,2))
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
