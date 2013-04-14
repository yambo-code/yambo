!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine Dipole_kb_pp_comp(ik,i_spin,Xk,kbv)
 !
 use pars,          ONLY:SP,pi
 use vec_operate,   ONLY:iku_v_norm,v_norm,c2a
 use R_lattice,     ONLY:b,g_vec,bz_samp
 use D_lattice,     ONLY:n_atomic_species,atom_pos,n_atoms_species
 use pseudo,        ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_n_l_comp,pp_kbs,&
&                        pp_kb,pp_kbd,pp_table,pp_n_l_max
 use wave_func,     ONLY:wf,wf_ng
 implicit none
 type(bz_samp) :: Xk
 complex(SP)   :: kbv(wf_ng,pp_kbv_dim,4)
 integer       :: ik,i_spin
 ! 
 ! Work Space
 !
 integer     :: ig,igm,i1,is,ia,il,i_pp,im,pp_spin
 complex(SP) :: Ylm(pp_n_l_max,2*pp_n_l_max+1),&
&               dYlm(2,pp_n_l_max,2*pp_n_l_max+1),c1(n_atomic_species),c2(4)
 real(SP)    :: Ygrad(2,3),v1(3),v2(3),r1,r2
 !
 kbv=(0._SP,0._SP)
 igm=1
 if (iku_v_norm(Xk%pt(ik,:))<=1.E-5) igm=2
 do ig=igm,wf_ng
   call c2a(b,(Xk%pt(ik,:)+g_vec(ig,:)),v1,'ki2c')
   call c2a(b,g_vec(ig,:),v2,'ki2c')
   r1=v_norm(v1)
   call Dipole_kb_Ylm(Ylm,dYlm,Ygrad,pp_n_l_max,v1)
   i1=0
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       r2 = dot_product(v2,atom_pos(:,ia,is))
       c1(is) = cmplx(cos(r2),sin(r2))
       do i_pp=1,pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         pp_spin = pp_table(3,is,i_pp)
         if(pp_spin==0) cycle
         if(pp_spin> 1) cycle
         do im = 1,2*(il-1)+1
           i1=i1+1
           c2(1)=sqrt(4.*pi/(2.*(il-1.)+1.))*Ylm(il,im)
           c2(2:4)=sqrt(4.*pi/(2.*(il-1.)+1.))*&
&                    (dYlm(1,il,im)*Ygrad(1,:)+dYlm(2,il,im)*Ygrad(2,:))
           kbv(ig,i1,1)=c2(1)*pp_kbs(is,i_pp)*pp_kb(ig,is,i_pp,i_spin)*c1(is)
           kbv(ig,i1,2:4)=c1(is)*(v1(:)/r1*pp_kbd(ig,is,i_pp,i_spin)*c2(1)+pp_kb(ig,is,i_pp,i_spin)*c2(2:4))
         enddo
       enddo
     enddo
   enddo
 enddo
 !
end subroutine
