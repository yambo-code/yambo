!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine Dipole_kb_pwscf_comp(ik,Xk,kbv)
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
 type(bz_samp) :: Xk
 complex(SP)   :: kbv(wf_ng,n_spinor,4,pp_kbv_dim_yambo)
 integer       :: ik,i_spinor
 ! 
 ! Work Space
 !
 complex(SP) :: Ylm(pp_n_l_max,2*(pp_n_l_max-1)+1),dYlm(2,pp_n_l_max,2*(pp_n_l_max-1)+1)
 complex(SP) :: kb_Ylm(4)
 real(SP)    :: Ygrad(2,3)
 !
 integer     :: ic,ig,i1,i2,is,ia
 integer     :: im,il,i_pp,j2p1
 complex(SP) :: e_iGR
 real(SP)    :: KpG_cc(3),G_cc(3)
 real(SP)    :: KpG_norm,G_dot_Rat,sf
 !
 kbv=cZERO
 !
 do ic=1,wf_nc_k(ik)
   ig=wf_igk(ic,ik)
   if(ig>wf_ng) cycle
   call c2a(b,(Xk%pt(ik,:)+g_vec(ig,:)),KpG_cc,'ki2c')
   call c2a(b,g_vec(ig,:),G_cc,'ki2c')
   KpG_norm=v_norm(KpG_cc)
   if(KpG_norm<1.E-5) cycle
   call DIPOLE_kb_Ylm(Ylm,dYlm,Ygrad,pp_n_l_max,KpG_cc)
   i1=0
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       G_dot_Rat = dot_product(G_cc,atom_pos(:,ia,is))
       e_iGR = cmplx(cos(G_dot_Rat),sin(G_dot_Rat))
       do i_pp=1,pp_n_l_times_proj_max
         !
         il      = pp_table(1,is,i_pp)
         if(il==0) cycle
         j2p1    = pp_table(2,is,i_pp)+1
         !
         do im = 1,2*(il-1)+1
           !
           kb_Ylm(1)=Ylm(il,im)                                              ! To be checked why
           kb_Ylm(2:4)=dYlm(1,il,im)*Ygrad(1,:)+dYlm(2,il,im)*Ygrad(2,:)     ! I need conjg here and 
           kb_Ylm=conjg(kb_Ylm)                                              ! not in abinit
            !                                                               
           i1=i1+1
           i2=pp_kbv_table(i1)
           !
           if(i2==0) cycle
           !
           do i_spinor=1,n_spinor
             !
             sf=pp_factor(im,il,j2p1,is,i_spinor)
             !
             kbv(ig,i_spinor, 1 ,i2)= kb_Ylm(1)*pp_kb(ic,is,i_pp,1)*pp_kbs(is,i_pp)*e_iGR*sf
             kbv(ig,i_spinor,2:4,i2)=(kb_Ylm(1)*KpG_cc(:)/KpG_norm*pp_kbd(ic,is,i_pp,1) +  &
&                                     kb_Ylm(2:4)*pp_kb(ic,is,i_pp,1) )*e_iGR*sf
             !
           enddo
         enddo
       enddo
     enddo
   enddo
 enddo
 !
 !kbv=conjg(kbv)      ! To be checked if I need this for Kerr
 !
 return
 !
end subroutine
