!
! License-Identifier: GPL
!
! Copyright (C) 2015 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine Dipole_kb_abinit_comp(npp,npp_tab,ia,is,ik,i_sp_pol,Xk,kbv)
 !
 use pars,          ONLY:SP,pi,cZERO
 use electrons,     ONLY:n_spinor
 use R_lattice,     ONLY:b,g_vec,bz_samp
 use vec_operate,   ONLY:v_norm,c2a
 use wave_func,     ONLY:wf_ng,wf_ncx,wf_nc_k,wf_igk
 use D_lattice,     ONLY:atom_pos
 use pseudo,        ONLY:pp_n_l_times_proj_max,pp_kbs,pp_kbv_dim_atom,&
&                        pp_kb,pp_kbd,pp_table,pp_kbv_table,pp_n_l_max
 !
 implicit none
 !
 type(bz_samp),  intent(in)    :: Xk
 complex(SP),    intent(out)   :: kbv(wf_ncx,n_spinor,4,pp_kbv_dim_atom)
 integer,        intent(in)    :: ia,is,ik,i_sp_pol
 integer,        intent(out)   :: npp
 integer,        intent(inout) :: npp_tab
 ! 
 ! Work Space
 !
 complex(SP) :: Ylm(pp_n_l_max,2*(pp_n_l_max-1)+1),dYlm(2,pp_n_l_max,2*(pp_n_l_max-1)+1)
 complex(SP) :: kb_Ylm(4)
 real(SP)    :: Ygrad(2,3)
 !
 integer     :: ic,ig,i1,i1_tab,i2,pp_spin
 integer     :: im,il,i_pp
 complex(SP) :: e_iGR
 real(SP)    :: KpG_cc(3),G_cc(3)
 real(SP)    :: KpG_norm,G_dot_Rat,sf
 !
 kbv=cZERO
 !
 do ic=1,wf_nc_k(ik)
   ig=wf_igk(ic,ik)
   if (ig > wf_ng) cycle
   call c2a(b,(Xk%pt(ik,:)+g_vec(ig,:)),KpG_cc,'ki2c')
   call c2a(b,g_vec(ig,:),G_cc,'ki2c')
   KpG_norm=v_norm(KpG_cc)
   if(KpG_norm<1.E-5) cycle
   call DIPOLE_kb_Ylm(Ylm,dYlm,Ygrad,pp_n_l_max,KpG_cc)
   i1=0
   i1_tab=npp_tab


       G_dot_Rat = dot_product(G_cc,atom_pos(:,ia,is))
       e_iGR = cmplx(cos(G_dot_Rat),sin(G_dot_Rat))
       do i_pp=1,pp_n_l_times_proj_max
         !
         il      = pp_table(1,is,i_pp)
         if(il==0) cycle
         pp_spin = pp_table(3,is,i_pp)
         if(pp_spin==0) cycle
         if(pp_spin> 1) cycle
         !
         sf=sqrt(4._SP*pi/(2._SP*real(il-1,SP)+1._SP))
         !
         do im = 1,2*(il-1)+1
           !
           kb_Ylm(1)=Ylm(il,im)
           kb_Ylm(2:4)=dYlm(1,il,im)*Ygrad(1,:)+dYlm(2,il,im)*Ygrad(2,:)
           !
           i1_tab=i1_tab+1
           i2=pp_kbv_table(i1_tab)
           !
           if(i2==0) cycle
           !
           i1=i1+1
           !
           kbv(ic,1,1  ,i1)=kb_Ylm(1)*pp_kbs(is,i_pp)*pp_kb(ic,is,i_pp,i_sp_pol)*e_iGR*sf
           kbv(ic,1,2:4,i1)=(kb_Ylm(1)*KpG_cc(:)/KpG_norm*pp_kbd(ic,is,i_pp,i_sp_pol)+ &
&                                 kb_Ylm(2:4)*pp_kb(ic,is,i_pp,i_sp_pol))*e_iGR*sf
           !
         enddo
       enddo


 enddo
 !
 if (n_spinor==2) kbv(:,2,:,:)=kbv(:,1,:,:)
 !
 npp=i1
 npp_tab=i1_tab
 !
 !kbv=conjg(kbv)      ! To be checked if I need this for Kerr
 !
 return
 !
end subroutine
