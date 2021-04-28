!
!        Copyright (C) 2000-2021 the YAMBO team
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
subroutine K_Transitions_k_resolved(iq,Ken,Xk,Dip)
 !
 ! Out: 
 !
 !   BS_K_dim(1)
 !   BS_H_dim
 !   BS_nT_at_k
 !
 use pars,         ONLY:SP
 use R_lattice,    ONLY:qindx_X,bz_samp
 use electrons,    ONLY:levels,n_sp_pol
 use DIPOLES,      ONLY:DIPOLE_t
 use BS_solvers,   ONLY:BSS_mode,BSS_slepc_double_grp,BSS_slepc_matrix
 use BS,           ONLY:BS_K_dim,BS_H_dim,BS_nT_at_k,BSE_mode,    &
&                       BS_K_coupling,BS_bands,BS_K_coupling,  &
&                       l_BS_ares_asymm,BS_res_ares_dim,BS_mat_res_ares_dim
 use parser_m,     ONLY:parser
 use BS,           ONLY:l_BS_magnons,l_BS_photolum
 !
#include<memory.h>
 !
 type(levels)   :: Ken
 type(bz_samp)  :: Xk
 type(DIPOLE_t) :: Dip
 integer        :: iq
 !
 logical, external :: K_Transitions_single_trans_is_ok
 !
 ! Work Space
 !
 real(SP)     :: E_eh,E_eh_SOC_corr(2/n_sp_pol),f_eh,f_eh_RES,f_eh_PL
 logical      :: l_BS_ares_symm,l_slepc_special
 integer      :: ikbz,ikpbz,iv,ic,ik,ikp,i_sp_v,i_sp_c,ib1,ib2,isp1,&
&                i_star,nb(2),BS_nT_at_k_tmp(Xk%nibz*2),i_res_ares
 !
 ! Here we will have two options to initialize the slepc matrix:
 call parser('BSSSlepcMatrix',BSS_slepc_matrix)
 !
 ! Developer options. Without the code works in default mode
 ! With the user can switch to experimental coding
 call parser('ImposeAsym',l_BS_ares_asymm)
 call parser('ImposeSymm',l_BS_ares_symm)
 !
 l_BS_ares_asymm=l_BS_ares_asymm.or.(iq/=1)
 l_BS_ares_asymm=(l_BS_ares_asymm.or.l_BS_photolum.or.(l_BS_magnons.and.n_sp_pol==2))
 !
#if defined _SLEPC && !defined _SLEPC_OFF
 !
 ! Special case, slepc with distributed matrix need to explicitly compute the anti-resonant block
 l_slepc_special = index(BSS_mode,'s')/=0 .and. BS_K_coupling .and. (.not.BSS_slepc_matrix)
 !
 ! The doubling is activated unless explicily imposed by input since slepc
 ! with shells requires the extended K_multiply_by_V for the non hermitian algorithm
 ! Slepc are not (yet) able to deal with a pseudo-hermitian algorithm 
 l_BS_ares_asymm=l_BS_ares_asymm .or. (l_slepc_special.and..not.l_BS_ares_symm)
 BSS_slepc_double_grp=(.not.l_BS_ares_asymm).and.l_slepc_special
 !
#endif
 !
 l_BS_ares_asymm=l_BS_ares_asymm.and.trim(BSE_mode)/="resonant"
 !
 BS_res_ares_dim=1
 BS_mat_res_ares_dim=1
 if (l_BS_ares_asymm) then
   BS_res_ares_dim=2
   if (.not.BS_K_coupling) BS_mat_res_ares_dim=2
 endif
 !
 BS_K_dim=0
 BS_nT_at_k_tmp=0
 !
 call k_build_up_BZ_tables(Xk)
 !
 nb(1)=Ken%nbf+1
 nb(2)=Ken%nbm
 !
 do i_res_ares=1,BS_res_ares_dim
   do i_sp_c=1,n_sp_pol
     i_sp_v=i_sp_c
     !
     if (l_BS_magnons.and.n_sp_pol==2) then
       if(i_res_ares==1.and.i_sp_c==1) cycle
       if(i_res_ares==2.and.i_sp_c==2) cycle
       i_sp_v=mod(i_sp_c,n_sp_pol)+1
     endif
     !
     do ik=1,Xk%nibz
       do i_star=1,Xk%nstar(ik)
         !
         ikbz=Xk%k_table(ik,Xk%star(ik,i_star))
         ikpbz=qindx_X(iq,ikbz,1)
         ikp  =Xk%sstar(ikpbz,1)
         !
         do iv=BS_bands(1),nb(2)
           do ic=nb(1),BS_bands(2)
             !
             if(i_res_ares==1) then; ib1=iv; ib2=ic; isp1=i_sp_c ; endif
             if(i_res_ares==2) then; ib1=ic; ib2=iv; isp1=i_sp_v ; endif
             !
             if (.not.K_Transitions_single_trans_is_ok(Ken,Dip,iq,ik,ikp,ib1,ib2,isp1,i_res_ares,&
&                                                      E_eh,E_eh_SOC_corr,f_eh,f_eh_RES,f_eh_PL)) cycle
             !
             BS_K_dim(i_res_ares)=BS_K_dim(i_res_ares)+1
             !
             BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)=BS_nT_at_k_tmp(ik+(i_res_ares-1)*Xk%nibz)+1
             !
           enddo ! conduction 
         enddo ! valence
         !
       enddo ! symmetries
     enddo ! k (ibz) 
   enddo ! spin
 enddo ! i_res_ares
 !
 YAMBO_ALLOC(BS_nT_at_k,(Xk%nibz*BS_res_ares_dim))
 BS_nT_at_k=BS_nT_at_k_tmp(:Xk%nibz*BS_res_ares_dim)
 !
 if (any(BS_nT_at_k==0)) call warning(' Null BSE kernel block dimension(s) found.')
 if (sum(BS_nT_at_k)==0) call   error(' The whole BSE has zero dimension !')
 !
 BS_H_dim=BS_K_dim(1)
 if (l_BS_ares_asymm) then
   if (BS_mat_res_ares_dim==2.or.BS_K_coupling) BS_H_dim=BS_K_dim(1)+BS_K_dim(2)
 else
   if (BS_K_coupling) BS_H_dim=2*BS_K_dim(1)
 endif
 !
end subroutine
