!
! Copyright (C) 2000-2008 A. Marini, C. Attaccalite and the YAMBO team 
!              http://www.yambo-code.org
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
subroutine RT_Energy(en,k,E_kin,E_ion,E_h,Exc,Trace_Gf)
 !
 use pars,           ONLY:SP,DP
 use drivers,        ONLY:l_sc_hf,l_sc_contains_lda
 use R_lattice,      ONLY:bz_samp
 use SC,              ONLY:SC_bands,V_hartree_sc,Ho,rho_n,H_nl_sc,SC_xc_functional
 use X_m,            ONLY:P_square
 use real_time,      ONLY:G_lesser,l_RT_no_updates,H_Hartree_Vxc_0,V_xc_0
 use electrons,      ONLY:levels
 use wave_func,      ONLY:wf
 use FFT_m,          ONLY:fft_size
 use xc_functionals, ONLY:E_xc
 use global_XC,      ONLY:WF_kind,WF_xc_functional
 use QP_m,           ONLY:QP_nk,QP_Sx
 use par_proc_m,     ONLY:pp_redux_wait,pp_indexes,pp_indexes_reset
 use par_indexes_m,  ONLY:par_indexes
 !
 implicit none 
 !
 type(levels),  intent(in) :: en
 type(bz_samp), intent(in) :: k
 complex(SP),   intent(out):: E_kin,E_ion,E_h,Trace_Gf
 real(SP),      intent(out):: Exc
 !
 ! Work Space
 !
 type(pp_indexes) :: px
 integer       :: ik,i1,SC_bands2
 complex(SP)   :: Ex
 !
#if defined _DOUBLE
 real(DP)      :: ddot
 complex(DP)   :: zdotc
#else
 real(SP)      :: sdot
 complex(SP)   :: cdotc
#endif
 !
 call pp_indexes_reset(px)
 call par_indexes(px,(/QP_nk/))
 !
 SC_bands2=SC_bands**2
 !
 H_nl_sc=(0._SP,0._SP)
 !
 ! Non-interaction kinetc energy
 !
 ! E_Kin = - i Tr { P^2 G^<}/2  
 !
 E_kin=(0._SP,0._SP)
 !
 do ik=1,QP_nk
   !
   if (.not.px%i1p(ik)) cycle
   !
#if defined _DOUBLE
   E_kin=E_kin-(0._SP,1._SP)*zdotc(SC_bands2,P_square(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
   E_kin=E_kin-(0._SP,1._SP)*cdotc(SC_bands2,P_square(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !
 call pp_redux_wait(E_kin)
 !
 E_kin=E_kin/2.
 !  
 ! E_ion = - i Tr {Ho G^<} - Ekin
 !
 E_ion=(0._SP,0._SP)
 !
 do ik=1,QP_nk
   !      
   if (.not.px%i1p(ik)) cycle
   !        
#if defined _DOUBLE
   E_ion=E_ion-(0._SP,1._SP)*zdotc(SC_bands2,Ho(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
   E_ion=E_ion-(0._SP,1._SP)*cdotc(SC_bands2,Ho(:,:,ik,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
   !
 enddo
 !
 call pp_redux_wait(E_ion)
 !
 E_ion = E_ion - E_kin
 !
 if(.not.l_RT_no_updates) then
   !
   ! Hartree potential Energy
   !
   E_h=(0._SP,0._SP)
   !
   do ik=1,QP_nk
     !
     if (.not.px%i1p(ik)) cycle
     !        
     H_nl_sc=(0._SP,0._SP)
     !
     call V_real_space_to_H(ik,V_hartree_sc,H_nl_sc,wf)
     !
#if defined _DOUBLE
     E_h=E_h-(0._SP,0.5_SP)*zdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
     E_h=E_h-(0._SP,0.5_SP)*cdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
     !
   enddo
   !
   call pp_redux_wait(E_h)
   !
   if(l_sc_hf) then
     !
     Ex=(0.,0.)
     !
     do ik=1,QP_nk
       !
       if (.not.px%i1p(ik)) cycle
       !        
       H_nl_sc=(0.,0.)
       !
       call V_qp_basis_to_H(ik,QP_Sx,H_nl_sc)
       !
#if defined _DOUBLE
       Ex=Ex+(0.,0.5)*zdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
       Ex=Ex+(0.,0.5)*cdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
       !
     enddo
     !
     Exc=real(Ex)
     !
     call pp_redux_wait(Exc)
     !
   else        
     !
     if(l_sc_contains_lda.and.SC_xc_functional/=0) then 
       !        
       ! E_xc Energy 
       !
       call IO_mute("report log io_in io_out")
       call xc_lda_driver(en,k,WF_KIND,WF_xc_functional,0,G_lesser)
       call IO_mute("  ")
       !
#if defined _DOUBLE
       Exc=ddot(fft_size,rho_n,1,E_xc,1)
#else
       Exc=sdot(fft_size,rho_n,1,E_xc,1)
#endif
       !
     else
       !      
       Exc=(0.,0.)
       !
       do ik=1,QP_nk
         !
         if (.not.px%i1p(ik)) cycle
         !        
         H_nl_sc=(0.,0.)
         !
         call V_real_space_to_H(ik,V_xc_0,H_nl_sc,wf) 
         !
#if defined _DOUBLE
         Exc=Exc+(0.,1.)*zdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#else
         Exc=Exc+(0.,1.)*cdotc(SC_bands2,H_nl_sc(:,:,1),1,G_lesser(:,:,ik),1)*k%weights(ik)
#endif
         !
       enddo
       !
       call pp_redux_wait(Exc)
       !
     endif
     !
   endif
   !
 else
   !
   E_h=(0.,0.)
   !
   do ik=1,QP_nk
     !      
     if (.not.px%i1p(ik)) cycle
     !        
     H_nl_sc=(0.,0.)
#if defined _DOUBLE
     E_h=E_h-(0.,1.0)*zdotc(SC_bands2,H_Hartree_Vxc_0(:,:,ik,1)+H_nl_sc(:,:,1),1, &
&        G_lesser(:,:,ik),1)*k%weights(ik)
#else
     E_h=E_h-(0.,1.0)*cdotc(SC_bands2,H_Hartree_Vxc_0(:,:,ik,1)+H_nl_sc(:,:,1),1, &
&        G_lesser(:,:,ik),1)*k%weights(ik)
#endif
     !
   enddo
   !
   call pp_redux_wait(E_h)
   !
   Exc=0.
   !
 endif
 !
 ! Total number of electrons
 !
 Trace_Gf=(0.,0.)
 !
 do i1=1,SC_bands
   Trace_Gf=Trace_Gf-(0.,1.)*sum(G_lesser(i1,i1,:)*k%weights(:))
 enddo
 !
 call pp_indexes_reset(px)
 !
end subroutine RT_Energy

