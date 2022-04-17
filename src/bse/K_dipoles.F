!
!        Copyright (C) 2000-2021 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine K_dipoles(iq,Ken,Xk,Dip)
 !
 use pars,          ONLY:SP,IP,pi,cZERO,cI
 use IO_m,          ONLY:io_DIP
 use LIVE_t,        ONLY:live_timing
 use vec_operate,   ONLY:v_norm
 use R_lattice,     ONLY:bz_samp,q0_def_norm,qindx_X
 use stderr,        ONLY:intc
 use wave_func,     ONLY:WF
 use interfaces,    ONLY:WF_load,WF_free
 use electrons,     ONLY:levels,spin
 use X_m,           ONLY:global_gauge
 use DIPOLES,       ONLY:DIPOLE_t,DIP_alloc,DIP_rotated
 use BS_solvers,    ONLY:BSS_Vnl_included
 use BS,            ONLY:BS_T_grp,BS_K_coupling,BS_nT_grps,L_kind,BS_bands,&
&                        BS_dip_size,l_BS_trace,BS_n_eh_spaces,&
&                        l_BS_abs,l_BS_kerr,l_BS_magnons,l_BS_dichroism,l_BS_photolum,&
&                        BSE_dipole_geometry,BS_field_direction,l_BS_ares_from_res
 use parallel_int,  ONLY:PP_redux_wait
 use parallel_m,    ONLY:PAR_BS_nT_col_grps,PAR_COM_eh_INDEX,PAR_IND_T_groups,&
&                        PAR_COM_Xk_ibz_INDEX
 use collision_el,  ONLY:elemental_collision,elemental_collision_free,elemental_collision_alloc
 use timing_m,      ONLY:timing
 use vec_operate,   ONLY:v_rotate
 !
#include<memory.h>
 !
 integer       ::iq
 type(levels)  ::Ken
 type(bz_samp) ::Xk
 type(DIPOLE_t)::Dip
 !
 ! Work space
 !
 integer           ::ik_bz,ok_bz,ik,ok,isymm,osymm,iGo,iv,ic,i_sp_pol_c,i_sp_pol_v,i_T_g,i_T,&
&                    io_err,i_res_ares,i_dip
 complex(SP)       ::DIP_Splus,DIP_Smins,DIP_projected(BS_dip_size),DIP_expanded(3)
 character(6)      ::DIP_kind
 !
 type(elemental_collision):: BSE_scatt
 !
 call section('=','Loading Dipoles @q'//trim(intc(iq)))
 !======================================================
 call timing('BSE dipoles',OPR='start')
 !
 if (trim(global_gauge)=='length'  ) DIP_kind ="DIP_iR"
 if (trim(global_gauge)=='velocity') DIP_kind ="DIP_v"
 !
 ! The residuals of the BSE are <wf_k|e^{iqr}|wf_k+q>
 ! These are trivial to compute at finite q, instead they are 
 ! obtained via the dipoles in the q->0 limit and stored to disk
 !
 if (iq==1) then
   call DIPOLE_IO(Xk,Ken,Dip,'read ',io_err,'K')
 else
   call WF_load(WF,1,maxval(qindx_X(:,:,2)),BS_bands,(/1,Xk%nibz/),title='-Oscillators/R space/K_IP')
   call elemental_collision_free(BSE_scatt)
   call elemental_collision_alloc(BSE_scatt,NG=1,TITLE="BSE") 
 endif
 !
 BSS_Vnl_included=Dip%Vnl_included
 !
 if (iq==1) then
   !
   ! Directions for the external field
   !
   BS_field_direction(:,1)=Dip%q0/v_norm(Dip%q0)
   !
   if (l_BS_trace) then
     BS_field_direction(:,1)=(/1._SP,0._SP,0._SP/)
     BS_field_direction(:,2)=(/0._SP,1._SP,0._SP/)
     BS_field_direction(:,3)=(/0._SP,0._SP,1._SP/)
   else if (trim(BSE_dipole_geometry)/='none'.and.l_BS_kerr) then
     call v_rotate(trim(BSE_dipole_geometry),-pi/2._SP,BS_field_direction(:,1),v_out=BS_field_direction(:,2))
   endif
   !
 endif
 !
 if (trim(global_gauge)=='length'  ) BS_field_direction=BS_field_direction*q0_def_norm
 !
 call live_timing('BSE dipoles',PAR_BS_nT_col_grps)
 !
 do i_T_g=1,BS_nT_grps
   !
   if (.not.PAR_IND_T_groups%element_1D(i_T_g)) cycle
   !
   i_res_ares=BS_T_grp(i_T_g)%i_res_ares
   !
   do i_T=1,BS_T_grp(i_T_g)%size
     !
     ! Indexes
     !=========
     !
     ik_bz = BS_T_grp(i_T_g)%table(i_T,1)
     iv    = BS_T_grp(i_T_g)%table(i_T,2)
     ic    = BS_T_grp(i_T_g)%table(i_T,3)
     i_sp_pol_c = BS_T_grp(i_T_g)%table(i_T,4)
     i_sp_pol_v = BS_T_grp(i_T_g)%table(i_T,5)
     !
     if(iq/=1) then
       !
       ok_bz=qindx_X(iq,ik_bz,1)
       iGo  =qindx_X(iq,ik_bz,2)
       !
       ik   =Xk%sstar(ik_bz,1) 
       isymm=Xk%sstar(ik_bz,2) 
       !
       ok   =Xk%sstar(ok_bz,1)
       osymm=Xk%sstar(ok_bz,2)
       !
       BSE_scatt%is=(/ic,ik,isymm,i_sp_pol_c/)
       BSE_scatt%os=(/iv,ok,osymm,i_sp_pol_v/)
       BSE_scatt%qs=(/iGo,iq,1/)
       !
     endif
     !
     ! *length*   : DIP(c,v,k) is     q*<v|r|c> while I need     q*<c|r|v> = conjg(    q*<v|r|c>)
     ! *velocity* : DIP(c,v,k) is q_ver*<v|p|c> while I need q_ver*<c|r|v> = conjg(q_ver*<v|r|c>)
     !
     ! Absorption, KERR, PL, Dichroism
     if(l_BS_abs.or.l_BS_kerr.or.l_BS_photolum.or.l_BS_dichroism) then
       !
       if(iq==1) then
         DIP_expanded=-cI*DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,DIP_kind,Xk)
         do i_dip=1,BS_dip_size
           DIP_projected(i_dip)=dot_product(BS_field_direction(:,i_dip),DIP_expanded)
         enddo
       else
         if(trim(global_gauge)=="length")   call scatter_Bamp(BSE_scatt)
         if(trim(global_gauge)=="velocity") call scatter_Bamp_using_the_gradient(BSE_scatt,"o")
         DIP_projected(1)=-conjg(BSE_scatt%rhotw(1))
       endif
       !
       BS_T_grp(i_T_g)%dipoles_opt(:BS_dip_size,i_T,1)=conjg(DIP_projected(:BS_dip_size))
       if (BS_K_coupling.and.l_BS_ares_from_res) then
         BS_T_grp(i_T_g)%dipoles_opt(:BS_dip_size,i_T,2)=DIP_projected(:BS_dip_size)
       endif
       !
     endif
     !
     ! Dichroism
     if (l_BS_dichroism) then
       if(iq==1) then  
         DIP_expanded=DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,"DIP_L",Xk)
         do i_dip=1,BS_dip_size
           DIP_projected(i_dip)=dot_product(BS_field_direction(:,i_dip),DIP_expanded)
         enddo
         if (trim(global_gauge)=='length'  ) DIP_projected=DIP_projected/q0_def_norm
       else
         call error(' finite q not implemented for dichroism')
       endif
       BS_T_grp(i_T_g)%dipoles_dic(:BS_dip_size,i_T,1)=conjg(DIP_projected(:BS_dip_size))
       if (BS_K_coupling.and.l_BS_ares_from_res) then
         BS_T_grp(i_T_g)%dipoles_dic(:BS_dip_size,i_T,2)=DIP_projected(:BS_dip_size)
       endif
     endif
     !
     ! Magnons
     if (l_BS_magnons) then
       !
       ! notice:
       ! In optics q=0 requires a special treatment due to the non analytic behaviour
       ! This is not needed for magnons.
       ! However to follow the same structure in K_IP.F fow WFs load / DIPOLE_IO here I keep it
       ! The implementation with scatter can be used to double check the spin dipoles
       !
       if(iq==1) then
         DIP_expanded   = DIP_rotated(ic,iv,ik_bz,i_sp_pol_c,"DIP_S",Xk)
       else
         call scatter_Bamp_spin(BSE_scatt,'x')
         DIP_expanded(1)=-conjg(BSE_scatt%rhotw(1))
         call scatter_Bamp_spin(BSE_scatt,'y')
         DIP_expanded(2)=-conjg(BSE_scatt%rhotw(1))
         !The following is not used at present
         call scatter_Bamp_spin(BSE_scatt,'z')
         DIP_expanded(3)=-conjg(BSE_scatt%rhotw(1))
       endif
       !
       ! DIP_Smins(c,v,k) is  <v|S-|c>(i_sp_pol_c), to get <c|S-|v>(i_sp_pol_v) = conjg(<v|S+|c>(i_sp_pol_c))
       ! DIP_Splus(c,v,k) is  <v|S+|c>(i_sp_pol_c), to get <c|S+|v>(i_sp_pol_v) = conjg(<v|S-|c>(i_sp_pol_c))
       !
       DIP_Splus = 0.5_SP*(DIP_expanded(1)+cI*DIP_expanded(2))  ! S+ for c\dn> transitions: <v\up|c\dn>
       DIP_Smins = 0.5_SP*(DIP_expanded(1)-cI*DIP_expanded(2))  ! S- for c\up> transitions: <v\dn|c\up>
       !
       BS_T_grp(i_T_g)%dipoles_mag(1,i_T,1)=conjg(DIP_Splus)   ! S- for |c\dn> trans: <c\dn|S-|v\up>(i_sp_pol_v) = conjg(<v\up|S+|c\dn>(i_sp_pol_c))
       BS_T_grp(i_T_g)%dipoles_mag(2,i_T,1)=conjg(DIP_Smins)   ! S+ for |c\up> trans: <c\dn|S+|v\up>(i_sp_pol_v) = conjg(<v\dn|S-|c\up>(i_sp_pol_c))
                                                               !      or the right R residual associated to (eps_c\dn-eps_v\up)
                                                               ! S- for |v\dn> trans: <v\dn|S-|c\up>(i_sp_pol_c) = conjg(<c\up|S+|v\dn>(i_sp_pol_v))
                                                               ! S+ for |v\up> trans: <v\up|S+|c\dn>(i_sp_pol_c) = conjg(<c\dn|S-|v\up>(i_sp_pol_v))
       if (BS_K_coupling.and.l_BS_ares_from_res) then          ! This is the right A residual associated to (eps_v\up-eps_c\dn)
         BS_T_grp(i_T_g)%dipoles_mag(1,i_T,2)=DIP_Smins        ! S- for |v\dn> trans: <v\dn|S-|c\up>(i_sp_pol_c)
         BS_T_grp(i_T_g)%dipoles_mag(2,i_T,2)=DIP_Splus        ! S+ for |v\up> trans: <v\up|S+|c\dn>(i_sp_pol_c)
       endif
       !
     endif
     !
   enddo
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing()
 !
 if (index(L_kind,"full")>0) then
   do i_T_g=1,BS_nT_grps
     if (.not.allocated(BS_T_grp(i_T_g)%dipoles_opt) ) then
       YAMBO_ALLOC(BS_T_grp(i_T_g)%dipoles_opt,(BS_dip_size,BS_T_grp(i_T_g)%size,2/BS_n_eh_spaces))
       BS_T_grp(i_T_g)%dipoles_opt=cZERO
     endif
     call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_opt, COMM=PAR_COM_Xk_ibz_INDEX%COMM )
     call PP_redux_wait(BS_T_grp(i_T_g)%dipoles_opt, COMM=PAR_COM_eh_INDEX%COMM )
   enddo
 endif
 !
 ! CLEAN
 !
 if (iq==1.and.io_DIP) then
   call DIP_alloc('DIP_iR')
   call DIP_alloc('DIP_P')
   call DIP_alloc('DIP_v')
   call DIP_alloc('DIP_S')
   call DIP_alloc('DIP_spin')
   call DIP_alloc('DIP_orbital')
   call DIP_alloc('P_square') 
   call DIP_alloc('DIP_P_spinor')
   !
   ! Here I need the distribution over k
   call PARALLEL_SETUP_K_scheme("Kdef")
   !
 endif
 !
 if(iq>1) then
   call elemental_collision_free(BSE_scatt)
   call WF_free(WF)
 endif
 !
 call timing('BSE dipoles',OPR='stop')
 !
end subroutine
