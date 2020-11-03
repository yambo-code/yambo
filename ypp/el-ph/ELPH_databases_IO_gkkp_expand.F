!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
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
subroutine ELPH_databases_IO_gkkp_expand(mode,iq,iq_s,k,GKKP,GKKP_expanded)
 !
 use pars,                ONLY:SP
 use R_lattice,           ONLY:bz_samp,qindx_B
 use D_lattice,           ONLY:nsym,i_time_rev,sop_inv,sop_tab
 use ELPH,                ONLY:elph_nb,GKKP_me,PH_qpt
 use YPP_ELPH,            ONLY:DB_ph_K_sym_table,YAMBO_K_found
 !
 implicit none
 !
 character(*)  ::mode
 integer       ::iq,iq_s
 type(bz_samp) ::k
 type(GKKP_me) ::GKKP,GKKP_expanded
 !
 ! Work Space 
 !
 integer  ::ik_bz,ib,ob,ok,ik_bz_gkkp,os,ik,ok_bz,is,nsmall,small(nsym)
 !
 ! "K" mode
 !==========
 !
 if (mode=="K") then
   !
   ! The small group of q
   call q_small( )
   ! 
   ! The GS K-grid is reduced using the small group of q. 
   ! I need to expand the K-grid to define the missing elements of elph_dV
   do ik=1,k%nbz
     if (.not.YAMBO_K_found(ik)) cycle
     do is=1,nsmall
       ik_bz=DB_ph_K_sym_table(ik,sop_inv(small(is)))
       if (YAMBO_K_found(ik_bz)) cycle
       GKKP%dVc(:,:,:,ik_bz,1)=GKKP%dVc(:,:,:,ik_bz,1)
       if (allocated(GKKP%dVr)) GKKP%dVr(:,:,:,ik_bz,1)=GKKP%dVr(:,:,:,ik_bz,1)
       GKKP%E_kpq(iq)%E(:,ik_bz,1)=GKKP%E_kpq(iq)%E(:,ik,1)
       YAMBO_K_found(ik_bz)=.TRUE.
     enddo
   enddo
   !
 endif
 !
 if (.not.mode=="Q") return
 !
 ! "Q" mode
 !==========
 ! GKKP m.e. rotation. Procedure copied from ELPH_Sigma_c
 !
 do ik_bz=1,k%nbz
   !
   ik=k%sstar(ik_bz,1)
   is=k%sstar(ik_bz,2)
   !
   do ok_bz=1,k%nbz
     if (qindx_B(ik_bz,ok_bz,1)==iq) then
       ok=k%sstar(ok_bz,1)
       os=k%sstar(ok_bz,2)
       exit
     endif
   enddo
   !
   do ib=1,elph_nb
     do ob=1,elph_nb
       ! 
       ! When using a uniform Q grid I cycle on the q symmetries 
       ! as well. To rotate the gkkp m.e. I use:
       !
       if (iq_s>nsym/(i_time_rev+1)) then
         ! 
         ! gkkp_{I_need}= <Tk+IRq n'|dV_{SCF}/du^{IRq nu}|Tk n>=
         !                [<(R^-1 S p)+q n|dV_{SCF}/du^{q nu}|(R^-1 S p) n'>]^*= 
         !                [gkkp(ik_bz,nu,n,n')]^*
         ! 
         ! with Tk + IRq = Sp 
         !                                 R^-1                 S
         !                                 -------------------- --
         ik_bz_gkkp=k%k_table(ok,sop_tab(sop_inv(iq_s-nsym/2),os))
         !
         !elph_gkkp_expanded(:,ob,ib,ik_bz)=conjg(elph_gkkp(:,ib,ob,ik_bz_gkkp))
         !
       else
         ! 
         ! gkkp_{I_need}= <Tk+Rq n'|dV_{SCF}/du^{Rq nu}|Tk n>=
         !                <(R^-1 T k)+q n'|dV_{SCF}/du^{q nu}|(R^-1 T k) n>= 
         !                gkkp(ik_bz,nu,n',n)
         !
         ik_bz_gkkp=k%k_table(ik,sop_tab(sop_inv(iq_s),is))
         !
         !elph_gkkp_expanded(:,ib,ob,ik_bz)=elph_gkkp(:,ib,ob,ik_bz_gkkp)
         !
       endif
       !
     enddo
   enddo
 enddo
 !
 !elph_gkkp=elph_gkkp_expanded
 !
 contains
   !
   subroutine q_small( )
   !---------------------
     use pars,           ONLY:SP
     use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
     use D_lattice,      ONLY:nsym
     use R_lattice,      ONLY:rl_sop
     use zeros,          ONLY:k_rlu_zero
     real(SP) :: r_v(3)
     integer  :: is
     small=0
     nsmall=0
     do is=1,nsym
       r_v=matmul(rl_sop(:,:,is),PH_qpt(iq,:)) -PH_qpt(iq,:)
       call k2bz(r_v)
       call c2a (v_in=r_v,mode='ki2a')
       if (rlu_v_is_zero(r_v,zero_=k_rlu_zero)) then
         nsmall=nsmall+1
         small(nsmall)=is
       endif
     enddo
   end subroutine
   !
end subroutine
