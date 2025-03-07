!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine ELPH_databases_IO_gkkp_expand(mode,iq,iq_s,k,q,qindx_ID,qindx_ID_frag,GKKP)
 !
 use pars,                ONLY:SP
 use R_lattice,           ONLY:bz_samp,qindx_B,qindx_B_load,qindx_S
 use ALLOC,               ONLY:ELPH_alloc
 use D_lattice,           ONLY:nsym,i_time_rev,sop_inv,sop_tab
 use ELPH,                ONLY:elph_nb,GKKP_me,PH_qpt
 use YPP_ELPH,            ONLY:DB_ph_K_sym_table,YAMBO_K_found,l_use_qindxB
 !
 implicit none
 !
 character(*)  ::mode
 integer, intent(in) :: qindx_ID,qindx_ID_frag
 integer, intent(in) :: iq,iq_s
 type(bz_samp), intent(in) ::k,q
 type(GKKP_me) ::GKKP
 !
 ! Work Space 
 !
 integer       ::ik_bz,ib,ob,ok,ik_bz_gkkp,os,ik,ok_bz,is,nsmall,small(nsym)
 logical       ::local_K_found(k%nbz),l_bands_reverted
 type(GKKP_me) ::GKKP_expanded
 integer, external ::qindx_B_init,qindx_B_close 
 integer       :: qindx_tmp(2),iq_bz
 !
 ! "K" mode
 !==========
 !
 if (mode=="K") then
   !
   local_K_found=YAMBO_K_found
   !
   ! The small group of q
   call q_small( )
   ! 
   ! The GS K-grid is reduced using the small group of q. 
   ! I need to expand the K-grid to define the missing elements of elph_dV
   do ik=1,k%nbz
     if (.not.local_K_found(ik)) cycle
     do is=1,nsmall
       ik_bz=DB_ph_K_sym_table(ik,sop_inv(small(is)))
       if (local_K_found(ik_bz)) cycle
       GKKP%dVc(:,:,:,ik_bz,1)=GKKP%dVc(:,:,:,ik,1)
       if (allocated(GKKP%dVr)) GKKP%dVr(:,:,:,ik_bz,1)=GKKP%dVr(:,:,:,ik,1)
       GKKP%E_kpq(iq)%E(:,ik_bz,1)=GKKP%E_kpq(iq)%E(:,ik,1)
       local_K_found(ik_bz)=.TRUE.
     enddo
   enddo
   !
   if(any(local_K_found.eqv..FALSE.)) &
&     call error(' Error expanding GKKP: please set force_symmorphic=.true. in NSCF and nk=1 in DVSCF calculation ')
   !
 endif
 !
 if (.not.mode=="Q") return
 !
 ! "Q" mode
 !==========
 ! GKKP m.e. rotation. Procedure copied from ELPH_Sigma_c
 !
 call ELPH_alloc("LEVELS dV",GKKP=GKKP_expanded,Nq_mem=1)
 !
 if(l_use_qindxB) qindx_tmp=qindx_B_init(qindx_ID,qindx_ID_frag) 
 !
 do ik_bz=1,k%nbz
   !
   ik=k%sstar(ik_bz,1)
   is=k%sstar(ik_bz,2)
   !
   if(l_use_qindxB) then
     !
     do ok_bz=1,k%nbz
       !
       qindx_tmp=qindx_B_load(ok_bz,ik_bz,qindx_ID_frag)
       iq_bz=qindx_tmp(1)
       !
       if (iq_bz==iq) then
         ok=k%sstar(ok_bz,1)
         os=k%sstar(ok_bz,2)
         exit
       endif
     enddo
     !
   else
     !
     ! Use qindx_S     
     !
     ok_bz=qindx_S(ik,iq,1)
     ok=k%sstar(ok_bz,1)
     os=k%sstar(ok_bz,2)
     !
   endif
   !
   l_bands_reverted=.FALSE.
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
     l_bands_reverted=.TRUE.
     !
   else
     ! 
     ! gkkp_{I_need}= <Tk+Rq n'|dV_{SCF}/du^{Rq nu}|Tk n>=
     !                <(R^-1 T k)+q n'|dV_{SCF}/du^{q nu}|(R^-1 T k) n>= 
     !                gkkp(ik_bz,nu,n',n)
     !
     ik_bz_gkkp=k%k_table(ik,sop_tab(sop_inv(iq_s),is))
     !
   endif
   !
   do ib=1,elph_nb
     do ob=1,elph_nb
       if (l_bands_reverted) then
         GKKP_expanded%dVc(:,ob,ib,ik_bz,1)=conjg(GKKP%dVc(:,ib,ob,ik_bz_gkkp,1))
       else
         GKKP_expanded%dVc(:,ib,ob,ik_bz,1)=GKKP%dVc(:,ib,ob,ik_bz_gkkp,1)
       endif
       !
     enddo
   enddo
 enddo
 !
 ! For the energies E(k+q)
 ! 
 do ik_bz=1,k%nbz
   ! 
   ik=k%sstar(ik_bz,1)
   is=k%sstar(ik_bz,2)
   !
   if(l_use_qindxB) then
     !       
     ! Use qindx_B
     !
     do ok_bz=1,k%nbz
       !
       qindx_tmp=qindx_B_load(ok_bz,ik_bz,qindx_ID_frag)
       iq_bz=qindx_tmp(1)
       !
       if (iq_bz==iq) then
         ok=k%sstar(ok_bz,1)
         os=k%sstar(ok_bz,2)
         exit
       endif
       !
     enddo
     !
   else
     !      
     ! Use qindx_S
     !
     ok_bz=qindx_S(ik,iq,1)
     ok=k%sstar(ok_bz,1)
     os=k%sstar(ok_bz,2)
     !
   endif
   !
   if (iq_s>nsym/(i_time_rev+1)) then
     ik_bz_gkkp=k%k_table(ok,sop_tab(sop_inv(iq_s-nsym/2),os))
   else
     ik_bz_gkkp=k%k_table(ik,sop_tab(sop_inv(iq_s),is))
   endif
   !
   ! Notice that the ok_bz index referees to the k-points in the Yambo order
   ! like in the q=0 case
   !
   GKKP_expanded%E_kpq(iq)%E(:,ik_bz,1)=GKKP%E_kpq(1)%E(:,ok_bz,1)
   !
 enddo
 !
 GKKP%dVc=GKKP_expanded%dVc
 GKKP%E_kpq(iq)%E=GKKP_expanded%E_kpq(iq)%E
 !
 if(l_use_qindxB) qindx_tmp=qindx_B_close(qindx_ID,qindx_ID_frag)
 !
 call ELPH_alloc("FREE",GKKP=GKKP_expanded)
 !
 contains
   !
   subroutine q_small( )
   !---------------------
     use pars,           ONLY:SP
     use vec_operate,    ONLY:rlu_v_is_zero,c2a,k2bz
     use D_lattice,      ONLY:nsym
     use R_lattice,      ONLY:rl_sop
     use YPP_ELPH,       ONLY:l_gkkp_expand
     use zeros,          ONLY:k_rlu_zero
     !
     real(SP) :: r_v(3)
     integer  :: is,iq_db
     !
     small=0
     nsmall=0
     !
     iq_db=iq  ! when this function is called iq is in the IBZ
     !
     if(l_gkkp_expand) then
       iq_db=iq
       if(iq>1) iq_db=sum(q%nstar(:iq-1))+1
     endif
     !
     do is=1,nsym
       r_v=matmul(rl_sop(:,:,is),PH_qpt(iq_db,:)) -PH_qpt(iq_db,:)
       call k2bz(r_v)
       call c2a (v_in=r_v,mode='ki2a')
       if (rlu_v_is_zero(r_v,zero_=k_rlu_zero)) then
         nsmall=nsmall+1
         small(nsmall)=is
       endif
     enddo
     !
   end subroutine
   !
end subroutine
