!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
!@brief Newton solver for the quasi-particle equation
!
subroutine QP_newton(X,Xen,Xk,en,k,q,qp,Xw,Dip)
 !
 use pars,          ONLY:SP,cZERO,schlen
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_ppa,l_mpa,l_elel_corr,l_cohsex,l_rim_w
 use com,           ONLY:msg
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,current_iq,l_recalculate_X
 use QP_m,          ONLY:QP_t,QP_dSc_steps,QP_Vnl_xc,QP_Vxc,QP_Sc,QP_table,&
&                        QP_n_states,QP_dSc,QP_dSc_delta,report_Sc,&
&                        On_Mass_Shell_approx
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,n_bands,spin
 use R_lattice,     ONLY:bz_samp
 use interfaces,    ONLY:OCCUPATIONS_Fermi
 use stderr,        ONLY:intc
 use IO_int,        ONLY:IO_and_Messaging_switch
#if defined _ELPH
 use drivers,       ONLY:l_elph_corr
#endif
#if defined _QED
 use drivers,       ONLY:l_elphoton_corr
#endif
 implicit none
 type(levels) ::en,Xen
 type(bz_samp)::Xk,k,q
 type(X_t)    ::X
 type(QP_t)   ::qp
 type(DIPOLE_t) :: Dip
 type(w_samp) ::Xw
 !
 ! Work Space
 !
 integer     :: i1,i2,iter
 integer     :: ik,is,ib
 complex(SP) :: Z(QP_dSc_steps-1),Eqp(QP_dSc_steps-1)
 !
 ! Dyson equation: Newton solver 
 !
 if (l_cohsex) then
   !
   call msg('r', '[Newton] Sc step       ',QP_dSc_delta*HA2EV,"[eV]")
   call msg('r', '[Newton] Sc steps      ',QP_dSc_steps)
   !
 endif
 !
 ! Self-consisnte GW or GWo can be performed using external scripts
 ! see the wiki 
 ! https://www.yambo-code.eu/wiki/index.php/Self-consistent_GW_on_eigenvalues_only
 ! in this subroutine iter is always 0
 !
 iter=0
 !
 if (l_elel_corr) then
   !
   ! AF,DALV: use of RIM_W implemented for PPA and MPA
   !
   if (l_ppa) then
     !
     if (l_rim_w) call QP_interpolate_W(X,Xw,q,'PPA')
     call QP_ppa_cohsex(X,Xk,en,k,q,qp,Xw,iter)
     !
   else if (l_mpa) then
     !
     if (l_RIM_W) call QP_interpolate_W(X,Xw,q,'MPA')
     call QP_mpa(X,Xk,en,k,q,qp,Xw,iter) 
     !
   else if (l_cohsex) then
     !
     call QP_ppa_cohsex(X,Xk,en,k,q,qp,Xw,iter)
     !
   else 
     !
     call QP_real_axis(X,Xen,Xk,en,k,q,qp,Xw,Dip,iter)
     !
   endif
 else
   QP_Sc=cZERO
 endif
 !
# if defined _ELPH 
 !
 if (l_elph_corr)     call ELPH_Sigma_c(en,k,q,qp)
 !
#endif
 !
# if defined _QED 
 !
 if (l_elphoton_corr) then
   !
   QP_dSc=cZERO
   !
   call QED_Sigma_c(en,k,q,qp)
   !
 endif
 !
#endif
 !
 do i1=1,QP_n_states
   !
   if(.not.l_cohsex) then
     !     
# if defined _QED 
     !
     ! In the e-gamma case the derivative is calculated analitically
     !     
     if (.not.l_elphoton_corr) then
#endif
       !
       QP_dSc(i1,1)=cZERO
       !     
       if (.not.On_Mass_Shell_approx) then
         do i2=1,QP_dSc_steps-1
           QP_dSc(i1,i2)=(QP_Sc(i1,i2+1)-QP_Sc(i1,i2))/QP_dSc_delta
         enddo
       endif
       !
# if defined _QED 
     endif
#endif
     !
     do i2=1,QP_dSc_steps-1
       !
       Z(i2)=1._SP/(1._SP-QP_dSc(i1,i2))
       !
       Eqp(i2)=qp%E(i1)+Z(i2)*QP_Sc(i1,1)
       !
       if (l_elel_corr.and.allocated(QP_Vnl_xc)) Eqp(i2)=Eqp(i2)+Z(i2)*QP_Vnl_xc(i1)
       if (l_elel_corr.and.allocated(QP_Vxc)   ) Eqp(i2)=Eqp(i2)-Z(i2)*QP_Vxc(i1)
       !
     enddo
     !
     qp%E(i1)=Eqp(1)+Z(1)*(qp%E_bare(i1)-qp%E(i1))  ! See Eq.3 of PRB 75, 235102(2007)
     qp%Z(i1)=Z(1)
     !
   else
     !
     ! COHSEX: no energy dependence
     !
     qp%E(i1)=qp%E_bare(i1)+QP_Sc(i1,1)
     if (allocated(QP_Vnl_xc)) qp%E(i1)=qp%E(i1)+QP_Vnl_xc(i1)
     if (allocated(QP_Vxc)   ) qp%E(i1)=qp%E(i1)-QP_Vxc(i1)
     qp%Z(i1)=1._SP
     !
   endif
   !
 enddo
 !
end subroutine

