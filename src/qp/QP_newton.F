!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine QP_newton(X,Xen,Xk,en,k,q,qp,Xw,Dip)
 !
 use pars,          ONLY:SP,cZERO,schlen
 use units,         ONLY:HA2EV
 use drivers,       ONLY:l_ppa,l_elel_corr,l_cohsex
 use com,           ONLY:msg
 use DIPOLES,       ONLY:DIPOLE_t
 use X_m,           ONLY:X_t,current_iq,l_recalculate_X
 use QP_m,          ONLY:QP_t,QP_dSc_steps,QP_Vnl_xc,QP_Vxc,QP_Sc,l_update_W,QP_table,&
&                        QP_n_states,QP_dSc,QP_dSc_delta,QP_n_G_bands,GW_iterations,&
&                        GWo_SC_done,GWo_iterations,SC_E_threshold,On_Mass_Shell_approx
 use frequency,     ONLY:w_samp
 use electrons,     ONLY:levels,n_bands,spin
 use R_lattice,     ONLY:bz_samp
 use QP_CTL_m,      ONLY:QP_apply
 use interfaces,    ONLY:OCCUPATIONS_Fermi
 use stderr,        ONLY:intc
 use IO_m,          ONLY:IO_and_Messaging_switch
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
 integer     :: i1,i2,iter,SC_iter,SC_iterations,io_X_err
 integer     :: ik,lowest_bnd,highest_bnd,is,ib,i_spin
 real(SP)    :: SC_corr_prev,SC_corr_now
 complex(SP) :: Z(QP_dSc_steps-1),Eqp(QP_dSc_steps-1)
 integer, external ::  X_dielectric_matrix
 character(schlen) :: FILE_name
 !
 ! Dyson equation: Newton solver 
 !
 if (l_cohsex) then
   !
   call msg('r', '[Newton] Sc step   [ev]:',QP_dSc_delta*HA2EV)
   call msg('r', '[Newton] Sc steps      :',QP_dSc_steps)
   !
 endif
 !
 call msg('rn','[Newton] SC iterations :',GWo_iterations)
 !
 iter=0
 SC_corr_prev=1._SP
 !
 SC_iterations=0
 if(GWo_iterations/=0) SC_iterations=GWo_iterations
 if(GW_iterations /=0) SC_iterations=GW_iterations
 !
 ! GPL_EXCLUDE_START
 !
 do while((iter<=SC_iterations.or.SC_iterations<0).and..not.GWo_SC_done) 
   !
   ! GPL_EXCLUDE_END
   !
   if (l_elel_corr) then
     if (l_ppa.or.l_cohsex) then
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
   SC_corr_now= maxval(real(qp%E(:)-qp%E_bare(:)))
   GWo_SC_done=abs( SC_corr_prev-SC_corr_now )<SC_E_threshold.or.iter==SC_iterations
   !
   ! GPL_EXCLUDE_START
   !
   if (iter>0) call msg('rs','[SC] Max corrections at prev/current cycle [ev]',&
&                      (/SC_corr_prev,SC_corr_now/)*HA2EV)
   SC_corr_prev=SC_corr_now
   !
   if (.not.GWo_SC_done.and.SC_iterations/=0) then
     !
     ! Now I need to transfer the calculated QP corrections to the "en" type
     !
     do i1=1,QP_n_states
        ib =QP_table(i1,1)
        ik =QP_table(i1,3)
        is=spin(QP_table(i1,:))
        en%E(ib,ik,is)=qp%E(i1)
     enddo
     !
     ! Shift the missing bands 
     ! Notice that there is a control in QP_driver to avoid strange QP ranges
     !
     lowest_bnd =minval(QP_table(:,1))
     highest_bnd=maxval(QP_table(:,1))
     !
     do ib=1,lowest_bnd-1
       en%E(ib,:,:)=en%Eo(ib,:,:)+en%E(lowest_bnd,:,:)-en%Eo(lowest_bnd,:,:)
     enddo
     !
     do ib=highest_bnd+1,n_bands
       en%E(ib,:,:)=en%Eo(ib,:,:)+en%E(highest_bnd,:,:)-en%Eo(highest_bnd,:,:)
     enddo
     !
     ! Find new Fermi Energy
     !
     call OCCUPATIONS_Fermi(en,k,mode="OCCUPATIONS")
     !
     ! Write QP on file
     !
     if(l_update_W) then
        FILE_name='qp_G'//trim(intc(iter))//'W'//trim(intc(iter))
     else
        FILE_name='qp_G'//trim(intc(iter))//'W0'
     endif
     call QP_of(qp,en,-1,FILE_name)
     !
     if(l_update_W) then
        !
        ! Copy quasi-particle in Xen
        !
        Xen%E=en%E
        Xen%f=en%f
        !
        call msg('rs','Updating W...')
        !
        ! Recalculate W
        !
        current_iq=0
        call IO_and_Messaging_switch("-report")
        l_recalculate_X=.true.
        io_X_err=X_dielectric_matrix(Xen,Xk,q,X,Xw,Dip,.true.)
        l_recalculate_X=.false.
        call IO_and_Messaging_switch("+report")
        if( io_X_err/=0) call error("Error in the dielectric constant calculation")
        !
     endif
     !
   endif
   !
   ! Increase the counter
   !
   iter=iter+1
   !
 enddo
 !
 ! GPL_EXCLUDE_END
 !
 ! Update GWo_iterations
 !
 if (GWo_iterations<0) GWo_iterations=iter
 if (GW_iterations<0)  GW_iterations =iter
 !
end subroutine
