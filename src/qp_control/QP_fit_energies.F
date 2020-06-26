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
subroutine QP_fit_energies(i_ACTION,bands,en,qp)
 !
 ! Performs a linear fit of the QP corrections in the database
 ! that will be passed to the QP_apply_stretch routine driven by
 ! the mod_QP_CTL module
 !
 use pars,          ONLY:SP
 use QP_m,          ONLY:QP_t
 use QP_CTL_m,      ONLY:QP_action
 use electrons,     ONLY:levels,n_sp_pol,spin
 use POL_FIT_m,     ONLY:POL_LK_fit
 !
 implicit none
 !
 type(QP_t)      ::qp
 type(levels)    ::en
 integer         ::i_ACTION,bands(2)
 !
 ! Work Space
 !
 integer  :: i1,ib,ik,Nqpc,Nqpv,fit_order,i_spin
 real(SP) :: Eoc,Eov,Eov_new,Eoc_new,Ec,Ev,E_tmp,EPS,&
&            Dy(qp%n_states),Dx(qp%n_states),M_def(2),M_val(2),M_con(2)
 !
 ! Loop on spin polarizations
 !
 do i_spin =1 , n_sp_pol
   !
   ! Fit energy range 
   !
   Eov=-1.E5_SP
   Eoc= 1.E5_SP
   Eov_new=-1.E5_SP
   Eoc_new= 1.E5_SP
   Ev =-1.E5_SP
   Ec = 1.E5_SP
   !
   do i1=1,qp%n_states
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2)) cycle
     if (spin(qp%table(i1,:))/=i_spin) cycle
     E_tmp=real(qp%E_bare(i1),SP)
     if(E_tmp<=1.E-5_SP) Eov=max(Eov,E_tmp)
     if(E_tmp> 1.E-5_SP) Eoc=min(Eoc,E_tmp)
     E_tmp=real(qp%E(i1),SP)
     if(E_tmp<=1.E-5_SP) Ev =max(Ev ,E_tmp)
     if(E_tmp> 1.E-5_SP) Ec =min(Ec ,E_tmp)
   enddo
   !
   do ik=1,en%nk
     do ib=bands(1),bands(2)
       if(.not.allocated(en%Eo)) E_tmp=en%E(ib,ik,i_spin)
       if(     allocated(en%Eo)) E_tmp=en%Eo(ib,ik,i_spin)
       if(E_tmp<=1.E-5_SP) Eov_new=max(Eov_new,E_tmp)
       if(E_tmp> 1.E-5_SP) Eoc_new=min(Eoc_new,E_tmp)
     enddo
   enddo
   !
   M_def=(/0._SP,1._SP/)
   M_val=M_def
   M_con=M_def
   !
   ! E>0 (conduction)
   !
   fit_order=1
   !
   Nqpc=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2).or.real(qp%E_bare(i1))<=1.E-5_SP) cycle
     if (spin(qp%table(i1,:))/=i_spin) cycle
     Nqpc=Nqpc+1
     Dy(Nqpc)=real(qp%E(i1)-Ec)
     Dx(Nqpc)=real(qp%E_bare(i1)-Eoc)
   enddo
   if (Nqpc>1) then
     call POL_LK_fit(Nqpc,Dx(:Nqpc),Dy(:Nqpc),fit_order,M_con,EPS,0._SP)
     if (fit_order==0) then
       call warning('Fit order on energies [c] reduced to 0')
       M_con(2)=M_def(2)
     endif
     QP_action(i_ACTION)%CTL(i_spin)%E_err(1)=EPS
   endif
   !
   ! E<0 (valence)
   !
   fit_order=1
   !
   Nqpv=0
   do i1=1,qp%n_states
     if (qp%table(i1,1)<bands(1).or.qp%table(i1,1)>bands(2).or.real(qp%E_bare(i1))>1.E-5) cycle
     if (spin(qp%table(i1,:))/=i_spin) cycle
     Nqpv=Nqpv+1
     Dy(Nqpv)=real(qp%E(i1)-Ev)
     Dx(Nqpv)=real(qp%E_bare(i1)-Eov)
   enddo
   if (Nqpv>1) then
     call POL_LK_fit(Nqpv,Dx(:Nqpv),Dy(:Nqpv),fit_order,M_val,EPS,0._SP)
     if (fit_order==0) then
       call warning('Fit order on energies [v] reduced to 0')
       M_val(2)=M_def(2)
     endif
     QP_action(i_ACTION)%CTL(i_spin)%E_err(2)=EPS
   endif
   !
   ! Storing
   !
   QP_action(i_ACTION)%CTL(i_spin)%E_ref      =(/Eoc    ,Eov/)
   QP_action(i_ACTION)%CTL(i_spin)%E_new_ref  =(/Eoc_new,Eov_new/)
   QP_action(i_ACTION)%CTL(i_spin)%db_scissor = (Ec-Eoc)-(Ev-Eov)
   QP_action(i_ACTION)%CTL(i_spin)%fit_scissor= M_con(1)-M_val(1)
   QP_action(i_ACTION)%CTL(i_spin)%E(1:2)     =(/M_con(1)+Ec-Eoc,M_con(2)/)
   QP_action(i_ACTION)%CTL(i_spin)%E(3:4)     =(/M_val(1)+Ev-Eov,M_val(2)/)
   !
 enddo
 !
end subroutine
