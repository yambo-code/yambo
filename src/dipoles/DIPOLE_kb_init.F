!
! License-Identifier: GPL
!
! Copyright (C) 2014 The Yambo Team
!
! Authors (see AUTHORS file for details): DS
!
subroutine Dipole_kb_init(Xen,Dip,io_err,ID)
 !
 use pars,                ONLY:SP,pi
 use stderr,              ONLY:intc
 use DIPOLES,             ONLY:DIPOLE_t
 use electrons,           ONLY:levels,n_spinor
 use com,                 ONLY:msg
 use IO_int,              ONLY:io_control
 use IO_m,                ONLY:VERIFY,REP,OP_RD_CL
 use D_lattice,           ONLY:n_atomic_species,n_atoms_species
 use pseudo,              ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table, &
&                              pp_factor,pp_n_l_max
 !
#include<memory.h>
 !
 type(levels),  intent(in)    :: Xen
 type(DIPOLE_t),intent(inout) :: Dip
 !
 integer                      :: io_err(3)
 integer                      :: ID
 !
 real(SP)                     :: j,mj
 logical                      :: psp_has_so
 integer                      :: is,ia,il,i_ml,i_mj,l,ml,twojp1,i_m_shift(2),&
&                                i_pp,pp_spin,i_spinor
 integer                      :: io_KB_abinit_err,io_KB_pwscf_err,D(3)
 !
 integer, external            :: io_KB_abinit,io_KB_pwscf
 !
 Dip%Vnl_included=.false.
 !
 io_KB_abinit_err=-1
 io_KB_pwscf_err =-1
 !
 pp_kbv_dim=0
 !
 ! Check for the KB pwscf DB 
 if(.not.io_KB_abinit_err==0) then
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_KB_pwscf_err=io_KB_pwscf(ID)
 endif
 !
 ! Check for the KB abinit DB 
 if(.not.io_KB_pwscf_err==0) then
   call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_KB_abinit_err=io_KB_abinit(ID)
 endif
 !
 Dip%Vnl_included= any((/io_KB_abinit_err==0,io_KB_pwscf_err==0/))
 !
 ! Here the pp_kbv_dim is evaluated
 ! The abinit and the pwscf approach are identical if n_spin=1 (a part from trivial factors)
 ! However for the case n_spin=2 the two approaches are different
 ! * Abinit
 ! If n_sp_pol=2 abinit kb factor are written for spin up and spin down
 ! If npinor=2   abinit kb factor are written with the pp_table and the average only is considered
 ! * PWscf
 ! If n_sp_pol=2 pwscf kb factor are written for the average only
 ! If npinor=2   pwscf kb factor are written in spin space with the spin-angle functions
 !
 if (io_KB_pwscf_err==0) then
   !
   D=(/2*(pp_n_l_max-1)+1,pp_n_l_max,maxval(pp_table(2,:,:)+1)/)
   YAMBO_ALLOC(pp_factor,(D(1),D(2),D(3),n_atomic_species,2))
   !
   do is = 1,n_atomic_species
     !
     psp_has_so=.not.all(pp_table(2,is,:)==0)
     if(     psp_has_so) pp_factor(:,:,:,is,:)=0._SP
     if(.not.psp_has_so) pp_factor(:,:,:,is,:)=1._SP
     !
     do ia = 1,n_atoms_species(is)
       do i_pp = 1, pp_n_l_times_proj_max
         il   = pp_table(1,is,i_pp)
         if(il==0) cycle
         twojp1 = pp_table(2,is,i_pp)+1
         l = il-1
         j = real(twojp1-1,SP)/2._SP
         !
         ! Here I'm doing a loop on the spin-angle projectors j,mj
         ! The associated angular armonics functions have m running
         ! from 1 to 2*l or from 2 to 2*l+1 depending if j>l
         ! Finally the coefficients from spin-angle to angular armonics
         ! coefficient are constructed in ters of Y_{l,m\pm 1}
         ! The shift takes into account the \pm 1 factor
         !
         pp_kbv_dim=pp_kbv_dim+2*l+1 
         !
         if( (.not.psp_has_so) .or. (.not.ia==1) ) cycle
         !
         if(j>real(l,SP)) i_m_shift=(/ 0,+1/)
         if(j<real(l,SP)) i_m_shift=(/-1, 0/)
         !
         ! Map 2jp1 projectors in (2lp1,i_spin) projectors
         !
         do i_mj=1,twojp1
           !
           mj=real(i_mj-1,SP)-j
           if(j>real(l,SP)) ml=int(mj-0.5_SP)
           if(j<real(l,SP)) ml=int(mj+0.5_SP)
           !
           do i_spinor=1,n_spinor
             i_ml=ml+l+1+i_m_shift(i_spinor)
             if(i_ml<1 .or. i_ml>2*l+1 ) cycle
             pp_factor(i_ml,il,twojp1,is,i_spinor)=spinor_factor(ml,l,j,i_spinor)
           enddo
           !
         enddo
       enddo
     enddo
   enddo
 endif
 !
 if (io_KB_abinit_err==0) then
   !
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       do i_pp = 1, pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         if(il==0) cycle
         pp_spin = pp_table(3,is,i_pp)
         if(pp_spin==0) cycle
         if(pp_spin>1 ) cycle
         pp_kbv_dim=pp_kbv_dim+2*(il-1)+1
       enddo
     enddo
   enddo
 endif
 !
 io_err(1)=io_KB_abinit_err
 io_err(2)=io_KB_pwscf_err
 io_err(3)=-1
 !
 if ( pp_kbv_dim>0 ) call msg('s','[x,Vnl] computed using '//trim(intc(pp_kbv_dim))//' projectors')
 if ( pp_kbv_dim>50) call warning(' [x,Vnl] slows the Dipoles computation. To neglect it rename the ns.kb_pp file')
 !
 return
 !
 contains
   !
   real(SP) function spinor_factor(m,l,j,i_spinor)
     !
     implicit none
     !
     integer :: m,l,i_spinor
     real(SP):: j
     !
     if(j>real(l,SP).and.i_spinor==1) spinor_factor= sqrt(real((l+m+1),SP)/real(2*l+1,SP))
     if(j>real(l,SP).and.i_spinor==2) spinor_factor= sqrt(real((l-m  ),SP)/real(2*l+1,SP))
     if(j<real(l,SP).and.i_spinor==1) spinor_factor= sqrt(real((l-m+1),SP)/real(2*l+1,SP))
     if(j<real(l,SP).and.i_spinor==2) spinor_factor=-sqrt(real((l+m  ),SP)/real(2*l+1,SP))
     !
   end function
   !
end subroutine

