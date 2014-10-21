!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
subroutine Dipole_kb_init(Xen,X,io_err,ID)
 !
 use pars,                ONLY:SP,pi
 use stderr,              ONLY:intc
 use X_m,                 ONLY:X_t
 use electrons,           ONLY:levels
 use com,                 ONLY:msg,warning
 use IO_m,                ONLY:io_control,VERIFY,REP,OP_RD_CL,OP_RD
 use D_lattice,           ONLY:n_atomic_species,n_atoms_species
 use pseudo,              ONLY:pp_kbv_dim,pp_n_l_times_proj_max,pp_table, &
&                              spin_m_table,pp_factor,pp_n_l_max
 !
 implicit none
 !
 type(levels),  intent(in)    :: Xen
 type(X_t),     intent(inout) :: X
 !
 integer                      :: io_err(3)
 integer                      :: ID
 !
 real(SP)                     :: j
 logical                      :: psp_has_so
 integer                      :: is,ia,il,im,l,m,j2p1,im_index,im_max,i_pp,pp_spin
 integer                      :: io_KB_abinit_err,io_KB_pwscf_err,io_Vnl_err
 !
 integer, external            :: io_KB_abinit,io_KB_pwscf,io_Vnl
 !
 X%Vnl_included=.false.
 !
 io_Vnl_err     =-1
 io_KB_abinit_err=-1
 io_KB_pwscf_err =-1
 !
 pp_kbv_dim=0
 !
 ! Check for the Vnl DB
 call io_control(ACTION=OP_RD_CL,COM=REP,SEC=(/1,2/),MODE=VERIFY,ID=ID)
 io_Vnl_err=io_Vnl(X,Xen,ID)
 !
 ! Check for the KB pwscf DB 
 if(.not.io_KB_abinit_err==0) then
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_KB_pwscf_err=io_KB_pwscf(ID)
 endif
 !
 ! Check for the KB abinit DB 
 if(.not.io_KB_pwscf_err==0) then
   call io_control(ACTION=OP_RD,COM=REP,SEC=(/1/),MODE=VERIFY,ID=ID)
   io_KB_abinit_err=io_KB_abinit(ID)
 endif
 !
 X%Vnl_included= any((/io_KB_abinit_err==0,io_KB_pwscf_err==0,io_Vnl_err==0/))
 !
 ! Here the pp_kbv_dim is evaluated
 ! Also the spin_m_table and the pp_factor table are created
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
   allocate(spin_m_table(3,pp_n_l_max,maxval(pp_table(2,:,:)+1),n_atomic_species))
   spin_m_table=0
   allocate(pp_factor(2*(pp_n_l_max-1)+1,pp_n_l_max,maxval(pp_table(2,:,:)+1),n_atomic_species,2))
   pp_factor=1._SP
   !
   do is = 1,n_atomic_species
     psp_has_so=.not.all(pp_table(2,is,:)==0)
     do ia = 1,n_atoms_species(is)
       do i_pp = 1, pp_n_l_times_proj_max
         il   = pp_table(1,is,i_pp)
         if(il==0) cycle
         j2p1 = pp_table(2,is,i_pp)+1
         l = il-1
         j = real(j2p1-1,SP)/2._SP
         !
         if(.not.psp_has_so)             im_max=2*l+1                    ! These are needed for
         if(psp_has_so.and.j>real(l,SP)) im_max=2*l+1                    ! spin-angle functions
         if(psp_has_so.and.j<real(l,SP)) im_max=2*l                      ! im_max
         if(ia==1) then                                                  !
           spin_m_table(1,il,j2p1,is) = im_max                           ! 
           spin_m_table(2,il,j2p1,is) = 0                                ! im_shift dw
           if(.not.psp_has_so)             spin_m_table(3,il,j2p1,is)=0  ! im_shift up
           if(psp_has_so.and.j>real(l,SP)) spin_m_table(3,il,j2p1,is)=0  ! Also pp_factor is need
           if(psp_has_so.and.j<real(l,SP)) spin_m_table(3,il,j2p1,is)=1  ! for the same reason
         endif                                                           !
         !
         do im_index=1,im_max
           !
           if(ia==1) then
             im=im_index+spin_m_table(2,il,j2p1,is)
             m=im-il
             pp_factor(im,il,j2p1,is,1)=spinor_factor(m,l,j,1,psp_has_so)
             !
             im=im_index+spin_m_table(3,il,j2p1,is)
             m=im-il
             pp_factor(im,il,j2p1,is,2)=spinor_factor(m,l,j,2,psp_has_so)
             !
           endif
           !
           pp_kbv_dim=pp_kbv_dim+1
           !
         enddo
       enddo
     enddo
   enddo
 endif
 !
 if (io_KB_abinit_err==0) then
   !allocate(spin_m_table(3,pp_n_l_max,1,n_atomic_species))
   !spin_m_table=0
   !allocate(pp_factor(2*(pp_n_l_max-1)1,pp_n_l_max,1,n_atomic_species,2))
   !pp_factor=1._SP
   !
   do is = 1,n_atomic_species
     do ia = 1,n_atoms_species(is)
       do i_pp = 1, pp_n_l_times_proj_max
         il = pp_table(1,is,i_pp)
         if(il==0) cycle
         pp_spin = pp_table(3,is,i_pp)
         if(pp_spin==0) cycle
         if(pp_spin>1 ) cycle
         !spin_m_table(il,1,1)  = 2*(il-1)+1                 ! Spin-angle functions not used
         !spin_m_table(il,1,2:3)= 0                          ! spin_m_table and pp_factor are
         !pp_factor(:,il,1,is,:)=sqrt(4.*pi/(2.*(il-1.)+1.)) ! trivial. pp_factor used for 2*l1
         do im = 1,2*(il-1)+1
           pp_kbv_dim=pp_kbv_dim+1
         enddo
       enddo
     enddo
   enddo
 endif
 !
 io_err(1)=io_KB_abinit_err
 io_err(2)=io_KB_pwscf_err
 io_err(3)=io_Vnl_err
 !
 if ( pp_kbv_dim>0 ) call msg('s','[x,Vnl] computed using '//trim(intc(pp_kbv_dim))//' projectors')
 if ( pp_kbv_dim>50) call warning(' [x,Vnl] slows the Dipoles computation. To neglect it rename the ns.kb_pp file')
 !
 return
 !
 contains
   !
   real(SP) function spinor_factor(m,l,j,i_spinor,psp_has_so)
     !
     implicit none
     !
     integer :: m,l,i_spinor
     real(SP):: j
     logical :: psp_has_so
     !
     if(.not.psp_has_so) then
       spinor_factor=1._SP
       return
     endif
     !
     if(j>real(l,SP).and.i_spinor==1) spinor_factor= sqrt(real((l+m+1),SP)/real(2*l+1,SP))
     if(j>real(l,SP).and.i_spinor==2) spinor_factor= sqrt(real((l-m+1),SP)/real(2*l+1,SP))
     if(j<real(l,SP).and.i_spinor==1) spinor_factor= sqrt(real((l-m  ),SP)/real(2*l+1,SP))
     if(j<real(l,SP).and.i_spinor==2) spinor_factor=-sqrt(real((l+m  ),SP)/real(2*l+1,SP))
     !
   end function
   !
end subroutine

