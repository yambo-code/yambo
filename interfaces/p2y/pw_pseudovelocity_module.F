! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

!#include "tools_error.h"

module pw_pseudovelocity_module
use pars, ONLY:SP
use pw_basis_module
use pw_wfc_module
implicit none

private

public :: pw_pseudovelocity, &
          pw_pseudovelocity_init, &
          pw_pseudovelocity_destroy, &
          pw_pseudovelocity_apply, &
          pw_pseudovelocity_braket

integer, parameter :: lmax = 3

type pw_pseudovelocity
  type(pw_basis), pointer :: basis
  integer                 :: nproj
  type(pw_wfc),   pointer :: proj(:)
  type(pw_wfc),   pointer :: dproj(:,:)
  real(SP),           pointer :: d(:)
end type pw_pseudovelocity

contains

subroutine pw_pseudovelocity_init(velocity,basis,atoms)
  use pw_atoms_module
  use numerical_module
  use num_interpolation_module
  use pw_pseudo_module
  use electrons,     ONLY:n_spinor
! use tools_module
  type(pw_pseudovelocity), intent(out) :: velocity(n_spinor)
  type(pw_basis), target, intent(in) :: basis
  type(pw_atoms), intent(in) :: atoms
  type(pw_pseudo), pointer :: pseudo
  integer :: nproj,iatom,ibeta,iproj(2),nbeta,ipw,npw,l,m,mmin,mmax,i_spinor
  real(SP)    :: j,spinor_factor
  complex(SP) :: tmp,tmp_vec(3)
  
  real(SP) :: b(3,3),kg(3),pos(3)
  real(SP) :: q(3,basis%npw),modq(basis%npw)
  complex(SP) :: struct_fact(basis%npw)
  complex(SP), allocatable :: xlylm(:,:,:)
  complex(SP), allocatable :: dxlylm(:,:,:,:)
  real(SP)    :: fl(basis%npw)
  real(SP)    :: dfl(3,basis%npw)

! write(*,*) "pseudovel_init: start",lmax
  allocate(xlylm(basis%npw,-lmax:lmax,0:lmax),dxlylm(3,basis%npw,-lmax:lmax,0:lmax))
  do i_spinor=1,n_spinor
    velocity(i_spinor)%basis => basis
  enddo
  b = basis%struct%b
  npw = basis%npw
  do ipw=1,npw
    kg = basis%k + real(basis%g(:,ipw),SP)   
!   q(:,ipw) = num_matmul(b,kg) !! CHECK
    q(:,ipw) = matmul(b,kg)
    modq(ipw) = sqrt(sum(q(:,ipw)**2))
  end do
  xlylm = 0.0
  do l=0,lmax
    do m=-l,l
      do ipw=1,npw
        xlylm(ipw,m,l) = num_xlylm(q(:,ipw),l,m)
        dxlylm(:,ipw,m,l) = num_xlylm_grad(q(:,ipw),l,m)
      end do
    end do
  end do
  iproj = 0
  do iatom=1,atoms%natoms
    pseudo => atoms%pseudo(atoms%type_map(iatom))
    iproj = iproj + sum(2*pseudo%lbeta(:)+1)
    if (n_spinor==2) iproj=iproj+count(pseudo%lbeta(:)==0)
  end do
! write(*,*) "pseudovel_init: velocity"
  nproj=iproj(1)
  do i_spinor=1,n_spinor
    velocity(i_spinor)%nproj = nproj
    allocate(velocity(i_spinor)%proj(nproj))
    allocate(velocity(i_spinor)%dproj(3,nproj))
    call pw_wfc_init(velocity(i_spinor)%proj,basis)
    call pw_wfc_init(velocity(i_spinor)%dproj,basis)
    allocate(velocity(i_spinor)%d(nproj))
  enddo
  iproj=0
! write(*,*) "pseudovel_init: loops atoms",1,atoms%natoms
  do iatom=1,atoms%natoms
    pseudo => atoms%pseudo(atoms%type_map(iatom))
    nbeta = pseudo%nbeta
    pos = atoms%positions(:,iatom)
    do ipw=1,npw
      struct_fact(ipw) = exp(-num_2pi_i*dot_product(real(basis%g(:,ipw),SP),pos))
    end do
!   write(*,*) "pseudovel_init: loops beta",1,nbeta
    do ibeta=1,nbeta
      do ipw=1,npw
!       write(*,*) "pseudovel_init: num calc"
        fl(ipw) = num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3)
!       write(*,*) "pseudovel_init: num calc done"
        if(modq(ipw) < 0.0000001) then
          dfl(:,ipw) = q(:,ipw) * 0.5 * &
                  num_interpolation_calc_der(pseudo%interpolation(ibeta),modq(ipw),3,ider=2)
        else
          dfl(:,ipw) = q(:,ipw)/modq(ipw) * &
                  num_interpolation_calc_der(pseudo%interpolation(ibeta),modq(ipw),3,ider=1)
        end if
      end do
      l = pseudo%lbeta(ibeta)
      j=0
      if(pseudo%psp_has_so) j = pseudo%jbeta(ibeta)
      do i_spinor=1,n_spinor
        if(.not.pseudo%psp_has_so) then
          mmin=-l
          mmax= l
        else
          if(j>l) then
            mmin=-l-1+(i_spinor-1)
            mmax= l+(i_spinor-1)
          elseif(j<l) then
            mmin=-l+(i_spinor-1)
            mmax= l-1+(i_spinor-1)
          endif
        endif
        do m=mmin,mmax
          iproj(i_spinor) = iproj(i_spinor) + 1
          velocity(i_spinor)%d(iproj(i_spinor)) = pseudo%d(ibeta)
          if(abs(m)>l) then
            do ipw=1,npw
              velocity(i_spinor)%proj(iproj(i_spinor))%val(ipw) = 0.
              velocity(i_spinor)%dproj(1,iproj(i_spinor))%val(ipw) = 0.
              velocity(i_spinor)%dproj(2,iproj(i_spinor))%val(ipw) = 0.
              velocity(i_spinor)%dproj(3,iproj(i_spinor))%val(ipw) = 0.
            enddo
            cycle
          endif
          if(.not.pseudo%psp_has_so) then
            spinor_factor=1.
          else
            if(j>l.and.i_spinor==1) spinor_factor= sqrt(real((l+m+1))/real(2*l+1))
            if(j>l.and.i_spinor==2) spinor_factor= sqrt(real((l-m+1))/real(2*l+1))
            if(j<l.and.i_spinor==1) spinor_factor= sqrt(real((l-m  ))/real(2*l+1))
            if(j<l.and.i_spinor==2) spinor_factor=-sqrt(real((l+m  ))/real(2*l+1))
          endif
          ! componente normale
          do ipw=1,npw
            tmp = xlylm(ipw,m,l) * &
                num_interpolation_calc(pseudo%interpolation(ibeta),modq(ipw),3) * &
                struct_fact(ipw)
            tmp=tmp*spinor_factor*(0.0,-1.0)**l
            velocity(i_spinor)%proj(iproj(i_spinor))%val(ipw) = tmp
          end do
          ! componenti derivate
          do ipw=1,npw
            tmp_vec = struct_fact(ipw) * &
                      (xlylm(ipw,m,l) * dfl(:,ipw) + dxlylm(:,ipw,m,l) * fl(ipw))
            tmp_vec=tmp_vec*spinor_factor*(0.0,-1.0)**l
            velocity(i_spinor)%dproj(1,iproj(i_spinor))%val(ipw) = tmp_vec(1)
            velocity(i_spinor)%dproj(2,iproj(i_spinor))%val(ipw) = tmp_vec(2)
            velocity(i_spinor)%dproj(3,iproj(i_spinor))%val(ipw) = tmp_vec(3)
          enddo
        end do
      end do
    end do
  end do
  if(any(iproj(:n_spinor) /= nproj)) call errore("pw_pseudovelocity_init","iproj ne nproj",1)
  deallocate(xlylm,dxlylm)
end subroutine pw_pseudovelocity_init

 subroutine pw_pseudovelocity_apply(wfc_new,velocity,wfc) 
   type (pw_wfc),   intent(inout) :: wfc_new(3)
   type (pw_pseudovelocity), intent(in)    :: velocity
   type (pw_wfc),   intent(in)    :: wfc
   integer :: iproj
   type (pw_wfc) :: wfc_tmp(3)
   type (pw_wfc) :: wfc_dtmp(3)
   complex(SP) :: projection(velocity%nproj)
   complex(SP) :: dprojection(3,velocity%nproj)
!
   real(SP) :: d

   do iproj=1,velocity%nproj
     d = velocity%d(iproj) / wfc_new(1)%basis%struct%a_omega
     projection(iproj) = pw_wfc_braket(velocity%proj(iproj),wfc) * d
     dprojection(1,iproj) = pw_wfc_braket(velocity%dproj(1,iproj),wfc) * d
     dprojection(2,iproj) = pw_wfc_braket(velocity%dproj(2,iproj),wfc) * d
     dprojection(3,iproj) = pw_wfc_braket(velocity%dproj(3,iproj),wfc) * d
   end do
!  write(*,*) "VEL: ",d,velocity%nproj,"|",velocity%d,"|",projection,"|",velocity%proj(1)%val(1),wfc%val(1)
   call pw_wfc_init(wfc_tmp,velocity%basis)
   call pw_wfc_init(wfc_dtmp,velocity%basis)
   wfc_new(1)%val = 0.0
   wfc_new(2)%val = 0.0
   wfc_new(3)%val = 0.0
   do iproj=1,velocity%nproj
     wfc_tmp(1) = velocity%proj(iproj)
     wfc_tmp(2) = velocity%proj(iproj)
     wfc_tmp(3) = velocity%proj(iproj)
     wfc_dtmp(:) = velocity%dproj(:,iproj)
     call pw_wfc_scale(wfc_tmp(1),dprojection(1,iproj))
     call pw_wfc_scale(wfc_tmp(2),dprojection(2,iproj))
     call pw_wfc_scale(wfc_tmp(3),dprojection(3,iproj))
     call pw_wfc_scale(wfc_dtmp(1),projection(iproj))
     call pw_wfc_scale(wfc_dtmp(2),projection(iproj))
     call pw_wfc_scale(wfc_dtmp(3),projection(iproj))
     wfc_new(1)%val = wfc_new(1)%val + wfc_tmp(1)%val + wfc_dtmp(1)%val
     wfc_new(2)%val = wfc_new(2)%val + wfc_tmp(2)%val + wfc_dtmp(2)%val
     wfc_new(3)%val = wfc_new(3)%val + wfc_tmp(3)%val + wfc_dtmp(3)%val
   end do
   call pw_wfc_destroy(wfc_tmp)
   call pw_wfc_destroy(wfc_dtmp)
   return
 end subroutine pw_pseudovelocity_apply

subroutine pw_pseudovelocity_destroy(velocity)
  type (pw_pseudovelocity), intent(inout) :: velocity
  call pw_wfc_destroy(velocity%proj)
  call pw_wfc_destroy(velocity%dproj)
  deallocate(velocity%proj)
  deallocate(velocity%dproj)
  deallocate(velocity%d)
end subroutine pw_pseudovelocity_destroy

 function pw_pseudovelocity_braket(velocity,bra,ket)
   type (pw_pseudovelocity), intent(in) :: velocity
   type (pw_wfc), intent(in) :: bra,ket
   complex(SP) :: pw_pseudovelocity_braket(3)
   type (pw_wfc) :: wfc_Vnl_ck(3)
 
!  write(*,*) "Start pw_pseudovelocity_braket"
   !
   ! Initialize |wfc_tmp>
   !
   call pw_wfc_init(wfc_Vnl_ck,bra%basis)
!  write(*,*) "Start pw_pseudovelocity_apply"
   !
   ! Here operate |wfc_tmp> = Vnl|ck>
   !
   call pw_pseudovelocity_apply(wfc_Vnl_ck,velocity,ket)
!  write(*,*) "Do pseudo braket"
   !
   ! Here compute <vk|Vnl|ck> = <wfc_val|wfc_tmp>
   !
   pw_pseudovelocity_braket(1) = pw_wfc_braket(bra,wfc_Vnl_ck(1))
   pw_pseudovelocity_braket(2) = pw_wfc_braket(bra,wfc_Vnl_ck(2))
   pw_pseudovelocity_braket(3) = pw_wfc_braket(bra,wfc_Vnl_ck(3))
   call pw_wfc_destroy(wfc_Vnl_ck)
 end function pw_pseudovelocity_braket
!
end module pw_pseudovelocity_module

!call      pw_pseudovelocity_braket(pseudovelocity,wfc_val, wfc_con)
! function pw_pseudovelocity_braket(velocity,      bra,     ket)
!      call pw_pseudovelocity_apply(wfc_tmp,velocity,ket)
