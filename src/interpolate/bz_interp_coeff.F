!
! Copyright (C) 2000-2010 C. Attaccalite and the YAMBO team 
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
subroutine bz_interp_coeff(Xk,VALUEs,engre,nvalues)
  ! Interpolation scheme (PRB 38 p2721)
  ! Aug2002 Georg Madsen : First version based on subroutine from D.Singh
  ! Apr2004 Georg Madsen : blocked dgemm version
  ! Code take from BoltzTrap
  ! http://www.icams.de/content/departments/ams/madsen/boltztrap.html
  !
  use pars,           ONLY:SP,DP,pi,cI,IP,cZERO,cONE
  use interpolate,    ONLY:nshells,lattice_vectors,make_star,metric,int_sop
  use D_lattice,      ONLY:nsym,dl_sop,a
  use R_lattice,      ONLY:bz_samp,b
  use memory_m,       ONLY:mem_est
  use vec_operate,    ONLY:c2a
  use com,            ONLY:error
  use wrapper,        ONLY:M_by_M,M_by_V
  !
  implicit none
  !
  integer,       intent(in)  :: nvalues
  type(bz_samp), intent(in)  :: Xk
  real(SP),      intent(in)  :: VALUEs(nvalues,Xk%nibz)
  complex(DP),   intent(out) :: engre(nshells,nvalues)
  !
  ! Work Space
  !
  real(DP),    allocatable :: rho(:)
  complex(SP), allocatable :: h_mat(:,:),delta(:,:)
  complex(DP), allocatable :: star_1(:,:),star_2(:,:)
  real(DP)  :: R2_min,R2,roughness_func
  real(DP), parameter :: twopi=2._SP*pi
  real(SP)  :: v(3),star_vec(3,nsym)
  integer   :: ik,ik2,iv,i1,i2,nstar,nkm1,info,ikn,ikibz,nkpt
  integer, allocatable :: ipiv(:)
  !
  nkpt=Xk%nibz             ! number of k-points in the BZ
  nkm1=Xk%nibz-1           ! number of k-points minus 1 in the BZ
  ikn =Xk%nibz             ! last k-point index
  !
  allocate(delta(nkm1,nvalues))
  call mem_est("delta",(/size(delta)/),(/SP/))
  allocate(rho(nshells))
  call mem_est("rho",(/nshells/),(/SP/))
  allocate(h_mat(nkm1,nkm1),star_1(nshells,nkpt),star_2(nshells,nkm1))
  call mem_est("h_mat star_1 star_2",(/size(h_mat),size(star_1),size(star_2)/),(/2*SP,2*SP,2*SP/))
  allocate(ipiv(nkm1))
  call mem_est("ipiv",(/size(ipiv)/),(/IP/))
  !
  ! Construct delta_epsilon eq. 10
  !
  delta=cZERO
  !
  do ik=1,nkm1
    delta(ik,:)=VALUEs(:,ik)-VALUEs(:,ikn)
  enddo
  !
  rho(1)=0._DP
  R2_min=dot_product(lattice_vectors(:,2),matmul(metric,lattice_vectors(:,2)))
  !
  do i1=2,nshells
    R2=dot_product(lattice_vectors(:,i1),matmul(metric,lattice_vectors(:,i1)))
    rho(i1)=roughness_func(R2,R2_min)
  enddo
  !
  h_mat =cZERO
  star_1=cZERO
  star_2=cZERO
  !
  do i1=2,nshells
    call make_star(lattice_vectors(:,i1),nsym,int_sop,nstar,star_vec) 
    do ik=1,nkpt
      call c2a(v_in=Xk%pt(ik,:),v_out=v,mode='ki2a')
      do i2=1,nstar
        star_1(i1,ik)=star_1(i1,ik) + exp(cI*twopi*dot_product(v(:),star_vec(:,i2)))
      enddo
    enddo
    star_1(i1,:)=star_1(i1,:)/real(nstar)
  enddo
  !
  do ik=1,nkm1
    star_1(2:nshells,ik) = star_1(2:nshells,ik) - star_1(2:nshells,nkpt)
  enddo
  !
  do ik=1,nkm1
    star_2(2:nshells,ik)=conjg(star_1(2:nshells,ik))/rho(2:nshells)
  enddo
  !
  do ik=1,nkm1
    do ik2=ik,nkm1
      h_mat(ik,ik2)=sum(star_1(2:nshells,ik)*star_2(2:nshells,ik2))
      h_mat(ik2,ik)=conjg(h_mat(ik,ik2))
    enddo
  enddo
  !
#if defined _DOUBLE
  call ZGETRF(nkm1,nkm1,h_mat,nkm1,ipiv,info)
#else
  call CGETRF(nkm1,nkm1,h_mat,nkm1,ipiv,info)
#endif
  !
  if(info/=0) call error("[Interp] Error in factorization ")
  !
#if defined _DOUBLE
  call  ZGETRS('N',nkm1,nvalues,h_mat,nkm1,ipiv,delta,nkm1,info)
#else
  call  CGETRS('N',nkm1,nvalues,h_mat,nkm1,ipiv,delta,nkm1,info)
#endif
  !
  if(info/=0) call error("[Interp] Error in -getrs")
  !
  engre=cZERO
  !
  forall(i1=2:nshells,iv=1:nvalues)
    engre(i1,iv)=sum(delta(1:nkm1,iv)*star_2(i1,1:nkm1))
  end forall
  !
  do iv=1,nvalues
    engre(1,iv)=VALUEs(iv,ikn) - sum(engre(2:nshells,iv)*star_1(2:nshells,nkpt))
  enddo
  !
  ! Deallocation
  !
  deallocate(h_mat,star_1,star_2)
  call mem_est("h_mat star_1 star_2")
  deallocate(ipiv)
  call mem_est("ipiv")
  deallocate(delta,rho)
  call mem_est("delta rho")
  !
  call k_compare(Xk,VALUEs,engre,nshells,nvalues)
  !
end subroutine bz_interp_coeff

pure real(DP) function roughness_func(R2,R2_min)
  use pars,           ONLY:DP
  implicit none
  real(DP),  intent(in) :: R2,R2_min
  real(DP),  parameter  :: C1=0.75_DP,C2=0.75_DP
  !
  roughness_func=(1._DP-C1*R2/R2_min)**2 + C2*(R2/R2_min)**3
  !
end function roughness_func

subroutine k_compare(Xk,VALUEs,engre,nshells,nvalues)
  use units,          ONLY:HA2EV
  use pars,           ONLY:SP,DP,schlen
  use R_lattice,      ONLY:bz_samp
  use interpolate,    ONLY:lattice_vectors
  use com,            ONLY:warning,msg
  !
  implicit none
  type(bz_samp), intent(in) :: Xk
  integer,       intent(in) :: nvalues,nshells
  real(SP),      intent(in) :: VALUEs(nvalues,Xk%nibz)
  complex(DP),   intent(in) :: engre(nshells,nvalues)
  !
  ! Work Space
  !
  integer :: ik,iv
  character(schlen)     :: dump_ch
  real(SP), allocatable :: new_VALUEs(:,:)
  real(SP) :: ave_err,max_err
  !
  allocate(new_VALUEs(nvalues,Xk%nibz))
  !
  call fourier_interpolation(Xk,new_VALUEs,engre,nvalues)
  !
  max_err=0._SP
  ave_err=0._SP
  !
  do ik=1,Xk%nibz
    do iv=1,nvalues
        ave_err=ave_err+abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))
        if(abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))>max_err) max_err=abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))
     enddo
  enddo
  !
  ave_err=ave_err/real(nvalues*Xk%nibz)
  !
  write(dump_ch,'(a,f16.8,a,f16.8)') '[Interp] Max error = ',max_err,' average error = ',ave_err
  call msg('s',dump_ch)
  !
  deallocate (new_VALUEs)
end subroutine k_compare
