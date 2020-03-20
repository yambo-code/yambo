!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): CA, DS
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
subroutine INTERPOLATION_BZ_coeff(Xk,VALUEs,engre,nvalues,Nk,ID_obj)
 !
 ! Interpolation scheme (PRB 38 p2721)
 ! Aug2002 Georg Madsen : First version based on subroutine from D.Singh
 ! Apr2004 Georg Madsen : blocked dgemm version
 ! Code take from BoltzTrap
 !
 ! https://www.imc.tuwien.ac.at//forschungsbereich_theoretische_chemie/forschungsgruppen/prof_dr_gkh_madsen_theoretical_materials_chemistry/boltztrap/
 !
 use pars,              ONLY:SP,DP,pi,cI,cZERO
 use interpolate,       ONLY:nshells,lattice_vectors,metric,int_sop,INTERP_obj,INTERPOLATE_is_quiet,INTERPOLATE_is_serial
 use interpolate_tools, ONLY:make_star
 use D_lattice,         ONLY:nsym
 use R_lattice,         ONLY:bz_samp
 use vec_operate,       ONLY:c2a
 use LIVE_t,            ONLY:live_timing
 use interfaces,        ONLY:PARALLEL_index
 use parallel_m,        ONLY:PP_redux_wait,PP_indexes,PP_indexes_reset,myid
 !
#include<memory.h>
 !
 integer,       intent(in)  :: nvalues
 integer,       intent(in)  :: Nk,ID_obj
 type(bz_samp), intent(in)  :: Xk
 real(SP),      intent(in)  :: VALUEs(nvalues,Nk)
 complex(DP),   intent(out) :: engre(nshells,nvalues)
 !
 ! Work Space
 !
 type(PP_indexes)  :: px
 real(DP)          :: R2_min,R2
 real(SP)          :: v(3),star_vec(3,nsym)
 integer           :: ik,ik2,ik_prod,iv,i1,i2,nstar,Nkm1,info,ikn,Npx(1),Nlive
 !
 ! Parameters
 !
 real(DP), parameter :: twopi=2._SP*pi
 !
 ! Allocatable
 !
 real(DP),    allocatable :: rho(:)
 complex(SP), allocatable :: h_mat(:,:),delta(:,:)
 complex(DP), allocatable :: star_1(:,:)
 integer,     allocatable :: ipiv(:)
 !
 ! External
 !
 complex(DP) :: ZDOTC,ZDOTU
 !
 Nkm1=Nk-1           ! number of k-points minus 1 in the BZ
 ikn =Nk             ! last k-point index
 !
 YAMBO_ALLOC(delta,(Nkm1,nvalues))
 YAMBO_ALLOC(rho,(nshells))
 YAMBO_ALLOC(h_mat,(Nkm1,Nkm1))
 YAMBO_ALLOC(star_1,(nshells,Nk))
 YAMBO_ALLOC(ipiv,(Nkm1))
 !
 ! Construct delta_epsilon eq. 10
 !
 delta=cZERO
 !
 do ik=1,Nkm1
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
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing("Definition of star_1",nshells-1)
 !
 do i1=2,nshells
   call make_star(lattice_vectors(:,i1),nsym,int_sop,nstar,star_vec) 
   do ik=1,Nk
     if(Nk==Xk%nibz) call c2a(v_in=Xk%pt(ik,:),v_out=v,mode='ki2a')
     ! Implementation to be finished. Here I canno use BZ_DbGd_kpts
     ! bacuse also extra points in the BZ are included ...
     !if(Nk> Xk%nibz) call c2a(v_in=Xk%pt_DbGd(ik,:),v_out=v,mode='ki2a')
     do i2=1,nstar
       star_1(i1,ik)=star_1(i1,ik) + exp(cI*twopi*dot_product(v(:),star_vec(:,i2)))
     enddo
   enddo
   if (.not.INTERPOLATE_is_quiet) call live_timing(steps=1)
   star_1(i1,:)=star_1(i1,:)/real(nstar)
 enddo
 if (.not.INTERPOLATE_is_quiet) call live_timing()
 !
 do ik=1,Nkm1
   star_1(2:nshells,ik) = star_1(2:nshells,ik) - star_1(2:nshells,Nk)
   star_1(2:nshells,ik) = star_1(2:nshells,ik) / sqrt(rho(2:nshells))
 enddo
 !
 ! Par Proc
 !
 Npx=(Nkm1*(Nkm1+1))/2
 Nlive=Npx(1)
 if (.not.INTERPOLATE_is_serial) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,Npx)
   Nlive=px%n_of_elements(myid+1)
 endif
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing("Computing h_mat",Nlive)
 !
 do ik=1,Nkm1
   do ik2=ik,Nkm1
     ik_prod=ik2-ik+1+(ik-1)*(Nkm1-ik+2)
     if (.not.INTERPOLATE_is_serial) then
       if (.not.px%element_1D(ik_prod)) cycle
     endif
     if(ik2==ik) h_mat(ik,ik)=real(ZDOTC(nshells-1,star_1(2:nshells,ik),1,star_1(2:nshells,ik),1),SP)
     if(ik2/=ik) then
       h_mat(ik,ik2)=cmplx(ZDOTC(nshells-1,star_1(2:nshells,ik2),1,star_1(2:nshells,ik),1),kind=SP)
       h_mat(ik2,ik)=conjg(h_mat(ik,ik2))
     endif
     if (.not.INTERPOLATE_is_quiet) call live_timing(steps=1)
   enddo
 enddo
 !
 if (.not.INTERPOLATE_is_serial) call PP_redux_wait(h_mat)
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing()
 !
 do ik=1,Nkm1
   star_1(2:nshells,ik) = star_1(2:nshells,ik) * sqrt(rho(2:nshells))
 enddo
 !
#if defined _DOUBLE
 call ZGETRF(Nkm1,Nkm1,h_mat,Nkm1,ipiv,info)
#else
 call CGETRF(Nkm1,Nkm1,h_mat,Nkm1,ipiv,info)
#endif
 !
 if(info/=0) call error("[INTERPOLATION] Error in factorization ")
 !
#if defined _DOUBLE
 call  ZGETRS('N',Nkm1,nvalues,h_mat,Nkm1,ipiv,delta,Nkm1,info)
#else
 call  CGETRS('N',Nkm1,nvalues,h_mat,Nkm1,ipiv,delta,Nkm1,info)
#endif
 !
 if(info/=0) call error("[INTERPOLATION] Error in -getrs")
 !
 ! Par Proc
 !
 Nlive=nshells
 if (.not.INTERPOLATE_is_serial) then
   call PP_indexes_reset(px)
   call PARALLEL_index(px,(/nshells/))
   Nlive=px%n_of_elements(myid+1)
 endif
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing("Computing engre",Nlive)
 !
 engre=cZERO
 !
 do i1=2,nshells
   if (.not.INTERPOLATE_is_serial) then
     if (.not.px%element_1D(i1)) cycle
   endif
   do iv=1,nvalues
     engre(i1,iv)= ZDOTC(Nkm1,star_1(i1,1:Nkm1)/rho(i1),1,cmplx(delta(1:Nkm1,iv),kind=DP),1)
   enddo
   if (.not.INTERPOLATE_is_quiet) call live_timing(steps=1)
 enddo
 !
 if (.not.INTERPOLATE_is_serial) call PP_redux_wait(engre)
 !
 if (.not.INTERPOLATE_is_quiet) call live_timing()
 !
 do iv=1,nvalues
   engre(1,iv)=VALUEs(iv,ikn) - ZDOTU(nshells-1,engre(2:nshells,iv),1,star_1(2:nshells,Nk),1)
 enddo
 !
 ! Deallocation
 !
 YAMBO_FREE(h_mat)
 YAMBO_FREE(star_1)
 YAMBO_FREE(ipiv)
 YAMBO_FREE(delta)
 YAMBO_FREE(rho)
 !
 call k_compare(Xk,VALUEs,engre,nshells,nvalues,Nk)
 !
 contains
  !
  pure real(DP) function roughness_func(R2,R2_min)
   real(DP),  intent(in) :: R2,R2_min
   real(DP),  parameter  :: C1=0.75_DP,C2=0.75_DP
   !
   roughness_func=(1._DP-C1*R2/R2_min)**2 + C2*(R2/R2_min)**3
   !
  end function roughness_func
  !
  subroutine k_compare(Xk,VALUEs,engre,nshells,nvalues,Nk)
   use units,          ONLY:HA2EV
   use pars,           ONLY:SP,lchlen
   use R_lattice,      ONLY:bz_samp
   use com,            ONLY:msg
   !
   type(bz_samp), intent(in) :: Xk
   integer,       intent(in) :: nvalues,nshells,Nk
   real(SP),      intent(in) :: VALUEs(nvalues,Nk)
   complex(DP),   intent(in) :: engre(nshells,nvalues)
   !
   ! Work Space
   !
   integer :: ik,iv
   character(lchlen)     :: dump_ch
   real(SP), allocatable :: new_VALUEs(:,:)
   real(SP) :: ave_err,max_err,rel_ave_err
   !
   YAMBO_ALLOC(new_VALUEs,(nvalues,Nk))
   !
   call INTERPOLATION_fourier(Xk,new_VALUEs,engre,nvalues,Nk)
   !
   max_err=0._SP
   ave_err=0._SP
   rel_ave_err=0._SP
   !
   do ik=1,Nk
     do iv=1,nvalues
       ave_err=ave_err+abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))
       if(abs(new_VALUEs(iv,ik))>0._SP) rel_ave_err=rel_ave_err+abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))/abs(new_VALUEs(iv,ik))
       if(abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))>max_err) max_err=abs(new_VALUEs(iv,ik)-VALUEs(iv,ik))
     enddo
   enddo
   !
   ave_err=ave_err/real(nvalues*Nk)
   rel_ave_err=rel_ave_err/real(nvalues*Nk)
   !
   write(dump_ch,'(a,3es16.8)') '[INTERPOLATION@'//trim(INTERP_obj(ID_obj)%what)//&
&                               '] Errors: Max, Avg, Avg_rel = ',max_err,ave_err,rel_ave_err
   call msg('s',dump_ch)
   !
   deallocate (new_VALUEs)
   !
  end subroutine k_compare 
  !
end subroutine INTERPOLATION_BZ_coeff
