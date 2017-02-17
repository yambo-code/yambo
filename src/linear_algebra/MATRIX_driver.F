!
!        Copyright (C) 2000-2016 the YAMBO team
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
subroutine MATRIX_driver(idriver,M,lib_in,blt,V_left,V_right,E_real,E_cmpl,SVD_digits,fill,det)
 !
 use pars,          ONLY:SP,cONE,cZERO
 use matrix_operate,ONLY:blacs_t,INV,DIAGO,USE_LK,USE_SVD,USE_SLK,min_cpu_block_size
 use wrapper,       ONLY:M_by_M,SV_decomposition,LU_factorization,LU_inversion,M_eigenvalues
 use parallel_m,    ONLY:PP_redux_wait,PAR_COM_INV,PAR_COM_DIAGO,mpi_comm_world
 !
 implicit none
 !
 integer                :: idriver
 complex(SP)            :: M(:,:)
 complex(SP),  optional :: V_left(:,:)
 complex(SP),  optional :: V_right(:,:)
 complex(SP),  optional :: E_cmpl(:)
 type(blacs_t),optional :: blt
 real(SP),     optional :: E_real(:)
 integer,      optional :: lib_in
 integer,      optional :: fill
 complex(SP),  optional :: det
 integer,      optional :: SVD_digits 
 !
 !Work Spaces ...
 !
 real(SP):: SV_min
 integer :: M_size,i1,lib_in_use,Ncpu_in_use,myID_in_use
 !
 ! ...serial invertion
 !
 integer       :: ifail,lwork
 integer, allocatable     :: ipvt(:)
 complex(SP), allocatable :: work(:)
 !
 ! ...additionals for serial diago
 !
 real(SP), allocatable :: rwk(:)
 !
 ! ...SVD
 !
 real(SP),    allocatable :: SV(:),r_WK(:)
 complex(SP), allocatable :: U(:,:),VH(:,:)
 !
 ! M dimensions and initialization
 !
 M_size=size(M,2)
 !
 Ncpu_in_use=1
 myID_in_use=0
 !
 ! Decide which to use among LK or SLK
 !
 lib_in_use=USE_LK
 if (present(lib_in)) then
    lib_in_use=lib_in
 endif
 !
 if (Ncpu_in_use==1.or.M_size/Ncpu_in_use<min_cpu_block_size.or.idriver==USE_SVD) lib_in_use=USE_LK 
 !
#if defined _MPI
 !
 ! CPUs (number and ID) 
 !
 if (lib_in_use==USE_SLK) then
   if (idriver==INV) then
     Ncpu_in_use=PAR_COM_INV%n_CPU
     myID_in_use=PAR_COM_INV%CPU_id
   else if (idriver==DIAGO) then
     Ncpu_in_use=PAR_COM_DIAGO%n_CPU
     myID_in_use=PAR_COM_DIAGO%CPU_id
   endif 
 endif
 !
#endif
 !
 ! =============== SERIAL SECTION ===============
 !
 !
 ! ####> SVD INVERSION (Using Singular Value decomposition)
 !
 if (idriver==USE_SVD.and.lib_in_use==USE_LK) then
   allocate(r_WK(5*M_size),SV(M_size),U(M_size,M_size),VH(M_size,M_size))
   allocate(work(1))
   call SV_decomposition(M_size,M,SV,U,VH,work,-1,r_WK,ifail)
   !
   if(ifail/=0) call report_diago_error('GESVD (SVD)','workspace failed')
   !
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
   call SV_decomposition(M_size,M,SV,U,VH,work,lwork,r_WK,ifail)
   !
   if(ifail/=0) call report_diago_error('GESVD (SVD)','failed')
   !
   SV_min=10._SP**(-SVD_digits)
   !
   do i1=1,M_size
     if (abs(SV(i1))<SV_min) then
       VH(i1,:)=cZERO
     else
       VH(i1,:)=VH(i1,:)/SV(i1)
     endif
   enddo
   !
   call M_by_M('c','c',M_size,VH,U,M)
   !
   deallocate(r_WK,SV,U,VH,work)
   !
   return
   !
 endif
 !
 ! ###> SERIAL INVERSION (standard LK)
 !
 if (idriver==INV.and.lib_in_use==USE_LK) then
   allocate(ipvt(M_size))
   call LU_factorization(M_size,M,ipvt,ifail)
   !
   if(ifail.ne.0) call report_diago_error('GETRF (SI)','workspace failed')
   !
   ! Evaluate determinant if required
   !
   if(present(det)) then
     det=cONE
     do i1=1,M_size
       det=det*M(i1,i1)
       if(ipvt(i1).ne.i1) det=-det
     enddo
   endif
   !
   allocate(work(1))
   call LU_inversion(M_size,M,ipvt,work,-1,ifail)
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
   call LU_inversion(M_size,M,ipvt,work,lwork,ifail)
   !
   if(ifail.ne.0) call report_diago_error('GETRI (SI)','failed')
   !
   deallocate(work,ipvt)
   return
   !
 endif
 !
 ! ###> SERIAL DIAGONALIZATION (Non-Hermitian Matrices)
 !
 !  The right eigenvector v(j) of A satisfies
 !                   A * v(j) = lambda(j) * v(j)
 !  where lambda(j) is its eigenvalue.
 !  The left eigenvector u(j) of A satisfies
 !                u(j)**H * A = lambda(j) * u(j)**H
 !  where u(j)**H denotes the conjugate transpose of u(j).
 !
 if (idriver==DIAGO.and.lib_in_use==USE_LK.and.present(E_cmpl)) then
   !
   allocate(work(1),rwk(2*M_size))
   call M_eigenvalues(M_size,M,E_cmpl,V_left,V_right,work,-1,rwk,ifail)
   !
   if(ifail.ne.0) call report_diago_error('M_eigenvalues Hermitian (SD)','workspace failed')
   !
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
   call M_eigenvalues(M_size,M,E_cmpl,V_left,V_right,work,lwork,rwk,ifail)
   !
   if(ifail.ne.0) call report_diago_error('M_eigenvalues Hermitian (SD)','failed')
   !
   deallocate(work,rwk)
   return
 endif
 !
 ! ###> SERIAL DIAGONALIZATION (Hermitian Matrices)
 !
 if (idriver==DIAGO.and.lib_in_use==USE_LK) then
   !
   allocate(work(1),rwk(max(1,3*M_size-2)))
   call M_eigenvalues(M_size,M,E_real,work,-1,rwk,ifail)
   !
   if(ifail.ne.0) call report_diago_error('M_eigenvalues (SD)','workspace failed')
   !
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
   call M_eigenvalues(M_size,M,E_real,work,lwork,rwk,ifail)
   !
   if(ifail.ne.0) call report_diago_error('M_eigenvalues (SD)','failed')
   !
   deallocate(work,rwk)
   return
 endif
 !
 contains  
   !
   integer function BF_compute(dim)
     integer :: dim
     real(SP), parameter :: m_factors(4)=(/3.,12.,16.,32./)
     integer , parameter :: bf_defs(5)=(/1,4,16,32,64/)
     real(SP):: log_dim 
     log_dim=log10(real(dim))
     if (0.<=log_dim.and.log_dim<1.) BF_compute=bf_defs(1)+int(log_dim*m_factors(1))
     if (1.<=log_dim.and.log_dim<2.) BF_compute=bf_defs(2)+int((log_dim-1.)*m_factors(2))
     if (2.<=log_dim.and.log_dim<3.) BF_compute=bf_defs(3)+int((log_dim-2.)*m_factors(3))
     if (3.<=log_dim.and.log_dim<4.) BF_compute=bf_defs(4)+int((log_dim-3.)*m_factors(4))
     if (4.<=log_dim               ) BF_compute=bf_defs(5)
   end function
   !
   subroutine v2v_blt(vin,vout)
     type(blacs_t) :: vin,vout
     vout%ictxt=vin%ictxt
     vout%nprow=vin%nprow
     vout%npcol=vin%npcol
     vout%myrow=vin%myrow
     vout%mycol=vin%mycol
     vout%descM =vin%descM 
     vout%descV=vin%descV
     vout%ldrow=vin%ldrow
     vout%ldcol=vin%ldcol
     vout%blocking_factor=vin%blocking_factor
   end subroutine
   !
   subroutine report_diago_error(calling_subr,message_)
     use stderr,  ONLY:string_pack
     use com,     ONLY:error
     character(*) calling_subr,message_
     call error( trim( string_pack('Matrix Ops. driver [',calling_subr,']:',message_) ))
   end subroutine
   !
end subroutine
