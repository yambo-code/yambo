!
!        Copyright (C) 2000-2015 the YAMBO team
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
subroutine MATRIX_driver(idriver,M,lib_in,blt,V_left,V_right,E_real,E_cmpl,SVD_digits,fill)
 !
 use pars,          ONLY:SP
 use matrix_operate,ONLY:blacs_t,INV,DIAGO,USE_LK,USE_SVD,USE_SLK,min_cpu_block_size
 use wrapper,       ONLY:M_by_M
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
#if defined _SCALAPACK
 !
 ! ...parallel inversion
 !
 complex(SP), allocatable :: lM(:,:) 
 integer                  :: i, j, i_loc, j_loc, liwork
 type(blacs_t)            :: iblt
 !
 ! ...parallel diagonalization
 !
 complex(SP), allocatable :: lV(:,:)
 integer,     allocatable :: iwork(:)
 integer      :: numroc, indxl2g,rwork
 character(1) :: top=' '
 !
#endif
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
#if defined _SCALAPACK 
 lib_in_use=USE_SLK
#endif
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
#if defined _DOUBLE
   call ZGESVD('S','A',M_size,M_size,M,M_size,SV,U,M_size,VH,M_size,work,-1,r_WK,ifail)
#else
   call CGESVD('S','A',M_size,M_size,M,M_size,SV,U,M_size,VH,M_size,work,-1,r_WK,ifail)
#endif
   !
   if(ifail/=0) call report_diago_error('GESVD (SVD)','workspace failed')
   !
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZGESVD('S','A',M_size,M_size,M,M_size,SV,U,M_size,VH,M_size,work,lwork,r_WK,ifail)
#else
   call CGESVD('S','A',M_size,M_size,M,M_size,SV,U,M_size,VH,M_size,work,lwork,r_WK,ifail)
#endif
   !
   if(ifail/=0) call report_diago_error('GESVD (SVD)','failed')
   !
   SV_min=1./10.**SVD_digits
   !
   do i1=1,M_size
     if (abs(SV(i1))<SV_min) then
       VH(i1,:)=cmplx(0.,0.,SP) 
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
#if defined _DOUBLE
   call zgetrf(M_size,M_size,M,M_size,ipvt,ifail)
#else
   call cgetrf(M_size,M_size,M,M_size,ipvt,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('GETRF (SI)','workspace failed')
   !
   allocate(work(1))
#if defined _DOUBLE
   call zgetri(M_size,M,M_size,ipvt,work,-1,ifail)
#else
   call cgetri(M_size,M,M_size,ipvt,work,-1,ifail)
#endif
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call zgetri(M_size,M,M_size,ipvt,work,lwork,ifail)
#else
   call cgetri(M_size,M,M_size,ipvt,work,lwork,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('GETRF (SI)','failed')
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
#if defined _DOUBLE
   call ZGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&             V_right,M_size,work,-1,rwk,ifail)
#else
   call CGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&             V_right,M_size,work,-1,rwk,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('GEEV (SD)','workspace failed')
   !
   lwork=real(work(1))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&             V_right,M_size,work,lwork,rwk,ifail)
#else
   call CGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&             V_right,M_size,work,lwork,rwk,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('EEV (SD)','failed')
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
#if defined _DOUBLE
   call ZHEEV('V','U',M_size,M,M_size,E_real,work,-1,rwk,ifail)
#else
   call CHEEV('V','U',M_size,M,M_size,E_real,work,-1,rwk,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('HEEV (SD)','workspace failed')
   !
   lwork=real(work(1))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZHEEV('V','U',M_size,M,M_size,E_real,work,lwork,rwk,ifail)
#else
   call CHEEV('V','U',M_size,M,M_size,E_real,work,lwork,rwk,ifail)
#endif
   !
   if(ifail.ne.0) call report_diago_error('HEEV (SD)','failed')
   !
   deallocate(work,rwk)
   return
 endif
 !
 ! =============== PARALLEL SECTION ===============
 !
#if defined _SCALAPACK
 !
 if (lib_in_use==USE_SLK) then
   !
   if (present(fill)) call v2v_blt(blt,iblt)
   if (.not.present(fill)) then
     !
     call blacs_pinfo(myID_in_use, Ncpu_in_use)
     call blacs_get(-1, 0, iblt%ictxt)
     !
     ! Creation of the processor grid
     !
     iblt%npcol = Ncpu_in_use / 2 
     iblt%nprow = Ncpu_in_use / iblt%npcol
     call blacs_gridinit(iblt%ictxt,'r',iblt%nprow,iblt%npcol)
     call blacs_gridinfo(iblt%ictxt,iblt%nprow,iblt%npcol,iblt%myrow, iblt%mycol)
     !
     ! blocking factor: number of rows of the global array in each local array
     !
     iblt%blocking_factor  = BF_compute(M_size)
     !
     ! definition of the leading M_size of a block (iblt%ldrow, iblt%ldcol)
     !
     iblt%ldrow = numroc(M_size, iblt%blocking_factor, iblt%myrow, 0, iblt%nprow)
     iblt%ldcol = numroc(M_size, iblt%blocking_factor, iblt%mycol, 0, iblt%npcol)
     !
     ! definition of the array descriptors
     !
     call descinit(iblt%descM,M_size,M_size,iblt%blocking_factor,&
&                  iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
   end if
   !
   ! ###> PARALLEL INVERSION
   !
   if (idriver==INV) then
     allocate( lM( iblt%ldrow, iblt%ldcol ) )
     do i_loc = 1, iblt%ldrow
       do j_loc = 1, iblt%ldcol
         i = indxl2g(i_loc, iblt%blocking_factor, iblt%myrow, 0, iblt%nprow)
         j = indxl2g(j_loc, iblt%blocking_factor, iblt%mycol, 0, iblt%npcol)
         lM(i_loc, j_loc) = M(i, j)
       end do
     end do
     !
     !Inversion of the global array M
     !
     allocate( ipvt(M_size + iblt%blocking_factor) )
     call pcgetrf( M_size, M_size, lM, 1, 1, iblt%descM, ipvt, ifail )
     if(ifail.ne.0) call report_diago_error('Par. Inv.','factorization failed')
     !
     allocate( work(1), iwork(1) )
     call pcgetri( M_size, lM, 1, 1, iblt%descM, ipvt, work, -1, iwork, -1, ifail )
     if(ifail.ne.0) call report_diago_error('Par. Inv.','workspace failed')
     !
     lwork = int( real(work(1)) )
     liwork = int( iwork(1) )
     deallocate( work, iwork )
     allocate( work(lwork), iwork(liwork) )
     call pcgetri(M_size, lM, 1, 1, iblt%descM, ipvt, work, lwork, iwork, liwork, ifail )
     if(ifail.ne.0) call report_diago_error('Par. Inv.','failed')
     !
     deallocate(ipvt, work, iwork)
     !
     ! Distribute the inverted array over the grid
     !
     M = 0
     do i_loc = 1, iblt%ldrow
       do j_loc = 1, iblt%ldcol
         i = indxl2g(i_loc, iblt%blocking_factor, iblt%myrow, 0, iblt%nprow)
         j = indxl2g(j_loc, iblt%blocking_factor, iblt%mycol, 0, iblt%npcol)
         M(i,j) = lM(i_loc, j_loc)
       end do
     end do  
     call PP_redux_wait(M,COMM=PAR_COM_INV%COMM)
     deallocate(lM)
   end if
   !
   ! ###> PARALLEL DIAGONALIZATION
   !
   if (idriver==DIAGO) then
     call descinit(iblt%descV,M_size,M_size,iblt%blocking_factor,&
&                  iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
     !
     ! Filling of the local array distributed in the processor grid
     !
     allocate( lM( iblt%ldrow, iblt%ldcol), lV( iblt%ldrow, iblt%ldcol) )
     do i_loc=1,M_size
       do j_loc=1,M_size
         call pcelset(lM,i_loc,j_loc,iblt%descM,M(i_loc,j_loc))
       enddo
     enddo
     !
     ! Diagonalization of the global array M
     !
     allocate( work(1), rwk(1) )
     call pcheev('V','U',M_size,lM,1,1,iblt%descM,E_real,lV,1,1,iblt%descV,&
&                work,-1,rwk,-1,ifail )
     if(ifail.ne.0) call report_diago_error('Par. Diag.','workspace failed')
     !
     lwork = int(real(work(1))); rwork = int(real(rwk(1)))*2
     deallocate( work, rwk )
     allocate( work(lwork), rwk(rwork) )
     call pcheev('V','U',M_size,lM,1,1,iblt%descM,E_real,lV,1,1,iblt%descV,&
&                work,lwork,rwk,rwork,ifail )
     if(ifail.ne.0) call report_diago_error('Par. Diag.','failed')
     !
     deallocate( work, rwk, lM)
     !
     ! Distribute the eigenvectors all over the grid
     !
     M = 0
     do i_loc = 1,M_size 
       do j_loc = 1,M_size 
         call pcelget('A', top, M(i_loc,j_loc), lV, i_loc, j_loc, iblt%descV)
       end do
     end do
     deallocate(lV)
   end if
   !
   !Release the proc grid and the BLACS library
   !
   call blacs_gridexit(iblt%ictxt)
   !
 endif
 !
#endif
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
