!
! Copyright (C) 2000-2005 A. Marini and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
 module matrix_operate
!=====================
  use pars
  integer, parameter :: USE_LK=1,USE_SLK=2,ALREADY_FILLED=3
  integer, parameter :: DIAGO=4,INV=5,PDIAGO=6,USE_SVD=7
  type blacs_t
!variables for blacs initialization and processor grid creation
    integer :: ictxt, nprow, npcol, myrow, mycol
!variables needed to distribute the global array across the grid
    integer :: descM(9), descV(9), ldrow, ldcol, blocking_factor 
  end type blacs_t

 contains
!
 subroutine mat_r2c(mr,mc)
!-------------------------
!The dimension of mc commands
!
 implicit none
 real(SP)    :: mr(:,:,:)
 complex(SP) :: mc(:,:)
 integer     :: i1,i2,mr_shp(3),mc_shp(2)
 mr_shp=shape(mr)
 mc_shp=shape(mc)
 do i1=1,min(mr_shp(1),mc_shp(1))
   do i2=1,min(mr_shp(2),mc_shp(2))
     mc(i1,i2)=mr(i1,i2,1)+(0._SP,1._SP)*mr(i1,i2,2)
   enddo
 enddo
 end subroutine

 subroutine mat_c2r(mc,mr)
!-------------------------
!The dimension of mr commands
!
 implicit none
 real(SP)    :: mr(:,:,:)
 complex(SP) :: mc(:,:)
 integer     :: i1,i2,mr_shp(3),mc_shp(2)
 mr_shp=shape(mr)
 mc_shp=shape(mc)
 do i1=1,min(mr_shp(1),mc_shp(1))
   do i2=1,min(mr_shp(2),mc_shp(2))
     mr(i1,i2,1)=real(mc(i1,i2))
     mr(i1,i2,2)=aimag(mc(i1,i2))
   enddo
 enddo
 end subroutine

 subroutine m3inv(m,m_inv)
!-------------------------
 use pars, ONLY:SP
 use com,  ONLY:error,warning
 implicit none
 real(SP)          :: m(3,3)
 real(SP),optional :: m_inv(3,3)
!ws
 real(SP) ::  det,inv(3,3)
 det=m3det(m)
 inv(1,1)=(-(m(2,3)*m(3,2)) + m(2,2)*m(3,3))/det
 inv(1,2)=(m(1,3)*m(3,2) - m(1,2)*m(3,3))/det
 inv(1,3)=(-(m(1,3)*m(2,2)) + m(1,2)*m(2,3))/det
 inv(2,1)=(m(2,3)*m(3,1) - m(2,1)*m(3,3))/det
 inv(2,2)=(-(m(1,3)*m(3,1)) + m(1,1)*m(3,3))/det
 inv(2,3)= (m(1,3)*m(2,1) - m(1,1)*m(2,3))/det
 inv(3,1)= (-(m(2,2)*m(3,1)) + m(2,1)*m(3,2))/det
 inv(3,2)= (m(1,2)*m(3,1) - m(1,1)*m(3,2))/det
 inv(3,3)= (-(m(1,2)*m(2,1)) + m(1,1)*m(2,2))/det
 if (abs(det)<=1.E-7) call error('Null determinant. Inversion failed.')
 if (present(m_inv))      m_inv=inv
 if (.not.present(m_inv)) m=inv
 end subroutine

 function m3det(m)
!-----------------
 use pars
 implicit none
 real(SP) m(3,3),m3det
 m3det =-m(1,3)*m(2,2)*m(3,1) + m(1,2)*m(2,3)*m(3,1) &
&   + m(1,3)*m(2,1)*m(3,2) - m(1,1)*m(2,3)*m(3,2) &
&   - m(1,2)*m(2,1)*m(3,3) + m(1,1)*m(2,2)*m(3,3)
 end function
!
 subroutine mat_dia_inv(idriver,lib_in,M,blt,V_left,V_right,&
&                       E_real,E_cmpl,SVD_cutoff,fill)
!============================================================
 use pars
 use com,         ONLY:error
 use par_proc_m,  ONLY:pp_redux_wait,ncpu,myid
 implicit none
 integer                :: idriver,lib_in
 complex(SP)            :: M(:,:)
 complex(SP),  optional :: V_left(:,:),V_right(:,:),E_cmpl(:)
 type(blacs_t),optional :: blt
 real(SP),     optional :: E_real(:),SVD_cutoff
 integer,      optional :: fill
!
!ws
 integer :: Md(2),i1,i2,lib_in_use
!
!ws (serial invertion)
 integer :: ifail,lwork
 type(blacs_t) :: iblt
 integer, allocatable :: ipvt(:)
 complex(SP), allocatable :: work(:)
!
!ws (additionals for serial diago)
!
 integer :: rwork
 real(SP), allocatable :: rwk(:)
!
!ws (SVD)
!
 real(SP),    allocatable :: SV(:),r_WK(:)
 complex(SP), allocatable :: U(:,:),VH(:,:)
!
!ws (parallel inversion)
!
 complex(SP), allocatable :: lM(:,:) 
 integer      :: i, j, i_loc, j_loc, liwork
!
!ws (parallel diagonalization)
!
 complex(SP), allocatable :: lV(:,:)
 integer,     allocatable :: iwork(:)
 integer      :: numroc, indxl2g
 character(1) :: top=' '
!
! M dimensions and initialization
!=================================
!
 Md=shape(M)
 lib_in_use=lib_in
 if (ncpu==1.or.Md(1)<ncpu) lib_in_use=USE_LK 
#if ! defined SLK
 lib_in_use=USE_LK
#endif
!
!SVD INVERTION (Using Singular Value decomposition)
!--------------------------------------------------
 if (idriver==USE_SVD.and.lib_in_use==USE_LK) then
   allocate(r_WK(5*Md(1)),SV(Md(1)),U(Md(1),Md(1)),VH(Md(1),Md(1)))
   allocate(work(1))
#if defined _DOUBLE
   call ZGESVD('S','A',Md(1),Md(1),M,Md(1),SV,U,Md(1),VH,Md(1),work,-1,r_WK,ifail)
#else
   call CGESVD('S','A',Md(1),Md(1),M,Md(1),SV,U,Md(1),VH,Md(1),work,-1,r_WK,ifail)
#endif
 if(ifail.ne.0) call error('SVD: CGESVD failed')
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZGESVD('S','A',Md(1),Md(1),M,Md(1),SV,U,Md(1),VH,Md(1),work,lwork,r_WK,ifail)
#else
   call CGESVD('S','A',Md(1),Md(1),M,Md(1),SV,U,Md(1),VH,Md(1),work,lwork,r_WK,ifail)
#endif
 if(ifail.ne.0) call error('SVD: CGESVD failed')
   do i1=1,Md(1)
     if (SV(i1)<SVD_cutoff) then
       VH(i1,:)=cmplx(0.,0.,SP) 
     else
       VH(i1,:)=VH(i1,:)/SV(i1)
     endif
   enddo
#if defined _DOUBLE
   call zgemm('c','c',Md(1),Md(1),Md(1),(1._SP,0._SP),VH,Md(1),U,Md(1),(0._SP,0._SP),M,MD(1))
#else
   call cgemm('c','c',Md(1),Md(1),Md(1),(1._SP,0._SP),VH,Md(1),U,Md(1),(0._SP,0._SP),M,MD(1))
#endif
   deallocate(r_WK,SV,U,VH,work)
   return
 endif
!
!SERIAL INVERTION (standard LK)
!------------------------------
 if (idriver==INV.and.lib_in_use==USE_LK) then
   allocate(ipvt(Md(1)))
#if defined _DOUBLE
   call zgetrf(Md(1),Md(1),M,Md(1),ipvt,ifail)
#else
   call cgetrf(Md(1),Md(1),M,Md(1),ipvt,ifail)
#endif
 if(ifail.ne.0) call error('Linv: CGETRF failed')
   allocate(work(1))
#if defined _DOUBLE
   call zgetri(Md(1),M,Md(1),ipvt,work,-1,ifail)
#else
   call cgetri(Md(1),M,Md(1),ipvt,work,-1,ifail)
#endif
   lwork=int(real(work(1)))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call zgetri(Md(1),M,Md(1),ipvt,work,lwork,ifail)
#else
   call cgetri(Md(1),M,Md(1),ipvt,work,lwork,ifail)
#endif
 if(ifail.ne.0) call error('Linv: CGETRF failed')
   deallocate(work,ipvt)
   return
 endif
!
!SERIAL DIAGONALIZATION (Non Hermitian Matrices)
!---------------------------------------------------------
!   The right eigenvector v(j) of A satisfies
!                   A * v(j) = lambda(j) * v(j)
!  where lambda(j) is its eigenvalue.
!  The left eigenvector u(j) of A satisfies
!                u(j)**H * A = lambda(j) * u(j)**H
!  where u(j)**H denotes the conjugate transpose of u(j).
!=========================================================
!
 if (idriver==DIAGO.and.lib_in_use==USE_LK.and.present(E_cmpl)) then

   allocate(work(1),rwk(2*Md(1)))
#if defined _DOUBLE
   call ZGEEV('V','V',Md(1),M,Md(1),E_cmpl,V_left,Md(1),&
&             V_right,Md(1),work,-1,rwk,ifail)
#else
   call CGEEV('V','V',Md(1),M,Md(1),E_cmpl,V_left,Md(1),&
&             V_right,Md(1),work,-1,rwk,ifail)
#endif
 if(ifail.ne.0) call error('Diagonalization: workspace check failed')
   lwork=real(work(1))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZGEEV('V','V',Md(1),M,Md(1),E_cmpl,V_left,Md(1),&
&             V_right,Md(1),work,lwork,rwk,ifail)
#else
   call CGEEV('V','V',Md(1),M,Md(1),E_cmpl,V_left,Md(1),&
&             V_right,Md(1),work,lwork,rwk,ifail)
#endif
 if(ifail.ne.0) call error('Diagonalization failed')
   deallocate(work,rwk)
   return
 endif
!
!SERIAL DIAGONALIZATION (Hermitian Matrices)
!-------------------------------------------
 if (idriver==DIAGO.and.lib_in_use==USE_LK) then
   allocate(work(1),rwk(max(1,3*Md(1)-2)))
#if defined _DOUBLE
   call ZHEEV('V','U',Md(1),M,Md(1),E_real,work,-1,rwk,ifail)
#else
   call CHEEV('V','U',Md(1),M,Md(1),E_real,work,-1,rwk,ifail)
#endif
 if(ifail.ne.0) call error('BSK diagonalization: workspace check failed')
   lwork=real(work(1))
   deallocate(work)
   allocate(work(lwork))
#if defined _DOUBLE
   call ZHEEV('V','U',Md(1),M,Md(1),E_real,work,lwork,rwk,ifail)
#else
   call CHEEV('V','U',Md(1),M,Md(1),E_real,work,lwork,rwk,ifail)
#endif
 if(ifail.ne.0) call error('Diagonalization failed')
   deallocate(work,rwk)
   return
 endif
!
#if defined SLK
!PARALLEL SECTION 
!----------------
 if (lib_in_use==USE_SLK) then
   if (present(fill)) call v2v_blt(blt,iblt)
   if (.not.present(fill)) then
     call blacs_pinfo(myid, ncpu)
     call blacs_get(-1, 0, iblt%ictxt)
!
!Creation of the processor grid
     iblt%npcol = ncpu / 2 
     iblt%nprow = ncpu / iblt%npcol
     call blacs_gridinit(iblt%ictxt,'r',iblt%nprow,iblt%npcol)
     call blacs_gridinfo(iblt%ictxt,iblt%nprow,iblt%npcol,&
&                        iblt%myrow, iblt%mycol)
!
!blocking factor: number of rows of the global array in each local array
      iblt%blocking_factor  = BF_compute(Md(1))
!
!definition of the leading Md(1) of a block (iblt%ldrow, iblt%ldcol)
     iblt%ldrow = numroc(Md(1), iblt%blocking_factor, iblt%myrow, 0, iblt%nprow)
     iblt%ldcol = numroc(Md(1), iblt%blocking_factor, iblt%mycol, 0, iblt%npcol)
!
!definition of the array descriptors
     call descinit(iblt%descM,Md(1),Md(1),iblt%blocking_factor,&
&                  iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
   end if
!
!PARALLEL INVERSION
!------------------
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
!-------------------------------
     allocate( ipvt(Md(1) + iblt%blocking_factor) )
     call pcgetrf( Md(1), Md(1), lM, 1, 1, iblt%descM, ipvt, ifail )
 if(ifail.ne.0) call error('INVERSION: factorization failed')
     allocate( work(1), iwork(1) )
     call pcgetri( Md(1), lM, 1, 1, iblt%descM, ipvt, work, -1, iwork, -1, ifail )
 if(ifail.ne.0) call error('INVERSION: workspace check failed')
     lwork = int( real(work(1)) )
     liwork = int( iwork(1) )
     deallocate( work, iwork )
     allocate( work(lwork), iwork(liwork) )
     call pcgetri(Md(1), lM, 1, 1, iblt%descM, ipvt, work, lwork, iwork, liwork, ifail )
 if(ifail.ne.0) call error('INVERSION failed')
     deallocate(ipvt, work, iwork)
!
!Distribute the inverted array over the grid
!-------------------------------------------
     M = 0
     do i_loc = 1, iblt%ldrow
       do j_loc = 1, iblt%ldcol
         i = indxl2g(i_loc, iblt%blocking_factor, iblt%myrow, 0, iblt%nprow)
         j = indxl2g(j_loc, iblt%blocking_factor, iblt%mycol, 0, iblt%npcol)
         M(i,j) = lM(i_loc, j_loc)
       end do
     end do  
     call pp_redux_wait(M)
     deallocate(lM)
   end if
!
!PARALLEL DIAGONALIZATION
!------------------------
   if (idriver==DIAGO) then
     call descinit(iblt%descV,Md(1),Md(1),iblt%blocking_factor,&
&                  iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
!------------------------------------------------------------------------
!
!Filling of the local array distributed in the processor grid
     allocate( lM( iblt%ldrow, iblt%ldcol), lV( iblt%ldrow, iblt%ldcol) )
     do i_loc=1,Md(1)
       do j_loc=1,Md(1)
         call pcelset(lM,i_loc,j_loc,iblt%descM,M(i_loc,j_loc))
       enddo
     enddo
!
!Diagonalization of the global array M
!-------------------------------------
     allocate( work(1), rwk(1) )
     call pcheev('V','U',Md(1),lM,1,1,iblt%descM,E_real,lV,1,1,iblt%descV,&
&                work,-1,rwk,-1,ifail )
 if(ifail/=0) call error('BSK diagonalization: workspace check failed')
     lwork = int(real(work(1))); rwork = int(real(rwk(1)))*2
     deallocate( work, rwk )
     allocate( work(lwork), rwk(rwork) )
     call pcheev('V','U',Md(1),lM,1,1,iblt%descM,E_real,lV,1,1,iblt%descV,&
&                work,lwork,rwk,rwork,ifail )
 if(ifail/=0) call error('BSK diagonalization failed')
     deallocate( work, rwk, lM)
!
!Distribute the eigenvectors all over the grid
!--------------------------------------------------
     M = 0
     do i_loc = 1, Md(1)
       do j_loc = 1, Md(1)
         call pcelget('A', top, M(i_loc,j_loc), lV, i_loc, j_loc, iblt%descV)
       end do
     end do
     deallocate(lV)
   end if
!
!Release the proc grid and the BLACS library
!-------------------------------------------
   call blacs_gridexit(iblt%ictxt)
   call blacs_exit(1)
 endif
#endif

 contains
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
 end subroutine

 end module
