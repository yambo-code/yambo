!
! Copyright (C) 2000-2012 A. Marini & M. Bruno and the YAMBO team 
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
module matrix_operate
 !
#if defined _DISTRIBUTED
 use mp_global,         ONLY:np_ortho,init_ortho_group
#endif
 !
 implicit none
 !
 integer, parameter :: USE_LK=1
 integer, parameter :: USE_SLK=2
 integer, parameter :: ALREADY_FILLED=3
 integer, parameter :: DIAGO=4
 integer, parameter :: INV=5
 integer, parameter :: PARTIAL_DIAGO=6
 integer, parameter :: USE_SVD=7
#if defined _DISTRIBUTED
 integer, parameter :: USE_PWDIAGO=8
#endif
 !
 integer, parameter :: min_cpu_block_size=100
 !
 type blacs_t
   !
   ! variables for blacs initialization and processor grid creation
   !
   integer :: ictxt, nprow, npcol, myrow, mycol
   !
   ! variables needed to distribute the global array across the grid
   !
   integer :: descM(9), descV(9), ldrow, ldcol, blocking_factor 
 end type blacs_t 
 !
 contains
   !
   subroutine mat_r2c_ordered(mr,mc,m_order) 
     !
     ! Copy mr in mc using m_order indexes for mc
     !
     use pars, ONLY:SP
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer     :: m_order(:)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     !
     mr_shp=shape(mr)
     mc_shp=shape(mc)
     !
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         mc(m_order(i1),m_order(i2))=mr(i1,i2,1)+(0._SP,1._SP)*mr(i1,i2,2)
       enddo
     enddo
     !
   end subroutine
   !
   subroutine mat_r2c(mr,mc)
     !
     ! Copy mr in mc 
     !
     use pars, ONLY:SP
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     !
     mr_shp=shape(mr)
     mc_shp=shape(mc) 
     !
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         mc(i1,i2)=mr(i1,i2,1)+(0._SP,1._SP)*mr(i1,i2,2)
       enddo
     enddo 
     !
   end subroutine
   !
   subroutine mat_c2r(mc,mr)
     !
     ! Copy mc in mr
     !
     use pars, ONLY:SP
     real(SP)    :: mr(:,:,:)
     complex(SP) :: mc(:,:)
     integer     :: i1,i2,mr_shp(3),mc_shp(2) ! Work Space
     mr_shp=shape(mr)
     mc_shp=shape(mc)
     do i1=1,min(mr_shp(1),mc_shp(1))
       do i2=1,min(mr_shp(2),mc_shp(2))
         mr(i1,i2,1)=real(mc(i1,i2),SP)
         mr(i1,i2,2)=aimag(mc(i1,i2))
       enddo
     enddo
     !
   end subroutine
   !
   subroutine m3inv(m,m_inv)
     !
     ! Analitic inverse of a real 3x3 matrix
     !
     use pars, ONLY:SP
     use com,  ONLY:error,warning
     real(SP)          :: m(3,3)
     real(SP),optional :: m_inv(3,3)
     real(SP)          :: det,inv(3,3) ! Work Space
     det=m3det(m)
     inv(1,1)=(-m(2,3)*m(3,2) + m(2,2)*m(3,3) )/det
     inv(1,2)=( m(1,3)*m(3,2) - m(1,2)*m(3,3) )/det
     inv(1,3)=(-m(1,3)*m(2,2) + m(1,2)*m(2,3) )/det
     inv(2,1)=( m(2,3)*m(3,1) - m(2,1)*m(3,3) )/det
     inv(2,2)=(-m(1,3)*m(3,1) + m(1,1)*m(3,3) )/det
     inv(2,3)=( m(1,3)*m(2,1) - m(1,1)*m(2,3) )/det
     inv(3,1)=(-m(2,2)*m(3,1) + m(2,1)*m(3,2) )/det
     inv(3,2)=( m(1,2)*m(3,1) - m(1,1)*m(3,2) )/det
     inv(3,3)=(-m(1,2)*m(2,1) + m(1,1)*m(2,2) )/det
     !
     if (abs(det)<=1.E-7) call error('Null determinant. Inversion failed.')
     if (present(m_inv))      m_inv=inv
     if (.not.present(m_inv)) m=inv
     !
   end subroutine
   !
   subroutine m3inv_c(m,m_inv)
     !
     ! Analitic inverse of a complex 3x3 matrix
     !
     use pars, ONLY:SP
     use com,  ONLY:error,warning
     complex(SP)          :: m(3,3)
     complex(SP),optional :: m_inv(3,3)
     complex(SP)          :: det,inv(3,3) ! Work Space
     det=m3det_c(m)
     inv(1,1)=(-m(2,3)*m(3,2) + m(2,2)*m(3,3) )/det
     inv(1,2)=( m(1,3)*m(3,2) - m(1,2)*m(3,3) )/det
     inv(1,3)=(-m(1,3)*m(2,2) + m(1,2)*m(2,3) )/det
     inv(2,1)=( m(2,3)*m(3,1) - m(2,1)*m(3,3) )/det
     inv(2,2)=(-m(1,3)*m(3,1) + m(1,1)*m(3,3) )/det
     inv(2,3)=( m(1,3)*m(2,1) - m(1,1)*m(2,3) )/det
     inv(3,1)=(-m(2,2)*m(3,1) + m(2,1)*m(3,2) )/det
     inv(3,2)=( m(1,2)*m(3,1) - m(1,1)*m(3,2) )/det
     inv(3,3)=(-m(1,2)*m(2,1) + m(1,1)*m(2,2) )/det
     !
     !if (abs(det)<=1.E-7) call error('Null determinant. Inversion failed.')
     if (present(m_inv))      m_inv=inv
     if (.not.present(m_inv)) m=inv
     !
   end subroutine
   !
   function m3det(m)
     use pars, ONLY:SP
     real(SP) :: m(3,3),m3det
     m3det = m(1,1)*( m(2,2)*m(3,3) - m(2,3)*m(3,2) )   &
&           -m(1,2)*( m(2,1)*m(3,3) - m(2,3)*m(3,1) ) + &
&            m(1,3)*( m(2,1)*m(3,2) - m(2,2)*m(3,1) )
   end function
   !
   function m3det_c(m)
     use pars, ONLY:SP
     complex(SP) :: m(3,3),m3det_c
     m3det_c = m(1,1)*( m(2,2)*m(3,3) - m(2,3)*m(3,2) )   &
&             -m(1,2)*( m(2,1)*m(3,3) - m(2,3)*m(3,1) ) + &
&              m(1,3)*( m(2,1)*m(3,2) - m(2,2)*m(3,1) )
   end function
   !
   subroutine mat_dia_inv(idriver,lib_in,M,blt,V_left,V_right,&
&                         E_real,E_cmpl,SVD_digits,fill)
     !
     use wrapper,     ONLY:M_by_M
     use pars,        ONLY:SP
     use par_proc_m,  ONLY:pp_redux_wait,ncpu,myid
#if defined _DISTRIBUTED
     use stderr,      ONLY:intc
     use com,         ONLY:msg
#endif
     integer                :: idriver,lib_in
     complex(SP)            :: M(:,:)
     complex(SP),  optional :: V_left(:,:),V_right(:,:),E_cmpl(:)
     type(blacs_t),optional :: blt
     real(SP),     optional :: E_real(:)
     integer,      optional :: fill,SVD_digits 
     !
     !Work Spaces ...
     !
     real(SP):: SV_min
     integer :: M_size,i1,i2,lib_in_use
     !
     ! ...serial invertion
     !
     integer       :: ifail,lwork
     type(blacs_t) :: iblt
     integer, allocatable     :: ipvt(:)
     complex(SP), allocatable :: work(:)
     !
     ! ...additionals for serial diago
     !
     integer :: rwork
     real(SP), allocatable :: rwk(:)
     !
     ! ...SVD
     !
     real(SP),    allocatable :: SV(:),r_WK(:)
     complex(SP), allocatable :: U(:,:),VH(:,:)
     !
     ! ...parallel inversion
     !
     complex(SP), allocatable :: lM(:,:) 
     integer                  :: i, j, i_loc, j_loc, liwork
     !
     ! ...parallel diagonalization
     !
     complex(SP), allocatable :: lV(:,:)
     integer,     allocatable :: iwork(:)
     integer      :: numroc, indxl2g
     character(1) :: top=' '
     !
     ! M dimensions and initialization
     !
     M_size=size(M,2)
     lib_in_use=lib_in
     !
     ! Decide which to use among LK or SLK
     !
     if (ncpu==1.or.M_size/ncpu<min_cpu_block_size) lib_in_use=USE_LK 
#if ! defined _SCALAPACK && ! defined _DISTRIBUTED
     lib_in_use=USE_LK
#endif
     !
#if defined _DISTRIBUTED
     if ((idriver/=DIAGO.or.present(E_cmpl).or.ncpu<4).and.lib_in_use==USE_PWDIAGO) lib_in_use=USE_LK
#endif

     !
     ! =============== SERIAL SECTION ===============
     !
     !
     ! ####> SVD INVERTION (Using Singular Value decomposition)
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
     ! ###> SERIAL INVERTION (standard LK)
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
     ! ###> SERIAL DIAGONALIZATION (Non Hermitian Matrices)
     !
     !   The right eigenvector v(j) of A satisfies
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
&                 V_right,M_size,work,-1,rwk,ifail)
#else
       call CGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&                 V_right,M_size,work,-1,rwk,ifail)
#endif
       !
       if(ifail.ne.0) call report_diago_error('GEEV (SD)','workspace failed')
       !
       lwork=real(work(1))
       deallocate(work)
       allocate(work(lwork))
#if defined _DOUBLE
       call ZGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&                 V_right,M_size,work,lwork,rwk,ifail)
#else
       call CGEEV('V','V',M_size,M,M_size,E_cmpl,V_left,M_size,&
&                 V_right,M_size,work,lwork,rwk,ifail)
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
#if defined _DISTRIBUTED
     if (idriver==DIAGO.and.lib_in_use==USE_PWDIAGO) then    
       !
       CALL init_ortho_group()
       call msg('r',"a parallel distributed memory algorithm will be used,")
       call msg('r',"eigenstates matrixes will be distributed block like on")
       call msg('r',"ortho sub-group = "//intc(np_ortho(1))//"*"//intc(np_ortho(2))//" procs")
       call pcdiagh_nodist(M_size, M_size, M, M_size, E_real, M)
       !
     endif
#endif
     !
#if defined _SALAPACK
     !
     if (lib_in_use==USE_SLK) then
       !
       if (present(fill)) call v2v_blt(blt,iblt)
       if (.not.present(fill)) then
         !
         call blacs_pinfo(myid, ncpu)
         call blacs_get(-1, 0, iblt%ictxt)
         !
         ! Creation of the processor grid
         !
         iblt%npcol = ncpu / 2 
         iblt%nprow = ncpu / iblt%npcol
         call blacs_gridinit(iblt%ictxt,'r',iblt%nprow,iblt%npcol)
         call blacs_gridinfo(iblt%ictxt,iblt%nprow,iblt%npcol,&
&                            iblt%myrow, iblt%mycol)
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
&                      iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
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
         call pp_redux_wait(M)
         deallocate(lM)
       end if
       !
       ! ###> PARALLEL DIAGONALIZATION
       !
       if (idriver==DIAGO) then
         call descinit(iblt%descV,M_size,M_size,iblt%blocking_factor,&
&                      iblt%blocking_factor,0,0,iblt%ictxt,iblt%ldrow,ifail)
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
&                    work,-1,rwk,-1,ifail )
         if(ifail.ne.0) call report_diago_error('Par. Diag.','workspace failed')
         !
         lwork = int(real(work(1))); rwork = int(real(rwk(1)))*2
         deallocate( work, rwk )
         allocate( work(lwork), rwk(rwork) )
         call pcheev('V','U',M_size,lM,1,1,iblt%descM,E_real,lV,1,1,iblt%descV,&
&                    work,lwork,rwk,rwork,ifail )
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
    !
end module
