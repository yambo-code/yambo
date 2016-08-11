!
!        Copyright (C) 2000-2016 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, AF
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
! Copyright (C) 2004 WanT Group
!
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
integer function PARALLEL_startup(POOL, ORTHO, ndim, Amat )
 !
 use pars,           ONLY:SP
 use SLK_m,          ONLY:POOL_group,ORTHO_group,ndim_blc,info,nprow,npcol,myrow,&
&                         mycol,descA,liwork,lwork
 use linear_algebra, ONLY:WS
 !
 implicit none
 !
 integer           :: ndim
 type(POOL_group)  :: POOL
 type(ORTHO_group) :: ORTHO
 complex(SP)       :: Amat(ndim,ndim)
 !
 ! Work Space
 !
 integer   :: lld, ils, ile, jls, jle
 !
 ! init global blacs grid
 !
 call BLACS_GRIDINFO( ORTHO%ortho_cntx, ORTHO%grid(1), ORTHO%grid(2), ORTHO%coordinate(1), ORTHO%coordinate(2) )
 !
 nprow=ORTHO%grid(1)
 npcol=ORTHO%grid(2)
 myrow=ORTHO%coordinate(1)
 mycol=ORTHO%coordinate(2)
 !
 ! spectator tasks
 if (ORTHO%coordinate(1) /= -1) PARALLEL_startup=1
 !
 if (PARALLEL_startup==1) then
   !
   ! distribute the matrix on the process grid
   ! Initialize the array descriptors for the matrices A and B
   !
   ndim_blc = int(ndim/nprow)
   if (ndim_blc*nprow < ndim ) ndim_blc=ndim_blc+1
   !
   lld = ndim_blc
   !
   call DESCINIT( descA, ndim, ndim, ndim_blc, ndim_blc, 0, 0, ORTHO%ortho_cntx, lld, info )
   !
   allocate( WS%m1_cmplx(ndim_blc,ndim_blc) )
   allocate( WS%v_int(ndim+ndim_blc) )
   !
   ! LWORK  = LOCr(N+MOD(IA-1,MB_A))*NB_A
   ! LIWORK = LOCc( N_A + MOD(JA-1, NB_A) ) + NB_A
   !
   lwork  = ndim_blc*ndim_blc
   liwork = ndim_blc+ndim_blc
   !
   allocate( WS%v_cmplx(lwork) )
   allocate( WS%vp_int(liwork) )
   !
   ! distribute the matrix A
   !
   ils=myrow*ndim_blc+1
   ile=min(myrow*ndim_blc+ndim_blc,ndim)
   jls=mycol*ndim_blc+1
   jle=min(mycol*ndim_blc+ndim_blc,ndim)
   !
   WS%m1_cmplx=0.0_SP
   WS%m1_cmplx=Amat(ils:ile,jls:jle)
   !
   return
   !
 endif
 !
 PARALLEL_startup=-1
 !
end function PARALLEL_startup
