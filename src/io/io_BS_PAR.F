!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM
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
subroutine io_BS_PAR_init(iq,X,ID)
 !
 ! Intialize BSE netcdf parallel file
 !
 use netcdf
 use parallel_m
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use IO_m,          ONLY:frag_BS_K,write_is_on,VERIFY,netcdf_call,&
&                        io_connect,io_header,io_mode,io_unit
 use BS_solvers,    ONLY:variables_BS
#include<memory.h>
 include 'netcdf.inc'
 !
 type(X_t) :: X 
 integer :: io_BS, iq, mode_flag, ncid, stat, ID
 character(schlen) :: db_name,PARALLEL_STRUCTURE
 character(lchlen) ::local_description(100)
 !
 ! Create parallel netCDF file.
 !
 db_name='BS_Q'//trim(intc(iq))//'.nc'
 mode_flag = IOR(nf_netcdf4, nf_mpiio)
 stat = nf_create_par(db_name,mode_flag,MPI_COMM_WORLD,MPI_INFO_NULL,io_unit(ID))
 write(*,*) stat
 call netcdf_call(stat,ID)
 !
 io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",&
 &                    PARALLEL_STRUCTURE=trim(PARALLEL_STRUCTURE),CUTOFF=.true.,FRAG=frag_BS_K)
 !
 io_BS=variables_BS(ID,local_description,X=X)
 !
end subroutine


subroutine io_BS_PAR_free(ID)
 !
 ! close BSE netcdf parallel file
 !
 use netcdf
 use IO_m,          ONLY:io_disconnect
 !
 integer  :: ID
 !
 call io_disconnect(ID)
 !
end subroutine


integer function io_BS_PAR_write_block(iq,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:myid
 use pointers,      ONLY:linkM2V
 use BS,            ONLY:BS_blk,n_BS_blks,BS_H_dim,BS_K_dim,BS_K_has_been_calculated_loaded
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,variables_BS
 use IO_m,          ONLY:io_sec,io_elemental,io_bulk,frag_BS_K,&
&                        read_is_on,write_is_on,io_mode,VERIFY, &
&                        io_fragment,IO_NOT_ALLOWED,io_BS_K,manage_OP_IF_START_and_CL_IF_END
 use timing_m,      ONLY:timing
#include<memory.h>
 !
 integer  :: iq,ID
 !
 ! Work Space
 !
 complex(SP),pointer ::tmp_link(:)
 integer             ::i_block,i1,n1,n2,idx,jdx,i,j,g1,g2,x1,x2,block_pos
 character(schlen)   ::ch,PARALLEL_STRUCTURE
 !
 call timing('io_BS',OPR='start')
 !
 i_block=maxval(io_sec(ID,:))-1
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT"
 if (BS_blk(i_block)%mode=="A") ch="BSE_COUPLING"
 !
 !get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 n1=BS_blk(i_block)%size(1)
 n2=BS_blk(i_block)%size(2)
 !write(*,*) x1,x2,n1,n2
 !  
#if 1
 !
 ! Write one element at a time
 !
 do i=1,n1
   do j=1,n2
     !calculate general index
     g1 = (x1-1) + (i-1)
     g2 = (x2-1) + (j-1)
     !convert from general index to flatten upper tridiagonal index
     jdx = g2*BS_K_dim+g1
     if (j<i) then 
       idx = 0
     else
       idx = jdx-(g2*(g2-1)/2)
     endif
     !write(*,*) g1,g2,idx,jdx
     !write the element 
     !nf90_put_var1(ncid, varid, BS_blk(i_block)%mat(i,j), (g1,g2))
   enddo
 enddo
#else
 !
 ! Write one column at a time
 !
 do i=1,n1
   !calculate (i,j) general index
   g1 = (x1-1) + (i-1)
   !calculate position of first element in flatten upper tridiagonal
   idx = g2*BS_K_dim+g1
   write(*,*) g1,idx
   !write the column
   nf90_put_vara(ncid, varid, BS_blk(i_block)%mat(i:,i), start=(idx), count=(idx+n1-i))
 enddo
 !nf90_put_var()   
#endif
 !   
 call timing('io_BS',OPR='stop')
 !
end function



