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
 use BS,            ONLY:BS_H_dim
 use IO_m,          ONLY:frag_BS_K,write_is_on,VERIFY,netcdf_call,&
&                        io_connect,io_header,io_mode,io_unit
 use BS_solvers,    ONLY:variables_BS
#include<memory.h>
 include 'netcdf.inc'
 !
 type(X_t) :: X 
 integer :: io_BS, iq, mode_flag, ncid, varid, stat, ID
 integer :: resonant_block_size,resonant_block_size_dim
 character(schlen) :: db_name,PARALLEL_STRUCTURE
 character(lchlen) ::local_description(100)
 !
 ! Create parallel netCDF file.
 !
 db_name='BS_Q'//trim(intc(iq))//'.nc'
 mode_flag = ior(nf_netcdf4, nf_mpiio)
 stat = nf90_create(db_name,mode_flag,comm=MPI_COMM_WORLD,info=MPI_INFO_NULL,ncid=io_unit(ID))
 call netcdf_call(stat,ID)
 !
#if 0
 !
 ! This is the upper triangular mode
 !
 resonant_block_size = BS_H_dim*(BS_H_dim-1)/2
 stat = nf_def_dim(io_unit(ID), "flattened_upper_triangular_size", &
&                  resonant_block_size, resonant_block_size_dim)
 !
 ! Intialize the variables where the matrix will be stored (resonant only FTM)
 !
 stat = nf_def_var(io_unit(ID), "BSE_RESONANT",   nf_float, 1, [resonant_block_size_dim], varid)
 call netcdf_call(stat,ID)
 stat = nf_var_par_access(io_unit(ID), varid, nf_collective)
 call netcdf_call(stat,ID)
 !
 stat = nf_def_var(io_unit(ID), "BSE_RESONANT_DONE", nf_byte, 1, [resonant_block_size_dim], varid)
 call netcdf_call(stat,ID)
 stat = nf_var_par_access(io_unit(ID), varid, nf_collective)
 call netcdf_call(stat,ID)
#else
 !
 ! Here is the 2D mode
 ! 
 resonant_block_size = BS_H_dim
 stat = nf_def_dim(io_unit(ID), "BS_H_dim", resonant_block_size, resonant_block_size_dim)
 call netcdf_call(stat,ID) 
 !
 ! Intialize the variables where the matrix will be stored (resonant only FTM)
 !
 stat = nf_def_var(io_unit(ID), "BSE_RESONANT",   nf_float, 2, &
&                  [resonant_block_size_dim,resonant_block_size_dim], varid)
 call netcdf_call(stat,ID)
 stat = nf_var_par_access(io_unit(ID), varid, nf_collective)
 call netcdf_call(stat,ID)
 !
 stat = nf_def_var(io_unit(ID), "BSE_RESONANT_DONE", nf_byte, 2, &
&                  [resonant_block_size_dim,resonant_block_size_dim], varid)
 call netcdf_call(stat,ID)
 stat = nf_var_par_access(io_unit(ID), varid, nf_collective)
 call netcdf_call(stat,ID)
#endif
 !
 !
 ! Stop define mode
 !
 stat = nf_enddef(io_unit(ID))
 call netcdf_call(stat,ID)
 !
 ! Write some metadata
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


subroutine io_BS_PAR_write_block(iq,i_block,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use netcdf
 use pars,          ONLY:SP,schlen,lchlen
 use X_m,           ONLY:X_t
 use stderr,        ONLY:intc
 use parallel_m,    ONLY:myid
 use pointers,      ONLY:linkM2V
 use BS,            ONLY:BS_blk,n_BS_blks,BS_H_dim,BS_K_dim,BS_K_has_been_calculated_loaded
 use BS_solvers,    ONLY:BSS_description,BSS_n_descs,variables_BS
 use IO_m,          ONLY:io_sec,io_elemental,io_bulk,frag_BS_K,netcdf_call,&
&                        read_is_on,write_is_on,io_mode,io_unit,VERIFY, &
&                        io_fragment,IO_NOT_ALLOWED,io_BS_K,manage_OP_IF_START_and_CL_IF_END
 use timing_m,      ONLY:timing
#include<memory.h>
 include 'netcdf.inc'
 !
 integer  :: iq,ID,varid,istat
 integer  :: start(1), count(1)
 !
 ! Work Space
 !
 complex(SP),pointer ::tmp_link(:)
 integer             ::i_block,i1,n1,n2,idx,jdx,i,j,g1,g2,x1,x2,block_pos
 character(schlen)   ::ch,PARALLEL_STRUCTURE
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 if (BS_blk(i_block)%mode=="R") ch="BSE_RESONANT"
 if (BS_blk(i_block)%mode=="A") ch="BSE_COUPLING"
 istat = nf90_inq_varid(io_unit(ID),ch,varid)
 call netcdf_call(istat,ID)
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 n1=BS_blk(i_block)%size(1)
 n2=BS_blk(i_block)%size(2)
 !write(*,*) x1,x2,n1,n2
 !  
#if 1
 !
 ! Write one block at a time in 2D
 !
 write(*,*) [x1,x2], [n1,n2]
 istat = nf_put_vara_real(io_unit(ID), varid, [x1,x2], [n1,n2], BS_blk(i_block)%mat)
 call netcdf_call(istat,ID)
#endif

#if 0
 !
 ! Write one element at a time in 1D upper triangular
 !
 do i=1,n1
   do j=1,n2
     !convert from general index to flatten upper tridiagonal index
     if (j>=i) then 
       !calculate general index
       g1 = (x1-1) + (i-1)
       g2 = (x2-1) + (j-1)
       !calculate tridiagonal index
       jdx = g2*BS_K_dim+g1
       idx = jdx-(g2*(g2-1)/2)
       !write the element 
       write(*,*)
       write(*,*) i,j,g1,g2,idx+1
       !istat = nf_put_vara_real(io_unit(ID), varid, [idx], [1], BS_blk(i_block)%mat(i,j))
       !call netcdf_call(istat,ID)
     endif
   enddo
 enddo
#endif

#if 0
 !
 ! Write one column at a time in 1D upper triangular
 !
 do i=1,n1
   !calculate (i,j) general index
   g1 = (x1-1) + (i-1)
   !calculate position of first element in flatten upper tridiagonal
   idx = g2*BS_K_dim+g1+1
   write(*,*) g1,idx
   !write the column
   start = [idx]
   count = [idx+n1-i]
   istat = nf_put_vara_real(io_unit(ID), varid, [idx], [idx+n1-i], BS_blk(i_block)%mat(:i,i))
   write(*,*) start, count, istat
   call netcdf_call(istat,ID)
 enddo
 !
#endif
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine



