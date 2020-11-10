!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): HM, DS
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
 ! Intialize variables on BSE netcdf parallel file
 !
 use parallel_m,    ONLY:MPI_COMM_WORLD,PP_redux_wait
 use com,           ONLY:msg
 use pars,          ONLY:SP,schlen,lchlen
 use stderr,        ONLY:intc
 use X_m,           ONLY:X_t
 use BS,            ONLY:BS_K_dim,BS_IO_dim,BS_K_coupling,l_BSE_restart,l_BSE_kernel_complete,&
&                        l_BSK_IO_triangular,l_BS_ares_asymm,BS_mat_res_ares_dim
 use BS_solvers,    ONLY:variables_BS,BSS_desc
 use IO_m,          ONLY:frag_BS_K,write_is_on,read_is_on,get_name,&
&                        io_connect,io_elemental,def_variable_bulk,&
&                        io_header,io_file,io_BS_K
 use descriptors,   ONLY:IO_desc,IO_desc_duplicate
 !
 implicit none
 !
 type(X_t) :: X 
 integer :: iq, ID
 !
 logical, external :: file_exists
 integer, external :: variables_BS_more
 !
 logical :: def_var
 integer :: io_BS,    VAR_IDs(8),n_vars,i1,i2,i3
 type(IO_desc)     :: local_desc
 character(schlen) :: db_name
 character(lchlen) :: full_db_name,ch(4)
 character(schlen) :: dim_names(3,2)
 !
 ! Connect to parallel netCDF file.
 !
 db_name='BS_PAR_Q'//trim(intc(iq))
 full_db_name=get_name(trim(db_name)," ",2,CORE_IO=.FALSE.,MORE_IO=.TRUE.,COM_IO=.FALSE.)
 !
 l_BSE_kernel_complete=.false.
 l_BSE_restart=.false.
 !
 if (write_is_on(ID).and.io_BS_K) then
   if (.not.file_exists(trim(full_db_name))) then
     call msg("s","BSE file not found. Creating an empty one.")
     l_BSE_restart=.false.
   else
     call msg("s","Partial BSE file found in "//trim(full_db_name)//". Using for restart.")
     l_BSE_restart = .true.
   endif
 endif
 !
 call PP_redux_wait()
 !
 io_BS=io_connect(desc=trim(db_name),type=2,ID=ID,comm=MPI_COMM_WORLD)
 if(io_BS/=0) return
 !
 ! Check if the file already contains data and it is consistent
 ! DS: warning. This is potentially dangerous since all MPI tasts are all writing
 !              both the header and the variables
 !              I cannot put here if(master_cpu) because I need all MPI_tasks to
 !              go through the definition of the variables (see similar issue in io_X.F)
 !
 io_BS=io_header(ID,QPTS=.true.,R_LATT=.true.,WF=.true.,IMPOSE_SN=.true.,XC_KIND="K_WF force Xs",CUTOFF=.true.,FRAG=frag_BS_K)
 !
 io_BS=variables_BS(ID,iq,local_desc,.TRUE.,X=X)
 !
 if (io_BS/=0) then
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   call warning('Invalid dimensions found. BSE restart file '//trim(io_file(ID))//' not accepted')
   return
 endif
 !
 io_BS=variables_BS_more(ID,'start')
 !
 if(read_is_on(ID).and.io_BS_K) then
   if(l_BSE_kernel_complete) then
     call msg("s","Complete BSE file found in "//trim(io_file(ID))//". Loading kernel.")
   else
     call msg("s","Partial BSE file found in "//trim(io_file(ID))//". Not accepted in reading mode.")
   endif
 endif
 !
 call PP_redux_wait()
 !
 ! Descriptions transfer
 !   
 call IO_desc_duplicate(local_desc,BSS_desc)
 !
 if(.not.io_BS_K) return
 ! 
 ! Define vars
 !
 ch(1)="BSE_RESONANT"
 ch(2)="BSE_COUPLING"
 ch(3)="BSE_ANTI-RESONANT"
 ch(4)="BSE_ANTI-COUPLING"
 VAR_IDs=(/1,2,3,4,5,6,7,8/)
 !
 if (.not.BS_K_coupling) n_vars=1
 if (     BS_K_coupling) n_vars=2
 if ( l_BS_ares_asymm  ) n_vars=n_vars*2
 !
 if(l_BSK_IO_triangular) then
   !
   ! This is the upper triangular mode
   !
   dim_names(:,1) = [character(schlen) :: 'complex', 'BS_IO_dim1','BS_IO_dim2']
   !
   if(mod(BS_K_dim(1),2)==0) then
     BS_IO_dim(1,1) = BS_K_dim(1)+1
     BS_IO_dim(2,1) = BS_K_dim(1)/2
   else
     BS_IO_dim(1,1) = BS_K_dim(1)
     BS_IO_dim(2,1) = (BS_K_dim(1)+1)/2
   endif
   !
   do i1=1,n_vars
     ch(i1)=trim(ch(i1))//"_RESCALED"
   enddo
   !
 else
   !
   ! Here is the 2D mode
   !
   dim_names(:,1) = [character(schlen) :: 'complex', 'BS_K_dim1', 'BS_K_dim1']
   BS_IO_dim(:,1) = BS_K_dim(1)
   !
   if(BS_mat_res_ares_dim==2) then
     dim_names(:,2) = [character(schlen) :: 'complex', 'BS_K_dim2', 'BS_K_dim2']
     BS_IO_dim(:,2) = BS_K_dim(2)
   endif
   !
 endif
 !
 i3=1
 do i1=1,n_vars
   i2=i1
   if(i1==2 .and. BS_mat_res_ares_dim==2) then; i2=3; i3=2; endif
   call def_variable_bulk(ID,trim(ch(i2)),       VAR_IDs(2*i2-1),&
           &               [2,BS_IO_dim(:,i3)],SP,dim_names(:,i3),par_io_kind='independent')
   call def_variable_bulk(ID,trim(ch(i2))//"_DONE",VAR_IDs(2*i2),&
           &                  BS_IO_dim(:,i3), 0,dim_names(2:3,i3),par_io_kind='independent')
 enddo
 !
end subroutine
!
!
subroutine io_BS_PAR_free(ID,l_kernel_complete)
 !
 ! close BSE netcdf parallel file
 !
 use parallel_m,    ONLY:PP_redux_wait
 use BS,            ONLY:l_BSE_kernel_complete,BS_K_has_been_calculated_loaded
 use IO_m,          ONLY:io_disconnect
 !
 implicit none
 !
 integer, intent(in)  :: ID
 logical, intent(in)  :: l_kernel_complete
 !
 integer              :: io_BS
 integer, external    :: variables_BS_more
 !
 call PP_redux_wait()
 !
 BS_K_has_been_calculated_loaded=l_kernel_complete
 l_BSE_kernel_complete          =l_kernel_complete
 !
 io_BS=variables_BS_more(ID,'end')
 !
 call io_disconnect(ID)
 !
end subroutine
!
!
subroutine io_BS_PAR_block(iq,i_block,ID)
 !
 ! Write one block of the BSE matrix to disk
 ! There is a single file for the full BSE matrix with maximum four variables
 ! for each of the four blocks that can be present (resonant, anti-resonant, coupling...)
 ! For each block only the upper triangular part is written
 ! Each block has a status variables indicating if the matrix element was already calculated
 !
 use pars,          ONLY:IP,IPL
 use BS,            ONLY:BS_blk,l_BSK_IO_triangular,BS_IO_dim,n_BS_blks_min
 use timing_m,      ONLY:timing
 use IO_m,          ONLY:io_variable_bulk,io_unit,netcdf_call,nf90_sync,&
&                        write_is_on,io_BS_K
 !
 implicit none
 !
 integer  :: ID
 !
 ! Work Space
 !
 integer             :: i_block,x1,x2,iq,VAR_IDs(2),stat
 integer             :: n2,i1,i2,idx1,idx2,imax
 integer(IPL)        :: idx
 !
 if(.not.io_BS_K) return
 !
 call timing('io_BS',OPR='start')
 !
 ! Get the type of block that we want to write 
 !
 select case(BS_blk(i_block)%mode)
 case("R")
   VAR_IDs=(/1,2/)
 case("C")
   VAR_IDs=(/3,4/)
 case("A")
   VAR_IDs=(/5,6/)
 case("Q")
   VAR_IDs=(/7,8/)
 end select
 !
 ! Get dimensions
 x1=BS_blk(i_block)%coordinate(1)
 x2=BS_blk(i_block)%coordinate(2)
 !
 !
 if(l_BSK_IO_triangular) then
   !
   do i2=1,BS_blk(i_block)%size(2)
     n2  = (x2+i2-1)
     idx = int(n2,kind=IPL)*int(n2-1,kind=IPL)/int(2,kind=IPL)+int(x1,kind=IPL)
     imax=min(BS_blk(i_block)%size(1),n2-x1+1)
     !
     ! Write column by column in 1D
     !
     !if(imax<1) cycle
     !call io_variable_bulk(ID, VAR_IDs(1),  C1=BS_blk(i_block)%mat(:imax,i2),  IPOS=[1,idx] )
     !call io_variable_bulk(ID, VAR_IDs(2), CH1=BS_blk(i_block)%done(:imax,i2), IPOS=[  idx] )
     !
     ! Write element by element in a reduced 2D matrix
     !
     do i1=1,imax
       idx1=int(mod(idx+int(i1-2,kind=IPL),int(BS_IO_dim(1,1),kind=IPL)),kind=IP)+1
       idx2=int((idx+int(i1-2,kind=IPL))/int(BS_IO_dim(1,1),kind=IPL),kind=IP)+1
       call io_variable_bulk(ID, VAR_IDs(1),  C2=BS_blk(i_block)%mat(i1:i1,i2:i2),  IPOS=[1,idx1,idx2] )
       call io_variable_bulk(ID, VAR_IDs(2), CH2=BS_blk(i_block)%done(i1:i1,i2:i2), IPOS=[  idx1,idx2] )
     enddo
   enddo
   !
 else
   ! 
   ! Write one block at a time in 2D
   !
   call io_variable_bulk(ID, VAR_IDs(1),  C2=BS_blk(i_block)%mat , IPOS=(/1,x1,x2/) )
   call io_variable_bulk(ID, VAR_IDs(2), CH2=BS_blk(i_block)%done, IPOS=(/  x1,x2/) )
   !
 endif
 !
 if(write_is_on(ID).and.i_block<=n_BS_blks_min) then
   stat = nf90_sync(io_unit(ID))
   call netcdf_call(stat,ID)
 endif
 !   
 call timing('io_BS',OPR='stop')
 !
end subroutine
