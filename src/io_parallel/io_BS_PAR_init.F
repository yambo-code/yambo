!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): HM DS
!
integer function io_BS_PAR_init(iq,ID,mode)
 !
 use stderr,        ONLY:intc,real2ch,STRING_remove
 use com,           ONLY:msg
 use pars,          ONLY:IP,IPL,SP,schlen,lchlen,max_io_vars_open
 use BS,            ONLY:BS_K_dim,BS_IO_dim,BS_K_coupling,l_BSE_kernel_complete,&
&                        BSK_IO_mode,MAX_BSK_LIN_size,BS_lin_size_compressed,BS_K_cutoff,     &
&                        BS_res_ares_n_mat,l_BS_ares_from_res,BS_n_eh_spaces
 use IO_m,          ONLY:io_BS_K,io_file,write_is_on,read_is_on,YAMBO_DEF_CHUNK
 use IO_int,        ONLY:io_connect,def_variable_bulk
 !
 implicit none
 !
 integer,      intent(in) :: iq,ID
 character(*), intent(in) :: mode
 !
 ! Workspace
 !
 character(schlen) :: db_name,ch_tmp,dim_names(3,3),BSK_IO_local
 character(lchlen) :: ch(8,max_io_vars_open)
 logical           :: def_var, io_var
 integer(IPL)      :: BS_lin_size
 integer           :: n_vars, i1,i2, i_rep,i_var, BSK_n_dims,n_replica
 integer           :: chunksize(3)
 !
 io_BS_PAR_init=-1
 !
 if(.not.io_BS_K .and. (.not.BS_K_cutoff>0._SP)) return
 !
 if(.not.io_BS_K .and. trim(mode)=="full" )      return
 !
 if( trim(mode)=="full"        ) db_name='BS_PAR_Q'//trim(intc(iq))
 if(index(mode,"compressed")/=0) db_name='BS_COMPRESS'//trim(adjustl(real2ch(BS_K_cutoff)))//'_Q'//trim(intc(iq))
 !
 ! If I'm in compressed_check mode I skip the definition of variables
 !
 io_BS_PAR_init=io_connect(desc=trim(db_name),type=2,ID=ID)
 !
 if(write_is_on(ID).and.io_BS_PAR_init/=0)  return ! here it should give error
 !
 if( read_is_on(ID).and.index(mode,"compressed")/=0) then
   ch_tmp="Compressed BSE file found"
   if(trim(mode)=="compressed_head") ch_tmp=trim(ch_tmp)//", initializing head "
   if(trim(mode)=="compressed_mat" ) ch_tmp="Compressed BSE file, initializing mat "
   if(io_BS_PAR_init==0) call msg("s",trim(ch_tmp),trim(io_file(ID)))
   if(io_BS_PAR_init/=0.or.trim(mode)=="compressed_check") return
 endif
 if( read_is_on(ID).and.trim(mode)=="full") then
   if(io_BS_PAR_init/=0) then
     call msg("s","Complete BSE file not found",trim(io_file(ID)))
     return
   endif
   if (.not.l_BSE_kernel_complete ) return
   if (     l_BSE_kernel_complete ) then
     call msg("s","Complete BSE file loading  ",trim(io_file(ID)))
   endif
 endif
 !
 io_BS_PAR_init=0
 !
 ! Define vars
 !
 dim_names(1,:)  =  'complex'
 !
 ch(1,:)="BSE_RESONANT"
 ch(2,:)="BSE_COUPLING"
 ch(3,:)="BSE_ANTI-RESONANT"
 !
 ! Intitiate chucking array with 0
 chunksize(1) = 2 !! Complex dim
 chunksize(2) = 0
 chunksize(3) = 0 
 !
 if (.not.BS_K_coupling     ) n_vars=1
 if (     BS_K_coupling     ) n_vars=2
 if (.not.l_BS_ares_from_res) n_vars=n_vars+1
 !
 BSK_IO_local=trim(STRING_remove(BSK_IO_mode,"norestart"))
 if(index(mode,"compressed")/=0 ) BSK_IO_local=trim(mode)
 !
 select case(trim(BSK_IO_local))
 case("1D_linear","compressed_head","compressed_mat")
   !
   ! This is the upper triangular matrix rescared in (many) 1D arrays
   !
   BSK_n_dims=1
   !
   if(trim(BSK_IO_local)=="1D_linear" .or. trim(BSK_IO_local)=="compressed_head" ) then
     dim_names(2:3,1)=[ 'BS_K_linearized1', 'BS_K_linearized2' ]
     BS_LIN_size=int(BS_K_dim(1),kind=IPL)*int(BS_K_dim(1)+1,kind=IPL)/int(2,kind=IPL)
   endif
   if(trim(BSK_IO_local)=="compressed_mat") then
     dim_names(2:3,1)=[ 'BS_K_compressed1', 'BS_K_compressed2' ]
     BS_LIN_size=BS_lin_size_compressed
   endif
   !
   if(trim(BSK_IO_local)=="1D_linear" )                                                 ch_tmp="_LINEARIZED"
   if(trim(BSK_IO_local)=="compressed_mat" .or. trim(BSK_IO_local)=="compressed_head" ) ch_tmp="_COMPRESSED"
   BS_IO_dim(:,1)=int(min(int(MAX_BSK_LIN_size,IPL),BS_lin_size),IP)
   !
   n_replica=1+int(BS_LIN_size/int(MAX_BSK_LIN_size,IPL),IP)
   do i_rep=1,n_replica
     do i2=1,n_vars
       i1=i2+n_vars*(i_rep-1)
       i_var=i2
       if (i2==2 .and. BS_res_ares_n_mat==2) i_var=3
       ch(i_var,i_rep)=trim(ch(i_var,i_rep))//trim(ch_tmp)//trim(intc(i_rep))
     enddo
   enddo
   BS_LIN_size=BS_LIN_size-int(MAX_BSK_LIN_size,IPL)*int(n_replica-1,IPL)
   !
   BS_IO_dim(2,1)=int(BS_LIN_size,IP)
   !
 case("2D_standard")
   !
   ! Here is the 2D mode
   !
   BSK_n_dims=2
   n_replica=1
   !
   dim_names(2:3,1) = [character(schlen) :: 'BS_K_dim', 'BS_K_dim']
   BS_IO_dim(:,1) = (/BS_K_dim(1),BS_K_dim(1)/)
   !
   if(BS_n_eh_spaces==2) then
     dim_names(2:3,2) = [character(schlen) ::  'BS_K_dim', 'BS_K_dim_A']
     BS_IO_dim(:,2) = (/BS_K_dim(1),BS_K_dim(2)/)
     dim_names(2:3,3) = [character(schlen) :: 'BS_K_dim_A', 'BS_K_dim_A']
     BS_IO_dim(:,3) = (/BS_K_dim(2),BS_K_dim(2)/)
   else if (BS_K_coupling) then
     dim_names(2:3,2) = [character(schlen) :: 'BS_K_dim', 'BS_K_dim']
     BS_IO_dim(:,2) = (/BS_K_dim(1),BS_K_dim(1)/)
   endif
   !
   ! For now we adapt chunking only to 2D_standard
   chunksize(2) = YAMBO_DEF_CHUNK
   chunksize(3) = YAMBO_DEF_CHUNK
   ! If BSE matrix is <YAMBO_DEF_CHUNK, we do not chunk.
   if (BS_K_dim(1) < YAMBO_DEF_CHUNK) chunksize(2:3) = 0
   if (BS_n_eh_spaces==2 .and. BS_K_dim(2) < YAMBO_DEF_CHUNK) chunksize(2:3) = 0
   !
 end select
 !
 do i_rep=1,n_replica
   do i2=1,n_vars
     i1=i2+n_vars*(i_rep-1)
     i_var=i2
     if(i2==2 .and. BS_res_ares_n_mat==2) then; i_var=3; endif
     if( (trim(BSK_IO_local)=="1D_linear".or.trim(BSK_IO_local)=="compressed_mat".or.trim(BSK_IO_local)=="compressed_head") &
&      .and.i_rep==n_replica.and.i_rep>1) then
       dim_names(2,i_var)=dim_names(3,i_var)
       BS_IO_dim(1,i_var)=BS_IO_dim(2,i_var)
     endif
     if(trim(mode)=="full" .or. trim(mode)=="compressed_mat") &
&           call def_variable_bulk(ID,trim(ch(i_var,i_rep)),         1+(i_var-1)*2+(i_rep-1)*8,&
&                [2,BS_IO_dim(1:BSK_n_dims,i_var)],SP,dim_names(1:BSK_n_dims+1,i_var),&
&                 par_io_kind='independent',chunksize=chunksize)
     if(                        trim(mode)=="compressed_head") &
&           call def_variable_bulk(ID,trim(ch(i_var,i_rep))//"_DONE",2+(i_var-1)*2+(i_rep-1)*8,&
&                BS_IO_dim(1:BSK_n_dims,i_var) , 0,dim_names(2:BSK_n_dims+1,i_var),par_io_kind='independent')
   enddo
 enddo
 !
end function io_BS_PAR_init
