!
! License-Identifier: GPL
!
! Copyright (C) 2006 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine io_variable_elemental(ID,VAR,CHECK,WARN,OP,     &
&                       I0, DB_I0, I1, DB_I1, &
&                       R0, DB_R0, R1, DB_R1, &
&                       D0, DB_D0, C0, DB_C0, &
&                       L0, DB_L0,            &
&                       CH0,DB_CH0,           &
&                       UNIT,DESCRIPTOR,DO_NOT_DUMP,TERMINATOR)
 !
 use pars,          ONLY:LP,SP,DP,schlen,msg_len
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use netcdf
#if !defined _io_lib
 use drivers, ONLY:list_dbs
#endif
 use com,     ONLY:msg,com_compose_msg
 use stderr,  ONLY:STRING_pack,int2log,log2int,STRING_same,STRING_match
 use IO_m,    ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_var,&
&                  netcdf_dim,io_rec_pos,read_is_on,io_mode,VERIFY,&
&                  io_status,DUMP,REP,LOG,NO_VAR,io_com,io_file,&
&                  io_code_version,io_serial_number,io_code_revision,IO_INCOMPATIBLE_VAR
 use descriptors,ONLY:IO_desc,IO_desc_add
 !
 implicit none
 !
 integer               :: ID
 character(*),optional :: VAR
 character(*),optional :: OP(:)
 integer,     optional :: I0,DB_I0,I1(:),DB_I1(:)
 real(SP),    optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
 real(DP),    optional :: D0,DB_D0
 complex(SP), optional, target :: C0
 complex(SP), optional :: DB_C0
 logical,     optional :: L0,DB_L0,CHECK,WARN,DO_NOT_DUMP
 character(*),optional :: CH0,DB_CH0,TERMINATOR
 type(IO_desc),optional:: DESCRIPTOR
 !
 ! Work Space
 !
 integer,parameter      :: MAX_DB_ELEMENTS=100
 character(msg_len)     :: msg_
 character(msg_len-1)   :: ch
 character(1)           :: msg_where
 integer                :: SZ,i_err,nf_error
 logical                :: CHECK_,WARN_,DUMP_,VERIFY_
 !
 ! DB variables
 !
 integer(LP)            :: l_db(1)
 integer                :: i_db(MAX_DB_ELEMENTS)
 real(SP)               :: r_db(MAX_DB_ELEMENTS),local_unit
 real(DP)               :: d_db(1)
 character(schlen)      :: ch_db(1)
 complex(SP), target    :: c_db(1)
 real(SP), pointer      :: rc_db(:)
 !
 DUMP_= (read_is_on(ID) .and. (io_mode(ID)==DUMP))
 if (present(DO_NOT_DUMP)) DUMP_=.not.DO_NOT_DUMP
 !
 CHECK_=.false.
 if (present(CHECK)) CHECK_=CHECK
 !
 i_err=0
 VERIFY_= (read_is_on(ID) .and. (io_mode(ID)==VERIFY .and. present(OP))) .or.&
&         (read_is_on(ID) .and. (CHECK_.and.present(OP)))
 !
 WARN_ =.false.
 if (present(WARN )) WARN_ =WARN
 !
 local_unit=1._SP
 if (present(UNIT)) local_unit=UNIT
 !
 msg_where=""
 if (io_com(ID)==REP            ) msg_where="r"
#if !defined _io_lib
 if (io_com(ID)==LOG.or.list_dbs) msg_where="s"
#endif
 !
 ! I0
 !
 if (present(I0)) then
   !
   ! I/O
   !
   SZ=1
   i_db(1)=I0
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),i_db(1),(/io_rec_pos(ID)/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),i_db(1),(/io_rec_pos(ID)/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) I0   =i_db(1)
   if (present(DB_I0)) DB_I0=i_db(1)
   if ( VERIFY_ ) i_err=i_verify((/I0/),i_db(1:SZ))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR).and.present(VAR)) then
     call IO_desc_add(DESCRIPTOR,'i',VAR,I=(/I0/),ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! I1
 !
 if (present(I1)) then
   !
   ! I/O
   !
   SZ=size(I1)
   i_db(1:SZ)=I1
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),i_db(1:SZ),(/io_rec_pos(ID)/),(/SZ/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),i_db(1:SZ),(/io_rec_pos(ID)/),(/Sz/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) I1   =i_db(1:SZ)
   if (present(DB_I1)) DB_I1=i_db(1:SZ)
   if ( VERIFY_      ) i_err=i_verify(I1,i_db(1:SZ))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR).and.present(VAR)) then
     call IO_desc_add(DESCRIPTOR,'i',VAR,SZ=SZ,I=I1,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! R0
 !
 if (present(R0)) then
   !
   ! I/O
   !
   SZ=1
   r_db(1)=R0
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),r_db(1),(/io_rec_pos(ID)/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),r_db(1),(/io_rec_pos(ID)/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) R0   =r_db(1)
   if (present(DB_R0)) DB_R0=r_db(1)
   if ( VERIFY_      ) i_err=r_verify((/R0/),r_db(1:SZ))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'r',VAR,R=(/R0*local_unit/),ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! R1
 !
 if (present(R1)) then
   !
   ! I/O
   !
   SZ=size(R1)
   r_db(1:SZ)=R1
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),r_db(1:SZ),(/io_rec_pos(ID)/),(/SZ/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),r_db(1:SZ),(/io_rec_pos(ID)/),(/SZ/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) R1   =r_db(1:SZ)
   if (present(DB_R1)) DB_R1=r_db(1:SZ)
   if ( VERIFY_ ) i_err=r_verify(R1,r_db(1:SZ))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'r',VAR,SZ=size(R1),R=R1*local_unit,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 if (present(D0)) then
   !
   ! I/O
   !
   SZ=1
   d_db(1)=D0
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),d_db(1),(/io_rec_pos(ID)/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),d_db(1),(/io_rec_pos(ID)/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) D0   =d_db(1)
   if (present(DB_D0)) DB_D0=d_db(1)
   if ( VERIFY_      ) i_err=r_verify((/real(D0,SP)/),real(d_db(1:SZ),SP))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'d',VAR,D=D0*local_unit,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! C0
 !
 if (present(C0)) then
   !
   ! I/O
   !
   SZ=1
   c_db(1)=C0
   call C_F_POINTER(C_LOC(c_db(1)), rc_db, (/2*SZ/) )
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),rc_db(1:2*SZ),(/io_rec_pos(ID)/),(/2*SZ/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),rc_db(1:2*SZ),(/io_rec_pos(ID)/),(/2*SZ/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+2*SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) C0   =cmplx(rc_db(1),rc_db(2))
   if (present(DB_C0)) DB_C0=cmplx(rc_db(1),rc_db(2))
   if ( VERIFY_      ) i_err=c_verify((/C0/),c_db(1:SZ))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'c',VAR,C=C0*local_unit,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! L0
 !
 if (present(L0)) then
   !
   ! I/O
   !
   SZ=1
   l_db(1)=log2int(L0)
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),l_db(1),(/io_rec_pos(ID)/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),l_db(1),(/io_rec_pos(ID)/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_        ) L0   =int2log(l_db(1))
   if (present(DB_L0)) DB_L0=int2log(l_db(1))
   !
   if ( VERIFY_ ) then
     i_db(1)=int(log2int(L0),LP)
     i_db(2)=int(l_db(1),LP)
     i_err=i_verify(i_db(1:1),i_db(2:2))
   endif
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'l',VAR,L=L0,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! CH0
 !
 if (present(CH0)) then
   !
   ! I/O
   !
   SZ=1
   ch_db(1)=CH0
   !
   if(write_is_on(ID)) nf_error=nf90_put_var(io_unit(ID),io_netcdf_var(ID,1),ch_db(1),(/io_rec_pos(ID)/),(/SZ*schlen/))
   if( read_is_on(ID)) nf_error=nf90_get_var(io_unit(ID),io_netcdf_var(ID,1),ch_db(1),(/io_rec_pos(ID)/),(/SZ*schlen/))
   !
   io_rec_pos(ID)=io_rec_pos(ID)+SZ*schlen
   call netcdf_call(nf_error,ID)
   !
   if ( DUMP_         ) CH0   =ch_db(1)
   if (present(DB_CH0)) DB_CH0=ch_db(1)
   if ( VERIFY_       ) i_err=ch_verify(CH0,ch_db(1))
   !
   ! DESCRIPTOR
   !
   if (present(DESCRIPTOR)) then
     call IO_desc_add(DESCRIPTOR,'s',VAR,S=CH0,ops=OP,check=CHECK_,term=TERMINATOR)
   endif
   !
 endif
 !
 ! STATUS
 !
 if ( io_mode(ID)==VERIFY .and. CHECK_ .and. io_status(ID)==0) io_status(ID)=i_err
 !
 ! MSG
 !
 if (present(VAR)) then
   ch=VAR
   if (read_is_on(ID).and.i_err/=0) then
     if (CHECK_) ch=STRING_pack("*ERR* ",VAR)
     if (WARN_ ) ch=STRING_pack("*WRN* ",VAR)
   endif
   msg_=com_compose_msg(pre=" ",body=trim(ch))
   if(present(I0) .or.present(I1)) call msg(msg_where,msg_,i_db(1:SZ),after_msg=TERMINATOR)
   if(present(R0) .or.present(R1)) call msg(msg_where,msg_,r_db(1:SZ)*local_unit,after_msg=TERMINATOR)
   if(present(D0)                ) call msg(msg_where,msg_,r_db(1:SZ)*local_unit,after_msg=TERMINATOR)
   if(present(C0)                ) call msg(msg_where,msg_,rc_db(1:2*SZ)*local_unit,after_msg=TERMINATOR)
   if(present(L0)                ) call msg(msg_where,msg_,l_db(1)==1,after_msg=TERMINATOR)
   if(present(CH0)               ) call msg(msg_where,msg_,trim(ch_db(1)),after_msg=TERMINATOR)
 endif
 !
 contains
   !
   integer function i_verify(iv1,iv2)
     integer :: iv1(:),iv2(:),i1
     i_verify=0
     do i1=1,size(iv1)
       if (OP(i1)=="==" .and. iv1(i1)/=iv2(i1)) i_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)=="<"  .and. iv1(i1)>=iv2(i1)) i_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)=="<=" .and. iv1(i1)> iv2(i1)) i_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">"  .and. iv1(i1)<=iv2(i1)) i_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">=" .and. iv1(i1)< iv2(i1)) i_verify=IO_INCOMPATIBLE_VAR
       if (i_verify/=0) return
     enddo
   end function
   !
   integer function r_verify(rv1,rv2)
     integer  :: i1
     real(SP) :: rv1(:),rv2(:),eq_accuracy,acc_fraction
     r_verify   =0
     eq_accuracy=1.E-5_SP
     acc_fraction=0.001_SP
     !
     ! Define the accuracy of the comparison
     !
     do i1=1,size(rv1)
       if ( abs(rv1(i1))>0._SP) eq_accuracy=min( eq_accuracy , abs(rv1(i1))*acc_fraction )
       if ( abs(rv2(i1))>0._SP) eq_accuracy=min( eq_accuracy , abs(rv2(i1))*acc_fraction )
     enddo
     !
     do i1=1,size(rv1)
       if (OP(i1)=="==" .and. abs(rv1(i1)-rv2(i1))>eq_accuracy) r_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)=="<"  .and.     rv1(i1)>=rv2(i1)            ) r_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)=="<=" .and.     rv1(i1)> rv2(i1)            ) r_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">"  .and.     rv1(i1)<=rv2(i1)            ) r_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">=" .and.     rv1(i1)< rv2(i1)            ) r_verify=IO_INCOMPATIBLE_VAR
       if (r_verify/=0) return
     enddo
   end function
   !
   integer function c_verify(cv1,cv2)
     integer     :: i1
     complex(SP) :: cv1(:),cv2(:)
     real(SP)    :: eq_accuracy,acc_fraction
     c_verify   =0
     eq_accuracy=1.E-5_SP
     acc_fraction=0.001_SP
     !
     ! Define the accuracy of the comparison
     !
     do i1=1,size(cv1)
       if ( abs(cv1(i1))>0._SP) eq_accuracy=min( eq_accuracy , abs(cv1(i1))*acc_fraction )
       if ( abs(cv2(i1))>0._SP) eq_accuracy=min( eq_accuracy , abs(cv2(i1))*acc_fraction )
       if (OP(i1)=="<=" .and. abs(cv1(i1))> abs(cv2(i1))      ) c_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">"  .and. abs(cv1(i1))<=abs(cv2(i1))      ) c_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)==">=" .and. abs(cv1(i1))< abs(cv2(i1))      ) c_verify=IO_INCOMPATIBLE_VAR
       if (c_verify/=0) return
     enddo
   end function
   !
   integer function ch_verify(chv1,chv2)
     character(*),optional :: chv1,chv2
     ch_verify=0
     !
     if (.not.present(OP) .and. .not.STRING_same(chv1,chv2)) ch_verify=IO_INCOMPATIBLE_VAR
     !
     if (     present(OP)) then
       if (OP(1)=="==" .and. .not.STRING_same(chv1,chv2) )  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)=="<"  .and. .not.STRING_match(chv2,chv1))  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)=="<=" .and. .not.STRING_match(chv2,chv1))  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)==">"  .and. .not.STRING_match(chv2,chv1))  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)==">=" .and. .not.STRING_match(chv2,chv1))  ch_verify=IO_INCOMPATIBLE_VAR
     endif
     !
   end function
   !
end subroutine
