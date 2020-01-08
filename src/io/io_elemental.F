!
!        Copyright (C) 2000-2019 the YAMBO team
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
subroutine io_elemental(ID,VAR,VAR_SZ,CHECK,WARN,OP,I0,DB_I0,I1,DB_I1,&
&                       R0,DB_R0,R1,DB_R1,D0,DB_D0,C0,DB_C0,CH0,DB_CH0,CH1,L0,DB_L0,UNIT,MENU,&
&                       DESCRIPTION,DO_NOT_DUMP)
 !
 use pars,          ONLY:SP,DP,schlen,lchlen,nf90_SP
 use iso_c_binding, ONLY:C_LOC,C_F_POINTER
 use netcdf
 use drivers, ONLY:list_dbs
 use com,     ONLY:msg
 use stderr,  ONLY:STRING_pack,intc,real2ch,complex2ch
 use IO_m,    ONLY:write_is_on,io_unit,netcdf_call,io_netcdf_elem_varid,ver_is_gt_or_eq,&
&                  netcdf_dim,io_rec_pos,read_is_on,io_mode,VERIFY,&
&                  io_status,DUMP,REP,LOG,io_com,io_file,&
&                  io_code_version,io_serial_number,io_code_revision,IO_INCOMPATIBLE_VAR
 implicit none
 !
 integer               :: ID
 integer,parameter     :: MAX_DB_ELEMENTS=100
 character(*),optional :: VAR,OP(:)
 integer,     optional :: VAR_SZ,MENU
                               ! MENU = 1 (on) 0 (off)
 integer,     optional :: I0,DB_I0,I1(:),DB_I1(:)
 real(SP),    optional :: R0,DB_R0,R1(:),DB_R1(:),UNIT
 real(DP),    optional :: D0,DB_D0
 complex(SP), optional, target :: C0
 complex(SP), optional :: DB_C0
 logical,     optional :: L0,DB_L0,CHECK,WARN,DO_NOT_DUMP
 character(*),optional :: CH0,DB_CH0,CH1(:)
 character(lchlen),optional :: DESCRIPTION
 !
 ! Work Space
 !
 character(lchlen) :: ch
 character(1)      :: msg_where
 character(3)      :: MODE_
 integer :: i_db(MAX_DB_ELEMENTS),i_err,nf_error,MENU_
 real(SP):: r_db(MAX_DB_ELEMENTS),r_from_c(2,MAX_DB_ELEMENTS),local_unit
 real(DP):: d_db(MAX_DB_ELEMENTS)
 complex(SP), target :: c_db
 real(SP), pointer  :: RC0(:)
 character(schlen) :: db_ch(MAX_DB_ELEMENTS)
 logical :: CHECK_,DUMP_
 !
 DUMP_= (io_mode(ID)==DUMP)
 if (present(DO_NOT_DUMP)) DUMP_=.not.DO_NOT_DUMP
 !
 CHECK_=.false.
 if (present(CHECK)) CHECK_=CHECK
 !
 MENU_=1
 if (present(MENU)) MENU_=MENU
 !
 if (write_is_on(ID)) MODE_="[WR"
 if ( read_is_on(ID)) MODE_="[RD"
 !
 local_unit=1._SP
 if (present(UNIT)) local_unit=UNIT
 !
 msg_where=""
 if (io_com(ID)==REP            ) msg_where="r"
 if (io_com(ID)==LOG.or.list_dbs) msg_where="s"
 !
 if (present(VAR).and.present(VAR_SZ)) then
   io_rec_pos(ID)=1
   if (VAR_SZ>0) then
     ch=STRING_pack(MODE_,trim(io_file(ID)),"]",repeat('-',max( 60-len_trim(io_file(ID))-4,1) ))
     if (write_is_on(ID)) then
       call netcdf_call(nf90_redef(io_unit(ID)),ID,VAR=VAR)
       nf_error=nf90_inq_varid(io_unit(ID),VAR,io_netcdf_elem_varid(ID))
       if (nf_error/=NF90_NOERR) then
         if (present(CH0).or.present(CH1) ) then
           nf_error=nf90_def_var(io_unit(ID),VAR,nf90_char,(/netcdf_dim(ID,schlen),netcdf_dim(ID,VAR_SZ)/),io_netcdf_elem_varid(ID))
         else
           nf_error=nf90_def_var(io_unit(ID),VAR,nf90_SP,netcdf_dim(ID,VAR_SZ),io_netcdf_elem_varid(ID))
         endif
         call netcdf_call(nf_error,ID,VAR=VAR)
       endif
       call netcdf_call(nf90_enddef(io_unit(ID)),ID,VAR=VAR) 
     else if (read_is_on(ID)) then
       call netcdf_call(nf90_inq_varid(io_unit(ID),VAR,io_netcdf_elem_varid(ID)),ID,VAR=VAR)
     endif
   else
     !
     ! GPL_EXCLUDE_START
     !
     write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
&          repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
&          '.',io_code_version(ID,3),' r.',io_code_revision(ID,1),' -'
     !
     ! GPL_EXCLUDE_END
     !
     ! GPL_INCLUDE_START
!     write (ch,'(a,i6.6,2a,3(a,i2.2),a,i5.5,a)')  '- S/N ',io_serial_number(ID),' ',&
!&          repeat('-',25),'- v.',io_code_version(ID,1),'.',io_code_version(ID,2),&
!&          '.',io_code_version(ID,3),' r.',io_code_revision(ID,2),' -'
     ! GPL_INCLUDE_END
     !
   endif
   if (MENU_==1) call msg(msg_where,trim(ch))
   return
 endif
 !
 if (present(I0)) then
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),I0,(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_I0)) DB_I0=I0
     if (present(VAR)) call msg(msg_where,VAR,I0)
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),i_db(1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (DUMP_)          I0=i_db(1)
     if (present(DB_I0)) DB_I0=i_db(1)
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=i_verify((/I0/),(/i_db(1)/))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),i_db(1))
     else if (present(VAR)) then
       call msg(msg_where,VAR,i_db(1))
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) DESCRIPTION=VAR//intc(I0)
   !
   io_rec_pos(ID)=io_rec_pos(ID)+1
   !
 endif
 !
 if (present(I1)) then
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),I1,(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_I1)) DB_I1=I1
     if (present(VAR)) call msg(msg_where,VAR,I1)
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),i_db(:size(I1)),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (DUMP_)          I1=i_db(:size(I1))
     if (present(DB_I1)) DB_I1=i_db(:size(I1))
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=i_verify(I1,i_db(:size(I1)))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),i_db(:size(I1)))
     else if (present(VAR)) then
       call msg(msg_where,VAR,i_db(:size(I1)))
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(intc(I1(1)))
     if (size(I1)==2) DESCRIPTION=VAR//trim(intc(I1(1)))//' - '//trim(intc(I1(2)))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+size(I1)
 endif
 !
 if (present(R0)) then
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),R0,(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_R0)) DB_R0=R0
     if (present(VAR)) call msg(msg_where,VAR,R0*local_unit)
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),r_db(1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (DUMP_)          R0=r_db(1)
     if (present(DB_R0)) DB_R0=r_db(1)
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=r_verify((/R0/),(/r_db(1)/))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),r_db(1)*local_unit)
     else if (present(VAR)) then
       call msg(msg_where,VAR,r_db(1)*local_unit)
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(real2ch(R0*local_unit))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+1
   !
 endif
 !
 if (present(R1)) then
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),R1,(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_R1)) DB_R1=R1
     if (present(VAR)) call msg(msg_where,VAR,R1*local_unit)
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),r_db(:size(R1)),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (DUMP_)          R1=r_db(:size(R1))
     if (present(DB_R1)) DB_R1=r_db(:size(R1))
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=r_verify(R1,r_db(:size(R1)))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),r_db(:size(R1))*local_unit)
     else if (present(VAR)) then
       call msg(msg_where,VAR,r_db(:size(R1))*local_unit)
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(real2ch(R1(1)*local_unit))
     if (size(R1)==2) DESCRIPTION=VAR//trim(real2ch(R1(1)*local_unit))//&
&                                 ' - '//trim(real2ch(R1(2)*local_unit))
     if (size(R1)==3) DESCRIPTION=VAR//trim(real2ch(R1(1)*local_unit))//&
&                                 ' '//trim(real2ch(R1(2)*local_unit))//&
&                                 ' '//trim(real2ch(R1(3)*local_unit))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+size(R1)
 endif
 !
 if (present(D0)) then
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),D0,(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_D0)) DB_D0=D0
     if (present(VAR)) call msg(msg_where,VAR,real(D0)*local_unit)
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),r_db(1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (DUMP_)          D0=r_db(1)
     if (present(DB_D0)) DB_D0=r_db(1)
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=r_verify((/real(D0,SP)/),(/r_db(1)/))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),r_db(1)*local_unit)
     else if (present(VAR)) then
       call msg(msg_where,VAR,r_db(1)*local_unit)
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(real2ch(real(D0,SP)*local_unit))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+1
   !
 endif
 !
 if (present(C0)) then
   if (write_is_on(ID)) then
     call C_F_POINTER(C_LOC(C0), RC0, (/2/) )
     nf_error=nf90_put_var(io_unit(ID), io_netcdf_elem_varid(ID), RC0, (/1/), (/2/))
     call netcdf_call(nf_error,ID)
     r_from_c(1,1)=real(C0)
     r_from_c(2,1)=aimag(C0)
     if (present(DB_C0)) DB_C0=C0
     if (present(VAR)) call msg(msg_where,VAR,r_from_c(:,1)*local_unit)
   else if (read_is_on(ID)) then
     call C_F_POINTER(C_LOC(c_db), RC0, (/2/) )
     nf_error=nf90_get_var(io_unit(ID), io_netcdf_elem_varid(ID), RC0, (/1/), (/2/))
     call netcdf_call(nf_error,ID)
     r_from_c(1,1)=real(C0)
     r_from_c(2,1)=aimag(C0)
     if (DUMP_)          C0=c_db
     if (present(DB_C0)) DB_C0=c_db
     ch=""
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=c_verify((/C0/),(/c_db/))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),r_from_c(:,1)*local_unit)
     else if (present(VAR)) then
       call msg(msg_where,VAR,r_from_c(:,1)*local_unit)
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(complex2ch(C0*local_unit))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+2
   !
 endif
 !
 if (present(L0)) then
   i_db(1)=0
   if (L0) i_db(1)=1
   if (write_is_on(ID)) then
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),i_db(1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(VAR)) call msg(msg_where,VAR,i_db(1)==1)
     if (present(DB_L0)) DB_L0=i_db(1)==1
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),i_db(2),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     ch=""
     if (present(DB_L0)) DB_L0=i_db(2)==1
     if (DUMP_)          L0=i_db(2)==1
     if (io_mode(ID)==VERIFY.and.present(OP)) then
       i_err=i_verify((/i_db(1)/),(/i_db(2)/))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),i_db(2)==1)
     else if (present(VAR)) then
       call msg(msg_where,VAR,i_db(2)==1)
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//' no'
     if (L0) DESCRIPTION=VAR//' yes'
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+1
 endif
 !
 if (present(CH0)) then
   if (write_is_on(ID)) then
     db_ch(1)=CH0
     nf_error=nf90_put_var(io_unit(ID),io_netcdf_elem_varid(ID),db_ch(:1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     if (present(DB_CH0))   DB_CH0=CH0
     if (present(VAR)) call msg(msg_where,VAR,trim(db_ch(1)))
   else if (read_is_on(ID)) then
     nf_error=nf90_get_var(io_unit(ID),io_netcdf_elem_varid(ID),db_ch(:1),(/io_rec_pos(ID)/))
     call netcdf_call(nf_error,ID)
     ch=""
     if (DUMP_)             CH0=db_ch(1)
     if (present(DB_CH0))   DB_CH0=db_ch(1)
     if (io_mode(ID)==VERIFY) then
       i_err=ch_verify(CH0,db_ch(1))
       if (CHECK_) then
         if (io_status(ID)==0) io_status(ID)=i_err
         if (i_err/=0.and.present(VAR)) ch=STRING_pack("*ERR* ",VAR)
       else if (present(WARN)) then
         if (WARN.and.i_err/=0.and.present(VAR)) ch=STRING_pack("*WRN* ",VAR)
       endif
     endif
     if (len_trim(ch)>0) then
       call msg(msg_where,trim(ch),trim(db_ch(1)))
     else if (present(VAR)) then
       call msg(msg_where,VAR,trim(db_ch(1)))
     endif
   endif
   !
   ! DESCRIPTION
   !
   if (present(DESCRIPTION)) then
     DESCRIPTION=VAR//trim(db_ch(1))
   endif
   !
   io_rec_pos(ID)=io_rec_pos(ID)+len(db_ch(1))
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
     enddo
     !
     do i1=1,size(cv1)
       if (OP(i1)=="==" .and. abs(cv1(i1)-cv2(i1))>eq_accuracy) c_verify=IO_INCOMPATIBLE_VAR
       if (OP(i1)=="<"  .and. abs(cv1(i1))>=abs(cv2(i1))      ) c_verify=IO_INCOMPATIBLE_VAR
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
     if (.not.present(OP) .and. trim(chv1)/=trim(chv2)) ch_verify=IO_INCOMPATIBLE_VAR
     !
     if (     present(OP)) then
       if (OP(1)=="==" .and. trim(chv2)/=trim(chv1)              )  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)=="<"  .and. index(trim(chv2),trim(chv1))==0     )  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)=="<=" .and. index(trim(chv2),trim(chv1))==0     )  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)==">"  .and. index(trim(chv2),trim(chv1))==0     )  ch_verify=IO_INCOMPATIBLE_VAR
       if (OP(1)==">=" .and. index(trim(chv2),trim(chv1))==0     )  ch_verify=IO_INCOMPATIBLE_VAR
     endif
     !
   end function
   !
end subroutine
