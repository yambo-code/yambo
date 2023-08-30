!
! License-Identifier: GPL
!
! Copyright (C) 2020 The Yambo Team
!
! Authors (see AUTHORS file for details): AM DS
!
subroutine DESC_write(dest,prefix,desc,d_range,skip_patt)
 !
 use pars,       ONLY:SP,msg_len
 use com,        ONLY:msg,com_compose_msg
 use descriptors,ONLY:IO_desc
 !
 implicit none
 !
 character(*),           intent(in) :: dest
 character(*),           intent(in) :: prefix
 type(IO_desc),          intent(in) :: desc
 integer, optional,      intent(in) :: d_range(2)
 character(*), optional, intent(in) :: skip_patt
 !
 ! Work Space
 !
 integer           :: i_d,n_val,i_space,var_len,range_(2)
 character(msg_len):: local_var,local_term
 character(1)      :: space=" "
 !
 if (desc%n==0) return
 !
 if (dest(1:1)/="s".and.dest(1:1)/="r") call msg(dest,prefix)
 !
 i_space=0
 if (index(dest,"o ")>0) i_space=1
 !
 range_=(/1,desc%n/)
 if (present(d_range)) then
   range_=d_range
 endif
 !
 var_len=msg_len-i_space-len(prefix)-1
 !
 do i_d=range_(1),range_(2)
   !
   if (present(skip_patt)) then
     if (index(desc%str(i_d),skip_patt)>0) cycle
   endif
   !
   n_val=desc%size(i_d)
   local_var=com_compose_msg(pre=prefix//space(1:i_space),body=trim(desc%str(i_d)))
   local_term=""
   if (len_trim(desc%term(i_d))>0) then
     local_term=trim(desc%term(i_d))
     if (index(desc%term(i_d),"[")==0) local_term="["//trim(desc%term(i_d))//"]"
   endif
   if (n_val==0) call msg(dest,local_var)
   if (n_val> 0) then
     if(desc%kind(i_d)=='i') call msg(dest,local_var,desc%ival(1:n_val,i_d),INDENT=0,after_msg=local_term)
     if(desc%kind(i_d)=='r') call msg(dest,local_var,desc%rval(1:n_val,i_d),INDENT=0,after_msg=local_term)
     if(desc%kind(i_d)=='d') call msg(dest,local_var,real(desc%dval(i_d),SP),INDENT=0,after_msg=local_term)
     if(desc%kind(i_d)=='c') call msg(dest,local_var,&
&                                     (/real(desc%cval(i_d)),aimag(desc%cval(i_d))/),INDENT=0,after_msg=local_term)
     if(desc%kind(i_d)=='s') call msg(dest,local_var,trim(desc%sval(i_d)),INDENT=0,after_msg=local_term)
     if(desc%kind(i_d)=='l') then
       if(     desc%lval(i_d)) call msg(dest,local_var,'yes',INDENT=0,after_msg=local_term)
       if(.not.desc%lval(i_d)) call msg(dest,local_var,'no',INDENT=0,after_msg=local_term)
     endif
   endif
 enddo
 !
 if (dest(1:1)/="s".and.dest(1:1)/="r") call msg(dest,prefix)
 !
end subroutine DESC_write
