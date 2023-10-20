!
! License-Identifier: GPL
!
! Copyright (C) 2022 The Yambo Team
!
! Authors (see AUTHORS file for details): AM
!
subroutine DESC_compare(D1,D2,i_err,exclude,only)
 !
 use descriptors,   ONLY:IO_desc
 use stderr,        ONLY:STRING_same,STRING_match
 !
 implicit none
 !
 type(IO_desc),          intent(in) :: D1,D2
 integer                            :: i_err(2)
 character(*), optional, intent(in) :: exclude
 character(*), optional, intent(in) :: only
 !
 ! Work Space
 !
 integer           :: i_d,i_v,i_dp
 !
 i_err=0
 !
 if (D1%n/=D2%n.and..not.present(exclude).and..not.present(only)) then
   i_err=1
   return
 endif
 do i_d=1,D1%n
   if (present(exclude)) then
     if (STRING_match(D1%str(i_d),exclude)) cycle
   endif
   if (present(only)) then
     if (.not.STRING_match(D1%str(i_d),only)) cycle
   endif
   do i_dp=1,D2%n
     if (.not.STRING_same(D1%str(i_d),D2%str(i_dp))) cycle
     do i_v=1,D1%size(i_d)
       if (D1%kind(i_d)=='i'.and.D1%ival(i_v,i_d)/=D2%ival(i_v,i_dp)) i_err=(/i_d,i_dp/)
       if (D1%kind(i_d)=='r'.and.D1%rval(i_v,i_d)/=D2%rval(i_v,i_dp)) i_err=(/i_d,i_dp/)
     enddo
     if (D1%kind(i_d)=='d'.and.real(D1%dval(i_d))/=real(D2%dval(i_dp))) i_err=(/i_d,i_dp/)
     if (D1%kind(i_d)=='c') then
       if (real(D1%cval(i_d)) /= real(D2%cval(i_dp))) i_err=(/i_d,i_dp/)
       if (aimag(D1%cval(i_d))/=aimag(D2%cval(i_dp))) i_err=(/i_d,i_dp/)
     endif
     if (D1%kind(i_d)=='s'.and..not.STRING_same(D1%sval(i_d),D2%sval(i_dp))) i_err=(/i_d,i_dp/)
     if (D1%kind(i_d)=='l'.and.D1%lval(i_d).neqv.D2%lval(i_dp)) i_err=(/i_d,i_dp/)
   enddo
 enddo
 !
end subroutine DESC_compare
