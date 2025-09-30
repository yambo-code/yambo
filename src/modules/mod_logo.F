!
! License-Identifier: GPL
!
! Copyright (C) 2008 The Yambo Team
! Copyright (C) 2025 The Lumen Team
!
! Authors (see AUTHORS file for details): AM,AC
!
module LOGO
 !
 use pars,     ONLY:schlen
 ! 
 implicit none
 !
 integer                     :: n_logo_lines
 integer                     :: ID_logo
 integer                     :: ID_logo_stderr
 integer, parameter, private :: max_n_logo_lines=100
 character(70)               :: logo_line(max_n_logo_lines)
 !
 contains
   !
   integer function pickup_a_random(rand_range)
     use pars,  ONLY:DP,SP
     real(SP) :: rand_range
     !
     ! Work Space
     !
     character(12)      :: ch(3)
     integer            :: iseed(8)
     real(DP), external :: dlaran
     call date_and_time(ch(1),ch(2),ch(3),iseed)
     iseed=iabs(iseed)
     !
     ! iseed(4) must be odd
     !
     iseed(4)=2*(iseed(4)/2)+1
     !
     pickup_a_random=int(dlaran(iseed(4:))*rand_range)
   end function
   !
   subroutine pickup_a_logo(unit_)
     !
     use C_driver, ONLY:code_version,code_revision,code_hash,code_libraries,code_branch
     use pars,     ONLY:SP
     use stderr,   ONLY:intc,slash
     !
     integer      :: unit_
     ! 
     ! Work Space
     !
     character(schlen) :: comment(4),ch
     integer           :: max_length,i1,left_space,iA,ic
     !
     comment(1)='Version '//&
&                trim(intc(code_version(1)))//'.'//trim(intc(code_version(2)))//&
&                '.'//trim(intc(code_version(3)))//' Revision '//trim(intc(code_revision))//' Hash (prev commit) '//trim(code_hash)
     !
     comment(2)="Branch is "//trim(code_branch)
     !
     comment(3)=trim(code_libraries)//" Build"
     !
     comment(4)='http://www.lumen-code.org'
     !
     ID_logo_stderr=1
     ID_logo       =1
     !
     n_logo_lines=9
     !
     logo_line( 1)='                                                     '
     logo_line( 2)='$$\                                                  '
     logo_line( 3)='$$ |                                                 '
     logo_line( 4)='$$ |     $$\   $$\ $$$$$$\$$$$\   $$$$$$\  $$$$$$$\  '
     logo_line( 5)='$$ |     $$ |  $$ |$$  _$$  _$$\ $$  __$$\ $$  __$$\ '
     logo_line( 6)='$$ |     $$ |  $$ |$$ / $$ / $$ |$$$$$$$$ |$$ |  $$ |'
     logo_line( 7)='$$ |     $$ |  $$ |$$ | $$ | $$ |$$   ____|$$ |  $$ |'
     logo_line( 8)='$$$$$$$$\\$$$$$$  |$$ | $$ | $$ |\$$$$$$$\ $$ |  $$ |'
     logo_line( 9)='\________|\______/ \__| \__| \__| \_______|\__|  \__|' 
     !
     ! Replace "S" with "\" and find the max length of 
     ! the lines to center the two comments
     !
     do i1=1,n_logo_lines
       iA=index(logo_line(i1),'S')
       if (iA==0) cycle
       ch=logo_line(i1)
       do while (index(ch,'S')/=0) 
         iA=index(ch,'S')
         ch(iA:iA)=slash
       enddo
       logo_line(i1)=ch
     enddo
     !
     max_length=-1
     do i1=1,n_logo_lines
       max_length=max( len_trim(logo_line(i1)) , max_length)
     enddo
     !
     n_logo_lines=n_logo_lines+1
     logo_line(n_logo_lines)=' '
     !
     do ic=1,4
       n_logo_lines=n_logo_lines+1
       logo_line(n_logo_lines)=trim(comment(ic))
       left_space=(max_length-len_trim(comment(ic)))/2+1
       if (left_space>0) then
         write (logo_line(n_logo_lines),'(t'//trim(intc(left_space))//',a)') trim(comment(ic))
       endif
     enddo
     !
   end subroutine
   !
end module LOGO
