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
     ! GPL_EXCLUDE_START
     !
     comment(1)='Version '//&
&                trim(intc(code_version(1)))//'.'//trim(intc(code_version(2)))//&
&                '.'//trim(intc(code_version(3)))//' Revision '//trim(intc(code_revision))//' Hash '//trim(code_hash)
     !
     ! GPL_EXCLUDE_END
     !
     ! GPL_INCLUDE_START
!     comment(1)='GPL Version '//&
!&                trim(intc(code_version(1)))//'.'//trim(intc(code_version(2)))//&
!&                '.'//trim(intc(code_version(3)))//' Revision '//trim(intc(code_GPL_revision))//&
!&                '.'//' (Based on r.'//trim(intc(code_revision))//' h.'//trim(code_hash)//')'
     ! GPL_INCLUDE_END
     !
     ! GPL_EXCLUDE_START
     comment(2)="Branch is "//trim(code_branch)
     ! GPL_EXCLUDE_END
     !
     ! GPL_INCLUDE_START
     !comment(2)=""
     ! GPL_INCLUDE_END
     !
     comment(3)=trim(code_libraries)//" Build"
     !
     comment(4)='http://www.yambo-code.org'
     !
     if (ID_logo<0) then
       ID_logo=pickup_a_random(10._SP)
       ID_logo_stderr=10+pickup_a_random(6._SP)
       if (ID_logo==0) ID_logo=1
       if (ID_logo_stderr==0) ID_logo_stderr=1
     endif
     !
     if (unit_==6) i1=ID_logo_stderr
     if (unit_/=6) i1=ID_logo
     !
     n_logo_lines=9
     !
     select case(i1)
       case(1)
         logo_line( 1)='                                                        '
         logo_line( 2)=' __  __   ________   ___ __ __    _______   ______      '
         logo_line( 3)='/_/S/_/S /_______/S /__//_//_/S /_______/S /_____/S     '
         logo_line( 4)='S S S S SS::: _  S SS::S| S| S SS::: _  S SS:::_ S S    '
         logo_line( 5)=' S:S_S S SS::(_)  S SS:.      S SS::(_)  S/_S:S S S S   '
         logo_line( 6)='  S::::_S/ S:: __  S SS:.S-/S  S SS::  _  S SS:S S S S  '
         logo_line( 7)='    S::S S  S:.S S  S SS. S  S  S SS::(_)  S SS:S_S S S '
         logo_line( 8)='     S__S/   S__S/S__S/ S__S/ S__S/ S_______S/ S_____S/ '
         logo_line( 9)='                                                        '
       case(2)
         logo_line( 1)='                                                         '
         logo_line( 2)=' ____  ____     _       ____    ____  ______      ___    '
         logo_line( 3)='|_  _||_  _|   / S     |_   S  /   _||_   _ S   ."   `.  '
         logo_line( 4)='  S S  / /    / _ S      |   S/   |    | |_) | /  .-.  S '
         logo_line( 5)='   S S/ /    / ___ S     | |S  /| |    |  __". | |   | | '
         logo_line( 6)='   _|  |_  _/ /   S S_  _| |_S/_| |_  _| |__) |S  `-"  / '
         logo_line( 7)='  |______||____| |____||_____||_____||_______/  `.___."  '
         logo_line( 8)='                                                         '
         logo_line( 9)='                                                         '
       case(3)
         logo_line( 1)='                                                       '
         logo_line( 2)='  __   __     _        __  __       ____      U  ___  u'
         logo_line( 3)='  S S / / U  /"S  U u |" S/ "| u U | __") u    S/"_ S/ '
         logo_line( 4)='   S V /   S/ _ S/   S| |S/| |/   S|  _ S/     | | | | '
         logo_line( 5)='  U_|"|_u  / ___ S    | |  | |     | |_) | .-,_| |_| | '
         logo_line( 6)='    |_|   /_/   S_S   |_|  |_|     |____/   S_)-S___/  '
         logo_line( 7)='.-,//|(_   SS    >>  <<,-,,-.     _|| SS_        SS    '
         logo_line( 8)=' S_) (__) (__)  (__)  (./  S.)   (__) (__)      (__)   '
         logo_line( 9)='                                                       '
       case(4)
         logo_line( 1)='                                                       '
         logo_line( 2)='                                                       '
         logo_line( 3)='Y88b    /   e           e    e      888~~S    ,88~-_   '
         logo_line( 4)=' Y88b  /   d8b         d8b  d8b     888   |  d888   S  '
         logo_line( 5)='  Y88b/   /Y88b       d888bdY88b    888 _/  88888    | '
         logo_line( 6)='   Y8Y   /  Y88b     / Y88Y Y888b   888  S  88888    | '
         logo_line( 7)='    Y   /____Y88b   /   YY   Y888b  888   |  Y888   /  '
         logo_line( 8)='   /   /      Y88b /          Y888b 888__/    `88_-~   '
         logo_line( 9)='                                                       '
       case(5)
         logo_line( 1)='                                                       '
         logo_line( 2)=' **    **    **     ****     ****  ******     *******  '
         logo_line( 3)='//**  **    ****   /**/**   **/** /*////**   **/////** '
         logo_line( 4)=' //****    **//**  /**//** ** /** /*   /**  **     //**'
         logo_line( 5)='  //**    **  //** /** //***  /** /******  /**      /**'
         logo_line( 6)='   /**   **********/**  //*   /** /*//// **/**      /**'
         logo_line( 7)='   /**  /**//////**/**   /    /** /*    /**//**     ** '
         logo_line( 8)='   /**  /**     /**/**        /** /*******  //*******  '
         logo_line( 9)='   //   //      // //         //  ///////    ///////   '
       case(6)
         logo_line( 1)='                                                       '
         logo_line( 2)=':   :::   :::     ::::    ::::  :::::::::   ::::::::  '
         logo_line( 3)=':+:   :+: :+: :+:   +:+:+: :+:+:+ :+:    :+: :+:    :+ '
         logo_line( 4)=' +:+ +:+ +:+   +:+  +:+ +:+:+ +:+ +:+    +:+ +:+    +:+ '
         logo_line( 5)='  +#++: +#++:++#++: +#+  +:+  +#+ +#++:++#+  +#+    +:+ '
         logo_line( 6)='   +#+  +#+     +#+ +#+       +#+ +#+    +#+ +#+    +#+ '
         logo_line( 7)='   #+#  #+#     #+# #+#       #+# #+#    #+# #+#    #+# '
         logo_line( 8)='   ###  ###     ### ###       ### #########   ########  '
         logo_line( 9)='                                                       '
       case(7)
         logo_line( 1)='                                                       '
         logo_line( 2)=' /$$     /$$ /$$$$$$  /$$      /$$ /$$$$$$$   /$$$$$$ '
         logo_line( 3)='|  $$   /$$//$$__  $$| $$$    /$$$| $$__  $$ /$$__  $$'
         logo_line( 4)=' S  $$ /$$/| $$  S $$| $$$$  /$$$$| $$  S $$| $$  S $$'
         logo_line( 5)='  S  $$$$/ | $$$$$$$$| $$ $$/$$ $$| $$$$$$$ | $$  | $$'
         logo_line( 6)='   S  $$/  | $$__  $$| $$  $$$| $$| $$__  $$| $$  | $$'
         logo_line( 7)='    | $$   | $$  | $$| $$S  $ | $$| $$  S $$| $$  | $$'
         logo_line( 8)='    | $$   | $$  | $$| $$ S/  | $$| $$$$$$$/|  $$$$$$/'
         logo_line( 9)='    |__/   |__/  |__/|__/     |__/|_______/  S______/ '
       case(8)
         logo_line( 1)=' __      __   ______   __       __  _______    ______  '
         logo_line( 2)='|  S    /  S /      S |  S     /  S|       S  /      S '
         logo_line( 3)=' S$$S  /  $$|  $$$$$$S| $$S   /  $$| $$$$$$$S|  $$$$$$S'
         logo_line( 4)='  S$$S/  $$ | $$__| $$| $$$S /  $$$| $$__/ $$| $$  | $$'
         logo_line( 5)='   S$$  $$  | $$    $$| $$$$S  $$$$| $$    $$| $$  | $$'
         logo_line( 6)='    S$$$$   | $$$$$$$$| $$S$$ $$ $$| $$$$$$$S| $$  | $$'
         logo_line( 7)='    | $$    | $$  | $$| $$ S$$$| $$| $$__/ $$| $$__/ $$'
         logo_line( 8)='    | $$    | $$  | $$| $$  S$ | $$| $$    $$ S$$    $$'
         logo_line( 9)='     S$$     S$$   S$$ S$$      S$$ S$$$$$$$   S$$$$$$ '
       case(9)
         logo_line( 1)='                                                          '
         logo_line( 2)='ooooo   oooo ..     ooo        ooo ooooooooo.    .oooo.   '
         logo_line( 3)=' `88.   .8" .88.    `88.       .88 `88"   `Y8b  dP"  `Yb  '
         logo_line( 4)='  `88. .8" .8"88.    888b     d"88  88     888 88      88 '
         logo_line( 5)='   `88.8" .8" `88.   8 Y88. .P  88  88oooo888" 88      88 '
         logo_line( 6)='    `88" .88ooo888.  8  `888"   88  88    `88b 88      88 '
         logo_line( 7)='     88 .8"     `88. 8    Y     88  88    .88P `8b    d8" '
         logo_line( 8)='    o88o88o      888o8          88 o88bood8P"   `Ybod8P"  '
         logo_line( 9)='                                                          '
       case(10)
         logo_line( 1)='                                                          '
         logo_line( 2)='____    ____    ___      .___  ___.  .______     ______   '
         logo_line( 3)='S   S  /   /   /   S     |   S/   |  |   _  S   /  __  S  '
         logo_line( 4)=' S   S/   /   /  ^  S    |  S  /  |  |  |_)  | |  |  |  | '
         logo_line( 5)='  S_    _/   /  /_S  S   |  |S/|  |  |   _  <  |  |  |  | '
         logo_line( 6)='    |  |    /  _____  S  |  |  |  |  |  |_)  | |  `--"  | '
         logo_line( 7)='    |__|   /__/     S__S |__|  |__|  |______/   S______/  '
         logo_line( 8)='                                                          '
         logo_line( 9)='                                                          '
       case(11)
         logo_line( 1)='                                     '
         logo_line( 2)=' ___ __  _____  __ __  _____   _____ '
         logo_line( 3)='|   Y  ||  _  ||  Y  ||  _  S |  _  |'
         logo_line( 4)='|   |  ||. |  ||.    ||. |  / |. |  |'
         logo_line( 5)=' S_  _/ |. _  ||.S_/ ||. _  S |. |  |'
         logo_line( 6)='  |: |  |: |  ||: |  ||: |   S|: |  |'
         logo_line( 7)='  |::|  |:.|:.||:.|:.||::.   /|::.  |'
         logo_line( 8)='  `--"  `-- --"`-- --"`-----" `-----"'
         logo_line( 9)='                                     '
       case(12)
         logo_line( 1)='                                                  '
         logo_line( 2)='                                                  '
         logo_line( 3)='_|      _|   _|_|   _|      _|  _|_|_|     _|_|   '
         logo_line( 4)='  _|  _|   _|    _| _|_|  _|_| _|    _| _|    _|  '
         logo_line( 5)='    _|     _|_|_|_| _|  _|  _| _|_|_|   _|    _|  '
         logo_line( 6)='    _|     _|    _| _|      _| _|    _| _|    _|  '
         logo_line( 7)='    _|     _|    _| _|      _| _|_|_|     _|_|    '
         logo_line( 8)='                                                  '
         logo_line( 9)='                                                  '
       case(13)
         logo_line( 1)='                                   '
         logo_line( 2)='  __ __  ____ ___ ___ ____   ___   '
         logo_line( 3)=' |  |  |/    |   |   |    S /   S  '
         logo_line( 4)=' |  |  |  o  | _   _ |  o  )     | '
         logo_line( 5)=' |  ~  |     |  S_/  |     |  O  | '
         logo_line( 6)=' |___, |  _  |   |   |  O  |     | '
         logo_line( 7)=' |     |  |  |   |   |     |     | '
         logo_line( 8)=' |____/|__|__|___|___|_____|S___/  '
         logo_line( 9)='                                   '
       case(14)
         logo_line( 1)='                                              '
         logo_line( 2)=' __    __ ______           ____     _____     '
         logo_line( 3)='/S S  /S SS  _  S  /"S_/`S/S  _`S /S  __`S    '
         logo_line( 4)='S `S`SS/"/ S SLS S/S      S S SLS S S S/S S   '
         logo_line( 5)=' `S `S /" S S  __ S S S__S S S  _ <" S S S S  '
         logo_line( 6)='   `S S S  S S S/S S S S_/S S S SLS S S S_S S '
         logo_line( 7)='     S S_S  S S_S S_S S_SS S_S S____/S S_____S'
         logo_line( 8)='      S/_/   S/_/S/_/S/_/ S/_/S/___/  S/_____/'
         logo_line( 9)='                                             '
       case(15)
         logo_line( 1)='                                             '
         logo_line( 2)=' __   __  _______  __   __  _______  _______ '
         logo_line( 3)='|  | |  ||   _   ||  |_|  ||  _    ||       |'
         logo_line( 4)='|  |_|  ||  |_|  ||       || |_|   ||   _   |'
         logo_line( 5)='|       ||       ||       ||       ||  | |  |'
         logo_line( 6)='|_     _||       ||       ||  _   | |  |_|  |'
         logo_line( 7)='  |   |  |   _   || ||_|| || |_|   ||       |'
         logo_line( 8)='  |___|  |__| |__||_|   |_||_______||_______|'
         logo_line( 9)='                                             '
       case(16)
         logo_line( 1)='                                             '
         logo_line( 2)='   ___      ___      ___      ___      ___   '
         logo_line( 3)='  /S__S    /S  S    /S__S    /S  S    /S  S  '
         logo_line( 4)=' |::L__L  /::S  S  /::L_L_  /::S  S  /::S  S '
         logo_line( 5)=' |:::S__S/::S:S__S/:/L:S__S/::S:S__S/:/S:S__S'
         logo_line( 6)=' /:;;/__/S/S::/  /S/_/:/  /S:S::/  /S:S/:/  /'
         logo_line( 7)=' S/__/     /:/  /   /:/  /  S::/  /  S::/  / '
         logo_line( 8)='           S/__/    S/__/    S/__/    S/__/  '
         logo_line( 9)='                                             '
     end select
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
