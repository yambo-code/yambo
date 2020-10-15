!
!        Copyright (C) 2000-2020 the YAMBO team
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
subroutine PH_of( )
 !
 use pars,      ONLY:lchlen,schlen
 use com,       ONLY:of_open_close
&                    QP_n_states,&
&                    &
&                    &
&                    Time_of
 !
 implicit none
 !
 ! Work Space
 !
 character(lchlen) ::file_name
 logical           ::state_is_2do(QP_n_states)
 !
 !o.qp titles
 !
 integer, parameter :: tpar=31
 integer            :: ntitles,titles_order(tpar)
 character(schlen)  :: titles(tpar)
 !
 titles(1)  = 'Sc|Eo' 
 titles(2)  = 'Sc|E' 
 titles(3)  = 'Eo'  
 titles(4)  = 'E' 
 titles(5)  = 'Z' 
 titles(6)  = 'dSc/dw|Eo' 
 titles(7)  = 'E-Eo' 
 titles(8)  = 'Q-point' 
 titles(9) = 'Branch' 
 titles(10) = 'Width[meV]' 
 titles(13) = 'Width[fs]' 
 titles(15) = 'Z(Re)' 
 titles(16) = 'Z(Im)' 
 titles(27) = 'Sc|Eo(meV)' 
 titles(28) = 'Width[ps]' 
 titles(30) = 'Width[ns]'
 !
 ntitles=5
 titles_order(:ntitles)=(/9,10,4,8,2/)
 !
! if (l_extended_output) then
!   !
!   ntitles=14
!   titles_order(:ntitles)=(/9,10,4,5,8,1,11,2,3,7,15,16,12,13/)
!   !
!   ! When QP components are read from DB some of the variabels are not available
!   !
!   if (QPdb_read_err==0) then
!     ntitles=11
!     titles_order(:ntitles)=(/9,10,4,5,8,1,11,15,16,12,13/)
!   endif
!   !
! endif
 !
 file_name="ph"
 call of_open_close(file_name,'ot')
 !
 !call DESC_write('o '//what,'#',qp%desc)
 !
 !call msg('o '//what,'#')
 !call msg('o '//what,'#',titles(titles_order(:ntitles)),INDENT=0,USE_TABS=.TRUE.)
 !call msg('o '//what,'#')
 !
 !do i1=1,QP_n_states
 !  !
 !  values=0.
 !  qp_b  =QP_table(i1,1)
 !  qp_k  =QP_table(i1,3)
 !  qp_isp=spin(QP_table(i1,:))
 !  !
 !  if (index(what,"qp")>0) then
 !    !
 !    if (.not.l_life) then
 !        if (report_Sc) values(2)=real(QP_Sc(i1,1))*HA2EV
 !        if (report_dSc) then
 !          values(7:8)=(/real(QP_dSc(i1,1)),real((qp%E(i1)-qp%E_bare(i1))*HA2EV)/) 
 !          values(3  )=real(QP_Sc(i1,1)+QP_dSc(i1,1)*(qp%E(i1)-qp%E_bare(i1)))*HA2EV
 !        else
 !          values(8)=real(qp%E(i1)-qp%E_bare(i1))*HA2EV
 !        endif
 !      if (report_Vxc)     values(1) =real(QP_Vxc(i1))*HA2EV
 !      if (report_Vnlxc)   values(11)=real(QP_Vnl_xc(i1))*HA2EV
 !    endif
 !    !
 !    values(4:6)  =(/qp%E_bare(i1)*HA2EV,real(qp%E(i1)*HA2EV),real(qp%Z(i1))/)
 !    values(9:10) =(/real(qp_k),real(qp_b)/)
 !    values(12:13)=(/aimag(qp%E(i1))*HA2EV/1.E-3,Time_of(aimag(qp%E(i1)),"fs")/)
 !    values(28)   =Time_of(aimag(qp%E(i1)),"ps")
 !    values(15:16)=(/real(qp%Z(i1)),aimag(qp%Z(i1))/)
 !    !
 !    if (allocated(QP_G_amplitude_integral)) then
 !      values(23)=QP_G_amplitude_integral(i1)
 !    endif
 !    if (allocated(QP_G_info)) then
 !      values(17:18)=QP_G_info(i1,1,:)*HA2EV
 !      values(19:20)=QP_G_info(i1,2,:)*HA2EV
 !      values(21:22)=QP_G_info(i1,3,:)*HA2EV
 !    endif
 !    !
 !  else if (what=="hf") then
 !    values(4:5)  =(/qp%E_bare(i1)*HA2EV,real(qp%E(i1)*HA2EV)/)
 !    values(8)=real(qp%E(i1)-qp%E_bare(i1))*HA2EV
 !    values(9:10) =(/real(qp_k),real(qp_b)/)
 !    if (report_Vxc)   values(1)=real(QP_Vxc(i1))*HA2EV
 !    if (report_Vnlxc) values(11)=real(QP_Vnl_xc(i1))*HA2EV
 !    values(26)=values(5)
 !  endif
 !  !
 !  if (qp_isp==1) values(14)= 1._SP
 !  if (qp_isp==2) values(14)=-1._SP
 !  !
 !  call msg('o '//what,'',values(titles_order(:ntitles)),INDENT=-2,USE_TABS=.TRUE.)
 !  !
 !enddo
 !
 call of_open_close(file_name)
 !
end subroutine
