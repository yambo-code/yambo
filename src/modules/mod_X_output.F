!
! Copyright (C) 2000-2013 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
module X_output
 !
 use pars,    ONLY:schlen,SP
 use stderr,  ONLY:intc
 use com,     ONLY:msg
 use X_m,     ONLY:alpha_dim
 use fields,  ONLY:global_gauge
#if defined _KERR
 use drivers,   ONLY:l_kerr
#endif
 implicit none
 !
 character(15)     :: headers(7)
 character(schlen) :: eps_file_name,eel_file_name,alpha_file_name,fxc_file_name
 character(schlen) :: off_file_name,beta_file_name,moke_file_name
 !
 contains
   !
   subroutine X_write_messages_before_headers(iq,GreenF,Vnl_included,ordering)
     use drivers,   ONLY:l_chi,l_bss
     use global_XC, ONLY:X_E_xc_string,X_WF_xc_string,K_E_xc_string,K_WF_xc_string
     integer      :: iq
     logical      :: Vnl_included,GreenF
     character(1) :: ordering
     ! WS
     character(schlen) :: message
     !
     ! T-ordering
     !
     call msg('o eps eel alpha off beta moke','#')
     if (GreenF) then
       if (l_chi) call msg('o eps eel alpha','# ','- G. Functions  are '//trim(X_E_xc_string(4)),INDENT=0)
       if (l_bss) call msg('o eps eel alpha off beta moke','# ','- G. Functions  are '//trim(K_E_xc_string),INDENT=0)
     else
       if (l_chi) call msg('o eps eel alpha','# ','- Energies      are '//trim(X_E_xc_string(4)),INDENT=0)
       if (l_bss) call msg('o eps eel alpha off beta moke','# ','- Energies      are '//trim(K_E_xc_string),INDENT=0)
     endif
     if (l_chi) call msg('o eps eel alpha','# ','- Wavefunctions are '//trim(X_WF_xc_string(4)),INDENT=0)
     if (l_bss) call msg('o eps eel alpha off beta moke','# ','- Wavefunctions are '//trim(K_WF_xc_string),INDENT=0)
     call msg('o eps eel alpha off beta moke','# ')
     message='- The Green`s function is T-ordered -'
     if (trim(ordering)=='a') message='- The Green`s function is antiresonant -'
     if (trim(ordering)=='r') message='- The Green`s function is resonant -'
     if (trim(ordering)=='c') message='- The Green`s function is causal -'
     call msg('o eps eel alpha off beta moke','# ',trim(message),INDENT=0)
     !
     ! Gauges
     !
     if (iq==1) then
       if (trim(global_gauge)=='length')         message='- Using the Length Gauge -'
       if (trim(global_gauge)=='velocity')       message='- Using the Velocity Gauge -'
       call msg('o eps eel alpha off beta moke','# ',trim(message),INDENT=0)
       message='- [r,Vnl] is *NOT* included -'
       if (Vnl_included) message='- [r,Vnl] *is* included -'
       call msg('o eps eel alpha off beta moke','# ',trim(message),INDENT=0)
     endif
     !
   end subroutine
   !
   subroutine X_write_descriptions(n_descs,desc)
     integer      :: n_descs,i_d
     character(*) :: desc(n_descs)
     call msg('o eps eel fxc alpha off beta moke','#')    
     do i_d=1,n_descs
       call msg('o eps eel fxc alpha off beta moke','#  ',trim(desc(i_d)),INDENT=0)    
     enddo
     call msg('o eps eel fxc alpha off beta moke','#')    
   end subroutine
   !
   subroutine X_write_q_plus_G(iq,Q_plus_G_pt,ig)
     !
     integer           :: iq
     integer,optional  :: ig
     real(SP)          :: Q_plus_G_pt(3)
     character(schlen) :: message(7)
     !
     if (iq==1.and.ig==1) then
       message(1) = '# Absorption @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(2) = '# EELS @ Q('//trim(intc(iq))//')  [q->0 direction]:'
       message(3) = '# Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//') [q->0 direction]'
       message(4) = '# F_xc @ Q('//trim(intc(iq))//')  [q->0 direction]:'
       message(5) = '# Off-diagonal Absorption @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(6) = '# Off-diagonal Polarizability @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(7) = '# moke parameters @ Q('//trim(intc(iq))//') [q->0 direction] :'
     else  if (iq/=1.and.ig==1) then
       message(1) = '# Absorption @ Q('//trim(intc(iq))//') [iku] :'
       message(2) = '# EELS @ Q('//trim(intc(iq))//')  [iku]:'
       message(3) = '# Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//') [iku]'
       message(4) = '# F_xc @ Q('//trim(intc(iq))//')  [iku]:'
     else  if (ig/=1) then
       message(1) = '# Absorption @ Q('//trim(intc(iq))//') + G('//trim(intc(ig))//') [iku] :'
       message(2) = '# EELS @ Q('//trim(intc(iq))//')  + G('//trim(intc(ig))//') [iku]:'
       message(3) = '# Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//')  + G('//trim(intc(ig))//') [iku]'
       message(4) = '# F_xc @ Q('//trim(intc(iq))//')  [iku]:'
     endif
     call msg("o eps_q",trim(message(1)),Q_plus_G_pt,INDENT=0)
     call msg("o eel_q",trim(message(2)),Q_plus_G_pt,INDENT=0)
     call msg("o alpha",trim(message(3)),Q_plus_G_pt,INDENT=0)
     call msg("o fxc",trim(message(4)),Q_plus_G_pt,INDENT=0)
     call msg("o off",trim(message(5)),Q_plus_G_pt,INDENT=0)
     call msg("o beta",trim(message(6)),Q_plus_G_pt,INDENT=0)
     call msg("o moke",trim(message(7)),Q_plus_G_pt,INDENT=0)
     call msg('o eps eel fxc alpha off moke','#')    
     !
   end subroutine
   !
   subroutine X_setup_file_names(iq,solver,approx,equation,ig)
     !
     integer                :: iq
     character(*)           :: solver,approx,equation
     integer,      optional :: ig
     !
     fxc_file_name=  'fxc_q'//trim(intc(iq))
     ! 
     eps_file_name  ='eps_q'//trim(intc(iq))
     eel_file_name  ='eel_q'//trim(intc(iq))
     alpha_file_name='alpha_q'//trim(intc(iq))
     !
     off_file_name  ='off_q'//trim(intc(iq))
     beta_file_name ='beta_q'//trim(intc(iq))
     moke_file_name ='moke_q'//trim(intc(iq))
     !
     if (present(ig)) then
       if (ig>1) then
         eps_file_name  = trim(eps_file_name)//'_G'//trim(intc(ig))
         eel_file_name  = trim(eel_file_name)//'_G'//trim(intc(ig))
         alpha_file_name= trim(alpha_file_name)//'_G'//trim(intc(ig))
       endif
     endif
     !
     if (len_trim(solver)>0) then
       eps_file_name  =  trim(eps_file_name)//'_'//solver
       eel_file_name  =  trim(eel_file_name)//'_'//solver
       alpha_file_name=  trim(alpha_file_name)//'_'//solver
       off_file_name  =  trim(off_file_name)//'_'//solver
       beta_file_name =  trim(beta_file_name)//'_'//solver
       moke_file_name =  trim(moke_file_name)//'_'//solver
     endif
     !
     if (len_trim(approx)>0) then
       eps_file_name  =  trim(eps_file_name)//'_'//approx
       eel_file_name  =  trim(eel_file_name)//'_'//approx
       alpha_file_name=  trim(alpha_file_name)//'_'//approx
       off_file_name  =  trim(off_file_name)//'_'//approx
       beta_file_name =  trim(beta_file_name)//'_'//approx
       moke_file_name =  trim(moke_file_name)//'_'//approx
     endif
     !
     if (len_trim(equation)>0) then
       eps_file_name  =  trim(eps_file_name)//'_'//equation
       eel_file_name  =  trim(eel_file_name)//'_'//equation
       alpha_file_name=  trim(alpha_file_name)//'_'//equation
       off_file_name  =  trim(off_file_name)//'_'//equation
       beta_file_name =  trim(beta_file_name)//'_'//equation
       moke_file_name =  trim(moke_file_name)//'_'//equation
     endif
     !
#if defined _KERR
     if (.not.l_kerr) then
#endif
       off_file_name=' '
       beta_file_name=' '
       moke_file_name=' '
#if defined _KERR
     endif
#endif
     !
   end subroutine
   !
   !
   subroutine X_setup_headers(desc)
     !
     character(*),  intent(in) :: desc
     ! 
     ! Work space
     ! 
     integer :: ii
     character(10) :: suffix(6)
     !
     suffix(:6) = (/' /Im[2]',' /Re[3]','0/Im[4]','0/Re[5]','`/Im[6]','`/Re[7]'/)
     headers(1) = 'E/ev[1]'
     !
     do ii = 1,6
       headers(ii+1)=trim(desc)//trim(suffix(ii))
     enddo
     !
   end subroutine
   !
end module
