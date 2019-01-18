!
!        Copyright (C) 2000-2018 the YAMBO team
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
module X_output
 !
 use pars,    ONLY:schlen,lchlen,SP
 use stderr,  ONLY:intc
 use com,     ONLY:msg
 use X_m,     ONLY:alpha_dim,global_gauge
 use drivers,   ONLY:l_kerr,l_magnons
 !
 implicit none
 !
 character(15)     :: headers(7)
 character(schlen) :: eps_file_name,eel_file_name,alpha_file_name,fxc_file_name,jdos_file_name,pl_file_name
 character(schlen) :: off_file_name,beta_file_name,moke_file_name,refl_file_name,magp_file_name,magm_file_name
 !
 contains
   !
   subroutine X_write_messages_before_headers(iq,GreenF,Vnl_included,ordering)
     use drivers,   ONLY:l_chi,l_bss
     use global_XC, ONLY:X_E_xc_string,X_WF_xc_string,K_E_xc_string,K_WF_xc_string
     integer      :: iq
     logical      :: Vnl_included,GreenF
     character(2) :: ordering
     ! WS
     character(schlen) :: message
     character(lchlen) :: files_list(2)
     !
     ! T-ordering
     !
     files_list(1)='o eps refl eel alpha off beta moke jdos pmag mmag'
     files_list(2)='o eps refl eel alpha'
     call msg(trim(files_list(1)),'#')
     if (GreenF) then
       if (l_chi) call msg(trim(files_list(2)),'# ','- G. Functions  are '//trim(X_E_xc_string(4)),INDENT=0)
       if (l_bss) call msg(trim(files_list(1)),'# ','- G. Functions  are '//trim(K_E_xc_string),INDENT=0)
     else
       if (l_chi) call msg(trim(files_list(2)),'# ','- Energies      are '//trim(X_E_xc_string(4)),INDENT=0)
       if (l_bss) call msg(trim(files_list(1)),'# ','- Energies      are '//trim(K_E_xc_string),INDENT=0)
     endif
     if (l_chi) call msg(trim(files_list(2)),'# ','- Wavefunctions are '//trim(X_WF_xc_string(4)),INDENT=0)
     if (l_bss) call msg(trim(files_list(1)),'# ','- Wavefunctions are '//trim(K_WF_xc_string),INDENT=0)
     call msg(trim(files_list(1)),'# ')
     message='- The Green`s function is T-ordered -'
     if (trim(ordering)=='Ra') message='- The Green`s function is Retarded-antiresonant -'
     if (trim(ordering)=='Ta') message='- The Green`s function is T-ordered-antiresonant -'
     if (trim(ordering)=='r')  message='- The Green`s function is Resonant -'
     if (trim(ordering)=='R')  message='- The Green`s function is Retarded -'
     call msg(trim(files_list(1)),'# ',trim(message),INDENT=0)
     !
     ! Gauges
     !
     if (iq==1) then
       if (trim(global_gauge)=='length')         message='- Using the Length Gauge -'
       if (trim(global_gauge)=='velocity')       message='- Using the Velocity Gauge -'
       call msg(trim(files_list(1)),'# ',trim(message),INDENT=0)
       message='- [r,Vnl] is *NOT* included -'
       if (Vnl_included) message='- [r,Vnl] *is* included -'
       call msg(trim(files_list(1)),'# ',trim(message),INDENT=0)
     endif
     !
   end subroutine
   !
   subroutine X_write_descriptions(n_descs,desc)
     integer      :: n_descs,i_d
     character(*) :: desc(n_descs)
     character(lchlen) :: files_list
     files_list='o eps refl eel alpha off beta moke jdos pmag mmag'
     call msg(trim(files_list),'#')    
     do i_d=1,n_descs
       call msg(trim(files_list),'#  ',trim(desc(i_d)),INDENT=0)    
     enddo
     call msg(trim(files_list),'#')    
   end subroutine
   !
   subroutine X_write_q_plus_G(iq,Q_plus_G_pt,ig)
     !
     integer           :: iq
     integer,optional  :: ig
     real(SP)          :: Q_plus_G_pt(3)
     character(schlen) :: message(11)
     !
     if (iq==1.and.ig==1) then
       message(1) = '# Absorption @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(2) = '# EELS @ Q('//trim(intc(iq))//')  [q->0 direction]:'
       message(3) = '# Polarizability ( -<<X>> ) ['//trim(alpha_dim)//' @ Q('//trim(intc(iq))//') [q->0 direction]'
       message(4) = '# F_xc @ Q('//trim(intc(iq))//')  [q->0 direction]:'
       message(5) = '# Off-diagonal Absorption @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(6) = '# Off-diagonal Polarizability @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(7) = '# moke parameters @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(8) = '# Reflectivity @ Q('//trim(intc(iq))//') [q->0 direction] :'
       message(9) = '# Joint density of states @ Q('//trim(intc(iq))//')'
       message(10)= '# Magnons (S+-) @ Q('//trim(intc(iq))//')'
       message(11)= '# Magnons (S-+) @ Q('//trim(intc(iq))//')'
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
     call msg("o refl",trim(message(8)),Q_plus_G_pt,INDENT=0)
     call msg("o jdos",trim(message(9)),0.,INDENT=0)
     call msg("o pmag",trim(message(10)),0.,INDENT=0)
     call msg("o mmag",trim(message(11)),0.,INDENT=0)
     call msg('o eps refl eel fxc alpha off moke pmag mmag','#')    
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
     pl_file_name   ='pl_q'//trim(intc(iq))
     eps_file_name  ='eps_q'//trim(intc(iq))
     refl_file_name ='refl_q'//trim(intc(iq))
     eel_file_name  ='eel_q'//trim(intc(iq))
     alpha_file_name='alpha_q'//trim(intc(iq))
     !
     off_file_name  ='off_q'//trim(intc(iq))
     beta_file_name ='beta_q'//trim(intc(iq))
     moke_file_name ='moke_q'//trim(intc(iq))
     !
     jdos_file_name ='jdos_q'//trim(intc(iq))
     !
     magp_file_name ='pmag_q'//trim(intc(iq))
     magm_file_name ='mmag_q'//trim(intc(iq))
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
       pl_file_name   =  trim(pl_file_name)//'_'//solver
       eps_file_name  =  trim(eps_file_name)//'_'//solver
       refl_file_name =  trim(refl_file_name)//'_'//solver
       eel_file_name  =  trim(eel_file_name)//'_'//solver
       alpha_file_name=  trim(alpha_file_name)//'_'//solver
       off_file_name  =  trim(off_file_name)//'_'//solver
       beta_file_name =  trim(beta_file_name)//'_'//solver
       moke_file_name =  trim(moke_file_name)//'_'//solver
       jdos_file_name =  trim(jdos_file_name)//'_'//solver
       magp_file_name =  trim(magp_file_name)//'_'//solver
       magm_file_name =  trim(magm_file_name)//'_'//solver
     endif
     !
     if (len_trim(approx)>0) then
       pl_file_name   =  trim(pl_file_name)//'_'//approx
       eps_file_name  =  trim(eps_file_name)//'_'//approx
       refl_file_name =  trim(refl_file_name)//'_'//approx
       eel_file_name  =  trim(eel_file_name)//'_'//approx
       alpha_file_name=  trim(alpha_file_name)//'_'//approx
       off_file_name  =  trim(off_file_name)//'_'//approx
       beta_file_name =  trim(beta_file_name)//'_'//approx
       jdos_file_name =  trim(jdos_file_name)//'_'//approx
       magp_file_name =  trim(magp_file_name)//'_'//approx
       magm_file_name =  trim(magm_file_name)//'_'//approx
     endif
     !
     if (len_trim(equation)>0) then
       pl_file_name   =  trim(pl_file_name)//'_'//equation
       eps_file_name  =  trim(eps_file_name)//'_'//equation
       refl_file_name =  trim(refl_file_name)//'_'//equation
       eel_file_name  =  trim(eel_file_name)//'_'//equation
       alpha_file_name=  trim(alpha_file_name)//'_'//equation
       off_file_name  =  trim(off_file_name)//'_'//equation
       beta_file_name =  trim(beta_file_name)//'_'//equation
       jdos_file_name =  trim(jdos_file_name)//'_'//equation
       magp_file_name =  trim(magp_file_name)//'_'//equation
       magm_file_name =  trim(magm_file_name)//'_'//equation
     endif
     !
     if (.not.l_kerr) then
       off_file_name=' '
       beta_file_name=' '
       moke_file_name=' '
     endif
     !
     if (.not.l_magnons) then
       magp_file_name=' '
       magm_file_name=' '
     endif
     !
   end subroutine
   !
   subroutine X_setup_headers(desc)
     !
     character(*),  intent(in) :: desc
     ! 
     ! Work space
     ! 
     integer :: ii,n_s
     character(10) :: suffix(6)
     !
     if (desc=="PL") then
       !
       suffix(1) = "[2]"
       suffix(2) = "o[3]"
       !
       n_s = 2
       !
     else
       !
       suffix(1:2) = (/'-Im[2]',  '-Re[3]'/)
       suffix(3:4) = (/'o-Im[4]','o-Re[5]'/)
       suffix(5:6) = (/'`-Im[6]','`-Re[7]'/)
       !
       n_s = 6
       !
     endif
     !
     headers(1) = 'E/ev[1]'
     !
     do ii = 1,n_s
       headers(ii+1)=trim(desc)//trim(suffix(ii))
     enddo
     !
   end subroutine
   !
end module
