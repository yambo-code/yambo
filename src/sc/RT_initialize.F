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
subroutine RT_initialize(file_name,Xen)
 ! 
 use pars,           ONLY:SP,lchlen,schlen
 use drivers,        ONLY:l_sc_hf
 use SC,             ONLY:SC_bands,rho_n
 use QP_m,           ONLY:QP_Sx_low_band
 use com,            ONLY:msg,of_open_close,warning
 use real_time,      ONLY:l_RPA,RT_resonant,RT_no_intraband,l_eps_analyze,&
&                         Thermal_steps,Thermal_time,RT_step,l_rt_efield,&
&                         Ext_Field_Name,G_lesser,G_lesser_previous,&
&                         l_RT_no_updates,Integrator_Name,EF_q0,l_qp_analyze, &
&                         l_no_diamagnetic
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 implicit none
 !
 character(lchlen)        :: file_name(3)
 type(levels)             :: Xen
 !
 ! Work space
 !
 character(schlen)        :: headings(10)
 integer                  :: i1,ik
 !
 ! Logicals
 !----------
 !
 call parser('RTResonant',RT_resonant)
 call parser('RTNoIntraband',RT_no_intraband)
 call parser('RTEpsAnalyze',l_eps_analyze)
 call parser('RTQpAnalyze',l_qp_analyze)
 call parser('KeepVxc',l_RPA)
 call parser('NoDiamagnetic',l_no_diamagnetic)
 !
 if(l_sc_hf.and.QP_Sx_low_band/=1) then
   QP_Sx_low_band=1
   call warning('  Exchange summation low band set to 1 ')
 endif
 !
 if(Thermal_steps/=0) then
   !
   Thermal_time=real(Thermal_steps)*RT_step
   !
 else
   !      
   Thermal_time=0._SP
   !
 endif
 !
 l_rt_efield=.false.
 if(Ext_Field_Name/='NONE'.and.Ext_Field_Name/='none') then
   !      
   l_rt_efield=.true.
   call check_symm(EF_q0)
   !
 else
   !      
   if( l_eps_analyze) then
     call warning(' No External Field  l_eps_analyze set to .false.')      
     l_eps_analyze=.false.
   endif
   !
 endif
 !
 if(l_rt_efield.and.RT_resonant.and..not.RT_no_intraband) then
   RT_no_intraband=.true.
   call warning('[RT] Intraband transitions excluded because Resonant Efield !')
 endif
 !
 ! OUTPUT Files
 !--------------
 !
 file_name(1)='dynamics'
 call of_open_close(file_name(1),'ot') 
 headings(1)="Time"
 headings(2)="E_kin"
 headings(3)="E_ion"
 if(.not.l_RT_no_updates) then
   !      
   if(l_sc_hf) then
     headings(4)="E_Hartree"
     headings(5)="E_x"
   else
     headings(4)="E_Hartree"
     headings(5)="E_xc"
   endif
   !
 else
   headings(4)="Eo_pot"
   headings(5)="none"
 endif
 headings(6)="N" 
 !
 call msg('o '//file_name(1),'#',headings(1:6),INDENT=-2,USE_TABS=.TRUE.)
 !
 if(l_rt_efield) then
   !      
   file_name(2)='response'
   call of_open_close(file_name(2),'ot') 
   headings(1)='Time'
   headings(2)='Re(Ext_Field)'
   headings(3)='Img(Ext_Field)'
   headings(4)='j_x'
   headings(5)='j_y'
   headings(6)='j_x'
   !
   call msg('o '//file_name(2),'#',headings(1:6),INDENT=-2,USE_TABS=.TRUE.)
   !
 endif
 !
 call msg('nr','[RT] Integrator          :'//trim(Integrator_Name))
 call msg( 'r','[RT] External Field      :'//trim(Ext_Field_Name))
 call msg(' r','[RT] Resonant Field      :',RT_resonant)
 call msg('nr','[RT] Intraband transitions :',.not.RT_no_intraband)
 call msg(' r','[RT] Dielectric function   :',l_eps_analyze)
 call msg(' r','[RT] RPA                   :',l_RPA)
 call msg(' r','[RT] Diamagnetic term      :',.not.l_no_diamagnetic)
 call msg(' r','[RT] Analyze QP-dynamics   :',l_qp_analyze)
 !
 file_name(3)="qp_rt"
 if(l_qp_analyze) call of_open_close(file_name(3),'ot')
 !
end subroutine RT_initialize
