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
 use vec_operate,    ONLY:normalize_v
 use drivers,        ONLY:l_sc_hf
 use SC,             ONLY:SC_bands,rho_n
 use QP_m,           ONLY:QP_Sx_low_band
 use com,            ONLY:msg,of_open_close,warning,error
 use real_time,      ONLY:l_RPA,Thermal_steps,Thermal_time,RT_step,l_rt_efield, &
&                         l_fast_exchange,G_lesser,G_lesser_previous,i_time,     &
&                         l_RT_no_updates,Integrator_Name,l_qp_analyze,          &
&                         l_no_diamagnetic,l_always_relax,EField,n_ext_fields
 use electrons,      ONLY:levels
 use parser_m,       ONLY:parser
 use stderr,         ONLY:intc
 implicit none
 !
 character(lchlen)        :: file_name(2)
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
 call parser('EFNoIntraband1',EField(1)%no_intraband)
 call parser('EFNoIntraband2',EField(2)%no_intraband)
 call parser('EFNoDiamagnetic1',EField(1)%no_diamagnetic)
 call parser('EFNoDiamagnetic2',EField(2)%no_diamagnetic)
 !
 call parser('RTQpAnalyze',l_qp_analyze)
 call parser('KeepVxc',l_RPA)
 call parser('NoDiamagnetic',l_no_diamagnetic)
 call parser('AlwaysRelax',l_always_relax)
 call parser('FastExchange',l_fast_exchange)
 !
 if(SC_bands==0) call error("SC_bands = 0 ! ")
 !
 i_time=0
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
 !
 do i1=1,n_ext_fields
   !      
   Efield(i1)%ef_name=trim(Efield(i1)%ef_name)      
   !
   if((Efield(i1)%ef_name/='none'.and.EField(i1)%intensity/=0.)) then
     !      
     l_rt_efield=.true.
     !
     EField(i1)%versor=normalize_v(Efield(i1)%versor)
     !
     call SC_check_symmetries(Efield(i1)%versor)
     !
   endif
   !
 enddo
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
 call msg('nr','[RT] Integrator          :'//trim(Integrator_Name))
 call msg(' r','[RT] RPA                   :',l_RPA)
 call msg(' r','[RT] Diamagnetic term      :',.not.l_no_diamagnetic)
 call msg(' r','[RT] Analyze QP-dynamics   :',l_qp_analyze)
 !
 do i1=1,n_ext_fields
   call msg('nr','[RT] External Field('//trim(intc(i1))//'      :'//trim(EField(i1)%ef_name))
   call msg(' r','[RT] Intensity                  :',EField(i1)%intensity)
   call msg(' r','[RT] Frequancy                  :',EField(i1)%frequency)
   call msg(' r','[RT] Direction                  :',EField(i1)%versor)
   call msg(' r','[RT] Intraband transitions      :',.not.EField(i1)%no_intraband)
 enddo  
 !
 file_name(2)="qp_rt"
 if(l_qp_analyze) call of_open_close(file_name(2),'ot')
 !
end subroutine RT_initialize
