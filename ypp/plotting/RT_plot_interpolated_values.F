!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS AM
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
subroutine RT_plot_interpolated_values(CIRCUIT_E,RT_E,CIRCUIT_dE,i_T,ID_circuit,ID_spin,Max_occ,WARN_elec,WARN_hole)
 !
 ! Output: bands on circuit
 !==========================
 !
 use pars,           ONLY:SP,schlen,zero_dfl
 use units,          ONLY:HA2EV,AUT2FS
 use R_lattice,      ONLY:FineGd_desc
 use RT_output_m,    ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use electrons,      ONLY:levels,n_spinor
 use com,            ONLY:msg,of_open_close
 use stderr,         ONLY:real2ch,intc,STRING_remove
 use interpolate,    ONLY:INTERP_obj
 use vec_operate,    ONLY:iku_v_norm
 use interfaces,     ONLY:DESC_write
 use YPPm,           ONLY:Nel_fac,CIRCUIT_k,CIRCUIT_k_label,CIRCUIT_E_db,BANDS_bands
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time,integrated_Nel,integrated_Nhole
 !
 implicit none
 !
 type(levels), intent(in) :: CIRCUIT_E,RT_E,CIRCUIT_dE
 integer, intent(in)      :: i_T,ID_circuit,ID_spin
 real(SP),intent(in)      :: Max_occ
 logical, intent(inout)   :: WARN_elec,WARN_hole
 !
 ! Workspace
 !
 integer               :: n_values,ib,ik
 real(SP)              :: values(3+2*(n_spinor-1))
 character(schlen)     :: headings(3+2*(n_spinor-1)),file_name_rt,file_name_rt_save,extension,k_label
 !
 headings(1)="|k|"
 if (l_RT_lifetimes  ) headings(3)='Widths [meV]'
 if (l_RT_occupations) headings(3)='Occ'
 n_values=3
 !
 if (l_RT_occupations .and. n_spinor==2) then
   headings(4)='occ(up)'
   headings(5)='occ(dw)'
   n_values=5
 endif
 !
 extension=STRING_remove(real2ch( RT_time(i_T)*AUT2FS )," ","")
 if (l_RT_lifetimes  ) file_name_rt="YPP-RT_NEQ_LIFE_bands_T_"//trim(extension)
 if (l_RT_occupations) file_name_rt="YPP-RT_occ_bands_T_"//trim(extension)
 !
 file_name_rt_save=file_name_rt
 !
 call of_open_close(file_name_rt,'ot')
 !
 call DESC_write('o '//trim(file_name_rt_save),'#',RT_desc)
 call DESC_write('o '//trim(file_name_rt_save),'#',Gr_desc)
 call DESC_write('o '//trim(file_name_rt_save),'#',FineGd_desc)
 call DESC_write('o '//trim(file_name_rt_save),'#',TIME_STEP_desc)
 !
 call msg('o bands',"#"," ",INDENT=0)
 call msg('o bands',"#",' TIME= '//trim(real2ch( RT_time(i_T)*AUT2FS ))//' fs',INDENT=0)
 call msg('o bands',"#"," ",INDENT=0)
 if (l_RT_occupations) then
   call msg('o bands',"#",' Integrated E>E_Fermi Carriers (built-in)= '//trim(real2ch(integrated_Nel(i_T,1))),INDENT=0)
   call msg('o bands',"#",'                           (interpolated)= '//trim(real2ch(integrated_Nel(i_T,2))),INDENT=0)
   call msg('o bands',"#",' Integrated E<E_Fermi Carriers (built-in)= '//trim(real2ch(integrated_Nhole(i_T,1))),INDENT=0)
   call msg('o bands',"#",'                           (interpolated)= '//trim(real2ch(integrated_Nhole(i_T,2))),INDENT=0)
 endif
 !
 do ib=BANDS_bands(1),BANDS_bands(2)
   !
   call msg('o bands',"#"," ",INDENT=0)
   !
   headings(2)=' b'//intc(ib) 
   !
   call msg('o bands',"#",headings(:n_values),INDENT=0,USE_TABS=.true.)
   call msg('o bands',"#"," ",INDENT=0)
   !
   do ik=1,CIRCUIT_k%nbz
     !
     ! Transfer of the INTERPOLATED_values...
     !
     values(3)=INTERP_obj(ID_circuit)%INTERP(ib,ik,1)
     if (l_RT_lifetimes   ) values(3)=values(3)*HA2EV*1000._SP
     !
     if (l_RT_occupations) then
       if (Max_occ>0. .and. abs(Nel_fac-1.)>zero_dfl ) values(3)=values(3)/Max_occ*Nel_fac
       !
       if (ib>=RT_E%nbf+1.and.values(3)<0.) then
         WARN_elec=.TRUE.
         values(3)=0.
       endif
       if (ib<=RT_E%nbf  .and.values(3)>0.) then
         WARN_hole=.TRUE.
         values(3)=0.
       endif
       !
       if (n_spinor==2) then
         values(4)=values(3)*INTERP_obj(ID_spin)%INTERP(ib,ik,1)
         values(5)=values(3)*INTERP_obj(ID_spin)%INTERP(ib,ik,2)
       endif
     endif
     !
     if (ik==1) values(1)=0._SP
     if (ik> 1) values(1)=values(1)+iku_v_norm(CIRCUIT_k%pt(ik,:3)-CIRCUIT_k%pt(ik-1,:3))
     !
     values(2)=CIRCUIT_E%E(ib,ik,1)*HA2EV
     if (allocated(CIRCUIT_E_db%E)) values(2)=CIRCUIT_E_db%E(ib,ik,1)*HA2EV
     if (allocated(RT_E%Eo).and.allocated(CIRCUIT_E_db%E)) then
       values(2)=(CIRCUIT_E_db%E(ib,ik,1)+CIRCUIT_dE%E(ib,ik,1))*HA2EV
     endif
     !
     k_label=" "
     if (len_trim(CIRCUIT_k_label(ik))>0) then
       k_label=" ["//trim(CIRCUIT_k_label(ik))//"]"
     endif
     call msg('o bands',' ',values(:n_values),INDENT=0,USE_TABS=.true.,after_msg=k_label)
     !
   enddo
   !
 enddo
 !
 call of_open_close(file_name_rt)
 !
end subroutine

