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
subroutine RT_occ_bands_interpolation(en,k,n_T_steps)
 !
 use pars,           ONLY:SP,schlen,zero_dfl,rZERO
 use units,          ONLY:HA2EV,AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset,n_spin
 use R_lattice,      ONLY:bz_samp,FineGd_desc
 use D_lattice,      ONLY:lattice
 use com,            ONLY:msg,of_open_close
 use YPPm,           ONLY:BANDS_steps,Nel_fac,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_bands,&
&                         CIRCUIT_E_db,CIRCUIT_k_label,l_PROJECT_line,l_PROJECT_plane,PROJECT_depth
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time,integrated_Nel,integrated_Nhole
 use YPP_interfaces, ONLY:PROJECTION_setup,PROJECTION_k_on_the_path_or_plane,PROJECTION_plot,k_special
 use stderr,         ONLY:real2ch,intc,STRING_remove
 use vec_operate,    ONLY:iku_v_norm
 use RT_control,     ONLY:RT_apply,RT_carriers_object,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use RT_output_m,    ONLY:RT_desc,Gr_desc,TIME_STEP_desc
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:INTERPOLATION_driver_seed,INTERPOLATION_driver_do,electrons_bands_interpolate,&
&                         INTERP_obj,INTERP_mode,GRID_k
 use LIVE_t,         ONLY:live_timing
 use interfaces,     ONLY:DESC_write
 !
#include<memory.h>
 !
 integer,       intent(in)    :: n_T_steps
 type(bz_samp), intent(in)    :: k
 type(levels),  intent(inout) :: en
 !
 ! Work space
 !
 type(bz_samp)         :: RT_k
 type(levels)          :: CIRCUIT_E,CIRCUIT_dE,RT_E
 real(SP)              :: Max_occ,values(3+2*(n_spinor-1))
 real(SP), allocatable :: integrated_dF_elecs(:,:,:),integrated_dF_holes(:,:,:)
 character(schlen)     :: headings(3+2*(n_spinor-1)),file_name_rt,file_name_rt_save,operation,extension,k_label
 logical               :: WARN_negative_electrons_df,WARN_positive_holes_df,is_PROJECTED
 integer               :: ib,ik,i_RT,i_T,n_values,ID_circuit,ID_spin,ID_magn,ID_bands(3),ID_grid,ik_path
 logical, external     :: file_exists
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 if(all(INTERP_grid>0).or.BANDS_steps<=0) then
   call error(" Interpolation of occupations/lifetimes only on a band circuit")
 endif
 !
 if(n_sp_pol>1) then
   call error('Occupations for spin polarized systems not implemented')
 endif
 !
 if (l_RT_lifetimes .and. .not.allocated(RT_carriers%NEQ_Lifetime)  ) then
   call error('No scattering => no Lifetimes')
 endif
 !
 if (BANDS_bands(1)<RT_bands(1).or.BANDS_bands(2)>RT_bands(2)) then
   BANDS_bands(1)=max(BANDS_bands(1),RT_bands(1))
   BANDS_bands(2)=min(BANDS_bands(2),RT_bands(2))
   call warning("User bands restricted to "//trim(intc(BANDS_bands(1)))//" - "//trim(intc(BANDS_bands(2))))
 endif
 !
 if (file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) BANDS_path= " "
 !
 ! Define the circuit
 !====================
 call k_special( )
 call k_circuit_driver( BANDS_path, BANDS_steps )
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 !
 ! Projection
 !============
 call PROJECTION_setup("setup")
 call PROJECTION_setup( "BANDS", BANDS_bands )
 !
 is_PROJECTED=l_PROJECT_line.or.l_PROJECT_plane
 if (is_PROJECTED) then
   YAMBO_ALLOC(integrated_dF_elecs,(GRID_k%nbz/PROJECT_depth,n_T_steps,n_spin))
   YAMBO_ALLOC(integrated_dF_holes,(GRID_k%nbz/PROJECT_depth,n_T_steps,n_spin))
   integrated_dF_elecs=rZERO
   integrated_dF_holes=rZERO
 endif
 !
 call section('=',"Interpolation")
 !================================
 !
 ! Map the RT k in RT_k...
 call RT_carriers_to_RT_k(RT_k)
 call k_ibz2bz(RT_k,'iku',.TRUE.)
 !
 ! ... and Eo in RT_E...
 call RT_carriers_to_RT_E(en,RT_E,"Eo")
 !
 ! Perform the energies interpolation
 !====================================
 call electrons_bands_interpolate(RT_E,RT_k,RT_bands,ID_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
 !
 ! Perform interpolation of spinorial factors and magnetization
 !==============================================================
 if(n_spinor>1) call electrons_spin_and_magn_interpolate("S",k,CIRCUIT_k,RT_bands,ID_spin,ID_magn)
 !
 ! Output: bands on circuit
 !==========================
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
 ! Perform the occupations interpolation
 !======================================
 !
 YAMBO_ALLOC(integrated_Nel,(n_T_steps,2))
 YAMBO_ALLOC(integrated_Nhole,(n_T_steps,2))
 integrated_Nel  =rZERO
 integrated_Nhole=rZERO
 !
 WARN_negative_electrons_df=.FALSE.
 WARN_positive_holes_df=.FALSE.
 !
 if (trim(INTERP_mode)=="NN") call live_timing('Time Interpolation(s)',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,en,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   ! RT_carriers -> RT_E (E components)
   !
   call RT_carriers_to_RT_E(en,RT_E,"E")
   !
   do i_RT=1,RT_carriers%nstates
     ib      =RT_carriers%table(i_RT,1)
     ik      =RT_carriers%table(i_RT,3)
     if (ib<=RT_E%nbf) integrated_Nhole(i_T,1)=integrated_Nhole(i_T,1)-RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
     if (ib> RT_E%nbf) integrated_Nel(i_T,1)  =integrated_Nel(i_T,1)+RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
   enddo
   Max_occ=maxval(abs(RT_carriers%delta_f))
   !
   if (l_RT_occupations) operation="df_on_circuit"
   if (l_RT_lifetimes)   operation="W_on_circuit"
     operation=trim(operation)//" Time "//trim(intc(i_T))//"/"//trim(intc(n_T_steps))
   !
   if (is_PROJECTED) then
     if (l_RT_occupations) operation="df_on_grid"
     if (l_RT_lifetimes)   operation="W_on_grid"
     operation=trim(operation)//" Time "//trim(intc(i_T))//"/"//trim(intc(n_T_steps))
     call INTERPOLATION_driver_seed(trim(operation),ID_grid,RT_bands,RT_k,SEED_RT=RT_carriers,&
&                                   QUIET=trim(INTERP_mode)=="NN")
     call INTERPOLATION_driver_do(trim(operation),ID_grid,GRID_k)
     do ik=1,GRID_k%nbz
       ik_path=PROJECTION_k_on_the_path_or_plane(ik)
       if (ik_path==0) cycle
       do ib=BANDS_bands(1),BANDS_bands(2)
         if (ib<=RT_E%nbf) then
           integrated_dF_holes(ik_path,i_T,:)=integrated_dF_holes(ik_path,i_T,:)-&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         else
           integrated_dF_elecs(ik_path,i_T,:)=integrated_dF_elecs(ik_path,i_T,:)+&
&                                             GRID_k%weights(ik)*INTERP_obj(ID_grid)%INTERP(ib,ik,1)
         endif
       enddo
     enddo
     if (l_RT_occupations) call EVAL_integrated_occupations(ID_grid)
   else
     call INTERPOLATION_driver_seed(trim(operation),ID_circuit,RT_bands,RT_k,SEED_RT=RT_carriers,&
&                                   QUIET=trim(INTERP_mode)=="NN")
     call INTERPOLATION_driver_do(trim(operation),ID_circuit,CIRCUIT_k)
     if (l_RT_occupations) call EVAL_integrated_occupations(ID_circuit)
   endif
   !
   if (is_PROJECTED) then
     !
     call INTERPOLATION_driver_end(ID_grid)
     !
     if (trim(INTERP_mode)=="NN") call live_timing(steps=1)
     cycle
   endif
   !
   ! OUTPUT
   !=======
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
           WARN_negative_electrons_df=.TRUE.
           values(3)=0.
         endif
         if (ib<=RT_E%nbf  .and.values(3)>0.) then
           WARN_positive_holes_df=.TRUE.
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
   call INTERPOLATION_driver_end(ID_circuit)
   !
   if (trim(INTERP_mode)=="NN") call live_timing(steps=1)
   !
 enddo
 !
 if (trim(INTERP_mode)=="NN") call live_timing()
 !
 if (WARN_negative_electrons_df) call warning("[INTERPOLATION] Found negative electron occupations")
 if (WARN_positive_holes_df)     call warning("[INTERPOLATION] Found positive hole occupations")
 !
 ! Projection Output
 !===================
 !
 if (l_PROJECT_line.or.l_PROJECT_plane) then 
   !
   if (l_RT_lifetimes  ) operation="lifetimes"
   if (l_RT_occupations) operation="occupations"
   ! 
   call PROJECTION_plot(trim(operation)//"_electrons", n_T_steps, (/1,1/), OBS_K=integrated_dF_elecs, TIME=RT_time ) 
   call PROJECTION_plot(trim(operation)//"_holes", n_T_steps, (/1,1/), OBS_K=integrated_dF_holes, TIME=RT_time ) 
   !
 endif
 !
 ! Clean
 !
 if(n_spinor>1) call INTERPOLATION_driver_end(ID_spin)
 call INTERPOLATION_driver_end(0)
 call RT_carriers_object(RT_carriers,WHAT='free')
 call E_reset(CIRCUIT_E)
 call E_reset(RT_E)
 !
 contains
   !
   subroutine EVAL_integrated_occupations(ID)
     integer  :: ID
     real(SP) :: NK
     !
     if (ID==ID_circuit) NK=real(CIRCUIT_k%nbz)
     if (ID==ID_grid)    NK=real(GRID_k%nbz)
     !
     integrated_Nhole(i_T,2)=-sum(INTERP_obj(ID)%INTERP(RT_bands(1):RT_E%nbf,:,:))/NK
     integrated_Nel(i_T,2)  = sum(INTERP_obj(ID)%INTERP(RT_E%nbf+1:RT_bands(2),:,:))/NK
     !
   end subroutine
   !
end subroutine RT_occ_bands_interpolation
