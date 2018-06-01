!
!        Copyright (C) 2000-2018 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS
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
 use pars,           ONLY:SP,schlen
 use units,          ONLY:HA2EV,AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:lattice
 use com,            ONLY:msg,of_open_close,error,file_exists
 use YPP,            ONLY:BANDS_steps,Nel_fac,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,PtsPath,n_path_pts,BANDS_bands
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time
 use stderr,         ONLY:real2ch,intc
 use vec_operate,    ONLY:iku_v_norm
 use parser_m,       ONLY:parser
 use RT_control,     ONLY:RT_apply,RT_carriers_object
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:INTERPOLATION_coefficients,INTERPOLATION_BZ,INTERPOLATION_reset, &
&                         electrons_bands_interpolate,INTERP_RT_id
 !
#include<memory.h>
 !
 integer,       intent(in) :: n_T_steps
 type(bz_samp), intent(in) :: k
 type(levels),  intent(in) :: en
 !
 ! Work space
 !
 type(bz_samp)         :: RT_k
 type(levels)          :: RT_E,CIRCUIT_E
 real(SP), allocatable :: fit_on_circuit(:,:,:,:),variable_to_fit(:,:,:),&
&                         spin_interpolated(:,:,:),magn_interpolated(:,:,:)
 real(SP)              :: Nel,Nhole,Real_Nhole,Real_Nel,Max_occ,values(3+2*(n_spinor-1))
 character(3)          :: message
 character(schlen)     :: headings(3+2*(n_spinor-1)),file_name_rt,file_name_rt_save
 logical               :: l_normalize_Nel,l_nearest_neig_interp
 integer               :: ib,ik,i_sp_pol,i_RT,i_T,n_values
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
 call section('+',"Loading special Points for the "//trim(lattice)//" lattice")
 !============================================================================= 
 if (file_exists(trim(CIRCUIT_E_DB_path)//"/SAVE/ns.db1")) BANDS_path= " "
 !
 PtsPath=BANDS_path
 n_path_pts=BANDS_steps
 !
 call k_special(.FALSE.)
 !
 CIRCUIT_made_of_SPECIAL_k=CIRCUIT_k%nbz>0
 CIRCUIT_k%nibz=CIRCUIT_k%nbz
 !
 ! Map RT_carriers%kpt ==> RT_k%kpt
 !     RT_carriers%E   ==> RT_E%E 
 !======================================
 call RT_occ_bands_kpts_and_En_map(en,k,RT_E,RT_k)
 !
 ! Perform the energies interpolation
 !======================================
 call electrons_bands_interpolate(RT_E,RT_k,RT_bands,CIRCUIT_E)
 !
 ! Perform interpolation of spinorial factors and magnetization
 !==============================================================
 if(n_spinor>1 .and. BANDS_steps> 0) then
   YAMBO_ALLOC(spin_interpolated,(RT_bands(1):RT_bands(2),CIRCUIT_k%nbz,n_spinor))
   YAMBO_ALLOC(magn_interpolated,(RT_bands(1):RT_bands(2),CIRCUIT_k%nbz,3))
   call electrons_spin_and_magn_interpolate(k,CIRCUIT_k,RT_bands,spin_interpolated,magn_interpolated)
 endif
 !
 ! Perform the occupations interpolation
 !======================================
 Max_occ=0._SP
 !
 ! Define the kind of interpolation
 call parser('NNInterp',l_nearest_neig_interp)
 !
 ! Check if Nel has to be renormalized
 call parser('NormN',l_normalize_Nel)
 !
 YAMBO_ALLOC(variable_to_fit,(RT_bands(1):RT_bands(2),RT_carriers%nk,n_sp_pol))
 !
 YAMBO_ALLOC(fit_on_circuit,(RT_bands(1):RT_bands(2),CIRCUIT_k%nbz,n_sp_pol,n_T_steps))
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,en,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   Real_Nhole=0._SP
   Real_Nel=0._SP
   do i_RT=1,RT_carriers%nstates
     ib      =RT_carriers%table(i_RT,1)
     !ik_small_grid=RT%table(i_RT,2)
     ik      =RT_carriers%table(i_RT,3)
     i_sp_pol=RT_carriers%table(i_RT,4)
     !
     if(ib<=RT_E%nbf) Real_Nhole=Real_Nhole-RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
     if(ib> RT_E%nbf) Real_Nel  =Real_Nel  +RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
     !
     if (.not.l_nearest_neig_interp) then
       if (l_RT_occupations) variable_to_fit(ib,ik,i_sp_pol)= RT_carriers%delta_f(i_RT)
       if (l_RT_lifetimes  ) variable_to_fit(ib,ik,i_sp_pol)= RT_carriers%NEQ_Lifetime(i_RT) 
     endif
     !
   enddo
   Max_occ=maxval((/Max_occ,RT_carriers%delta_f/))
   !
   if (.not.l_nearest_neig_interp) then
     !
     ! STEP 1: define coefficients for occupations/lifetimes
     !
     call INTERPOLATION_coefficients(R2D=variable_to_fit,k=RT_k,Nk=RT_k%nibz,ID=INTERP_RT_id)
     !
     ! STEP 2
     ! 1: interpolate on the circuit      if (BANDS_steps>0)
     !
     if (BANDS_steps> 0) call INTERPOLATION_BZ(K=CIRCUIT_k,R2D=fit_on_circuit(:,:,:,i_T),ID=INTERP_RT_id)
     !
     call INTERPOLATION_reset(INTERP_RT_id)
     !
   elseif(l_nearest_neig_interp) then
     !
     message='RT '
     if(i_T==1) message='RT0'
     !
     if(l_RT_occupations)  then
       call Nearest_kpt_interpolation(RT_carriers%nk,CIRCUIT_k%nbz,RT_carriers%nb,&
&                                     RT_carriers%nstates,1,1,RT_carriers%table,RT_carriers%kpt, &
&                                     CIRCUIT_k%pt,RT_carriers%k_weight,CIRCUIT_k%weights,&
&                                     RT_carriers%delta_f,fit_on_circuit(:,:,:,i_T),message,l_normalize_Nel)
     endif
     if(l_RT_lifetimes) then
       call Nearest_kpt_interpolation(RT_carriers%nk,CIRCUIT_k%nbz,&
&                                     RT_carriers%nb,RT_carriers%nstates,1,1,RT_carriers%table,RT_carriers%kpt, &
&                                     CIRCUIT_k%pt,RT_carriers%k_weight,CIRCUIT_k%weights,&
&                                     RT_carriers%NEQ_Lifetime,fit_on_circuit(:,:,:,i_T),message,.false.)
     endif
     !
   endif
   !
   ! Normilize the occupations
   !
   if(l_RT_occupations) then
     !
     Nhole=-sum(fit_on_circuit(RT_bands(1):RT_E%nbf,:,:,i_T))/real(CIRCUIT_k%nbz)
     Nel=   sum(fit_on_circuit(RT_E%nbf+1:RT_bands(2),:,:,i_T))/real(CIRCUIT_k%nbz)
     !
     if(.not.l_nearest_neig_interp.and.l_normalize_Nel) then
       if (Nhole>0) then
         fit_on_circuit(RT_bands(1):RT_E%nbf,:,:,i_T)=fit_on_circuit(RT_bands(1):RT_E%nbf,:,:,i_T)/Nhole*Real_Nhole
       endif
       if (Nel>0) then
         fit_on_circuit(RT_E%nbf+1:RT_bands(2),:,:,i_T)=fit_on_circuit(RT_E%nbf+1:RT_bands(2),:,:,i_T)/Nel*Real_Nel
       endif
     endif
     !
   endif
   !
 enddo 
 !
 if (.not.l_nearest_neig_interp) call INTERPOLATION_reset( )
 !
 call RT_carriers_object(RT_carriers,WHAT='free')
 !
 YAMBO_FREE(variable_to_fit)
 !
 ! Output: bands on circuit
 !==========================
 !
 headings(1)="|k|"
 if (l_RT_lifetimes  ) headings(3)='LIFE'
 if (l_RT_occupations) headings(3)=' occ'
 n_values=3
 !
 if (l_RT_occupations .and. n_spinor==2) then
   headings(4)='occ(up)'
   headings(5)='occ(dw)'
   n_values=5
 endif
 !
 do i_T=1,n_T_steps
   !
   if (l_RT_lifetimes  ) file_name_rt="YPP-RT_NEQ_LIFE_bands_iT"//intc(i_T)
   if (l_RT_occupations) file_name_rt="YPP-RT_occ_bands_iT"//intc(i_T)
   !
   file_name_rt_save=file_name_rt
   !
   call of_open_close(file_name_rt,'ot')
   !
   call RT_write_descriptions(trim(file_name_rt_save))
   !
   call msg('o bands',"#"," ",INDENT=0)
   call msg('o bands',"#",' TIME= '//trim(real2ch( RT_time(i_T)*AUT2FS ))//' fs',INDENT=0)
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
       if (ik==1) values(1)=0._SP
       if (ik> 1) values(1)=values(1)+iku_v_norm(CIRCUIT_k%pt(ik,:)-CIRCUIT_k%pt(ik-1,:))
       !
       values(2)=CIRCUIT_E%E(ib,ik,1)*HA2EV
       if (l_RT_lifetimes  ) values(3)=fit_on_circuit(ib,ik,1,i_T)*HA2EV*1000._SP
       if (l_RT_occupations) values(3)=fit_on_circuit(ib,ik,1,i_T)/Max_occ*Nel_fac
       if (l_RT_occupations .and. n_spinor==2) then
         values(4)=values(3)*spin_interpolated(ib,ik,1)
         values(5)=values(3)*spin_interpolated(ib,ik,2)
       endif
       call msg('o bands',' ',values(:n_values),INDENT=0,USE_TABS=.true.)
       !
     enddo
     !
   enddo
   !
   call of_open_close(file_name_rt)
   !
 enddo
 !
 YAMBO_FREE(fit_on_circuit)
 !
 call E_reset(CIRCUIT_E)
 if(n_spinor==2) then
   YAMBO_FREE(spin_interpolated)
   YAMBO_FREE(magn_interpolated)
 endif
 !
end subroutine RT_occ_bands_interpolation
