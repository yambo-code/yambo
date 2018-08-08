!
!        Copyright (C) 2000-2018 the YAMBO team
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
 use pars,           ONLY:SP,schlen,lchlen
 use units,          ONLY:HA2EV,AUT2FS
 use electrons,      ONLY:levels,n_sp_pol,n_spinor,E_reset
 use R_lattice,      ONLY:bz_samp
 use D_lattice,      ONLY:lattice
 use com,            ONLY:msg,of_open_close,error,file_exists,warning,msg_deliver
 use YPP,            ONLY:BANDS_steps,Nel_fac,INTERP_grid,CIRCUIT_k,BANDS_path,CIRCUIT_E_DB_path,&
&                         CIRCUIT_made_of_SPECIAL_k,PtsPath,n_path_pts,BANDS_bands,&
&                         CIRCUIT_E_db,CIRCUIT_k_label
 use YPP_real_time,  ONLY:l_RT_lifetimes,l_RT_occupations,RT_time,RT_carriers_to_RT_k,RT_carriers_to_RT_E
 use stderr,         ONLY:real2ch,intc
 use vec_operate,    ONLY:iku_v_norm
 use parser_m,       ONLY:parser
 use RT_control,     ONLY:RT_apply,RT_carriers_object
 use real_time,      ONLY:RT_carriers,RT_bands
 use interpolate,    ONLY:INTERPOLATION_driver,electrons_bands_interpolate,INTERP_obj,INTERP_mode
 use LIVE_t,         ONLY:live_timing
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
 type(levels)          :: CIRCUIT_E,CIRCUIT_dE,RT_E
 real(SP)              :: Nel,Nhole,Real_Nhole,Real_Nel,Max_occ,values(3+2*(n_spinor-1))
 character(schlen)     :: headings(3+2*(n_spinor-1)),file_name_rt,file_name_rt_save,operation
 character(lchlen)     :: of_string
 logical               :: l_normalize_Nel,l_Nel_fac
 integer               :: ib,ik,i_RT,i_T,n_values,ID_df_or_G,ID_spin,ID_magn
 !
 call section('*','Interpolation tool')
 !=====================================
 !
 ! Nel renomarlization?
 call parser('NelFac',  l_Nel_fac)
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
   call warning("User bands restricted to "//trim(intc(RT_bands(1)))//" - "//trim(intc(RT_bands(2))))
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
 !======================================
 call electrons_bands_interpolate(RT_E,RT_k,RT_bands,CIRCUIT_E,CIRCUIT_dE=CIRCUIT_dE)
 !
 ! Perform interpolation of spinorial factors and magnetization
 !==============================================================
 if(n_spinor>1 .and. BANDS_steps> 0) then
   call electrons_spin_and_magn_interpolate("S",k,CIRCUIT_k,RT_bands,ID_spin,ID_magn)
 endif
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
 ! Perform the occupations interpolation
 !======================================
 !
 call live_timing('Interpolation',n_T_steps)
 !
 do i_T=1,n_T_steps
   !
   call RT_apply(RT_bands,en,k,Time=RT_time(i_T),keep_RT_carriers=.TRUE.)
   !
   ! RT_carriers -> RT_E (E components)
   !
   call RT_carriers_to_RT_E(en,RT_E,"E")
   !
   Real_Nhole=0._SP
   Real_Nel=0._SP
   do i_RT=1,RT_carriers%nstates
     ib      =RT_carriers%table(i_RT,1)
     ik      =RT_carriers%table(i_RT,3)
     if(ib<=RT_E%nbf) Real_Nhole=Real_Nhole-RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
     if(ib> RT_E%nbf) Real_Nel  =Real_Nel  +RT_carriers%delta_f(i_RT)*RT_k%weights(ik)
   enddo
   Max_occ=maxval(abs(RT_carriers%delta_f))
   !
   if (l_RT_occupations) operation="df_on_RT_grid"
   if (l_RT_lifetimes)   operation="G_on_RT_grid"
   !
   call INTERPOLATION_driver(bands=RT_bands,SEED_RT=RT_carriers,SEED_k=RT_k,operation=trim(operation),&
&                            QUIET=.TRUE.,ID_op=ID_df_or_G)
   call INTERPOLATION_driver(OUT_k=CIRCUIT_k,operation=trim(operation),QUIET=i_T>1.and..not.n_T_steps==1)
   !
   ! Normalize the occupations
   !
   if(l_RT_occupations) then
     !
     Nhole=-sum(INTERP_obj(ID_df_or_G)%INTERP(RT_bands(1):RT_E%nbf,:,:))/real(CIRCUIT_k%nbz)
     Nel=   sum(INTERP_obj(ID_df_or_G)%INTERP(RT_E%nbf+1:RT_bands(2),:,:))/real(CIRCUIT_k%nbz)
     !
     if(.not.trim(INTERP_mode)=="NN".and.l_normalize_Nel) then
       if (Nhole>0) then
         INTERP_obj(ID_df_or_G)%INTERP(RT_bands(1):RT_E%nbf,:,:)=&
&          INTERP_obj(ID_df_or_G)%INTERP(RT_bands(1):RT_E%nbf,:,:)/Nhole*Real_Nhole
       endif
       if (Nel>0) then
         INTERP_obj(ID_df_or_G)%INTERP(RT_E%nbf+1:RT_bands(2),:,:)=&
&          INTERP_obj(ID_df_or_G)%INTERP(RT_E%nbf+1:RT_bands(2),:,:)/Nel*Real_Nel
       endif
     endif
     !
   endif
   !
   ! OUTPUT
   !=======
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
       if (allocated(CIRCUIT_E_db%E)) values(2)=CIRCUIT_E_db%E(ib,ik,1)*HA2EV
       if (allocated(RT_E%Eo).and.allocated(CIRCUIT_E_db%E)) then
         values(2)=(CIRCUIT_E_db%E(ib,ik,1)+CIRCUIT_dE%E(ib,ik,1))*HA2EV
       endif
       !
       values(3)=INTERP_obj(ID_df_or_G)%INTERP(ib,ik,1)
       if (l_RT_lifetimes   ) values(3)=values(3)*HA2EV*1000._SP
       if (l_RT_occupations .and. Max_occ>0. .and. l_Nel_fac ) values(3)=values(3)/Max_occ*Nel_fac
       if (l_RT_occupations .and. n_spinor==2) then
         values(4)=values(3)*INTERP_obj(ID_spin)%INTERP(ib,ik,1)
         values(5)=values(3)*INTERP_obj(ID_spin)%INTERP(ib,ik,2)
       endif
       !
       call msg('o bands',' ',values(:n_values),INDENT=0,USE_TABS=.true.,formatted_msg=of_string)
       if (len_trim(CIRCUIT_k_label(ik))>0) then
         of_string=trim(of_string)//"  ["//trim(CIRCUIT_k_label(ik))//"]"
       endif
       call msg_deliver('bands',of_string)  
       !
     enddo
     !
   enddo
   !
   call INTERPOLATION_driver(operation="END",ID_op=ID_df_or_G)
   !
   call of_open_close(file_name_rt)
   !
   call live_timing(steps=1)
   !
 enddo
 !
 call live_timing( )
 !
 ! Clean
 !
 call INTERPOLATION_driver(operation="END")
 call RT_carriers_object(RT_carriers,WHAT='free')
 call E_reset(CIRCUIT_E)
 call E_reset(RT_E)
 if(n_spinor>1 .and. BANDS_steps> 0) call INTERPOLATION_driver(operation="END",ID_op=ID_spin)
 !
end subroutine RT_occ_bands_interpolation
