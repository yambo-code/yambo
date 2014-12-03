!
!        Copyright (C) 2000-2014 the YAMBO team
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
integer function variables_RT(ID,local_description,what)
 !
 use pars,           ONLY:lchlen,SP
 use units,          ONLY:FS2AUT,AU2KWCMm2,HA2EV
 use drivers,        ONLY:l_elph_scatt,l_elel_scatt,l_elel_corr
 use electrons,      ONLY:BZ_RIM_tot_nkpts
 use stderr,         ONLY:intc
 use rt_ctl,         ONLY:RT_IO_JPO_steps,RT_IO_G_steps,RT_IO_steps_DB, &
&                         RT_description,n_RT_descriptions
 use real_time,      ONLY:RT_nk,RT_nstates,l_NE_with_fields,RT_step,NE_steps,NE_time,              &
&                         Integrator_name, &
&                         RAD_LifeTime,Phase_LifeTime,              &
&                         l_RT_induced_field,G_MEM_steps,&
&                         l_RT_CCA_Kernel,Gr_description,l_RT_impose_N_dN,&
&                         l_RT_uses_E_RIM,N_Gr_descs,RT_life_interp_steps,&
&                         RT_life_interp_KIND,l_NE_simulate_Irredux,RIM_EE_percent,l_RT_impose_E
 use ELPH,           ONLY:ph_modes,elph_branches
 use X_m,            ONLY:global_gauge
 use fields,         ONLY:n_ext_fields,Efield
 use IO_m,           ONLY:io_elemental,io_status,ver_is_gt_or_eq,IO_INCOMPATIBLE_VAR,&
&                         variable_is_found
 !
 implicit none
 !
 integer            ::ID
 character(lchlen)  ::local_description(100)
 character(*)       ::what
 !
 integer               :: i1,i_desc,i_dummy,RT_diagonalization_steps
 logical               :: l_J,l_G,l_occupations,l_NE_with_fields_disk,l_MP,l_RT_diagonalize,l_RT_diamagnetic
 real(SP)              :: RT_life_interp_decay_time
 character(lchlen)     :: ch
 !
 RT_diagonalization_steps=0
 l_RT_diagonalize=.false.
 RT_life_interp_decay_time=0._SP
 !
 l_J           = what=="J_and_P"
 l_G           = what=="G"
 l_occupations = what=="occupations"
 !
 i_desc=0
 local_description=' '
 !
 ! RT_vars
 !
 call io_elemental(ID,VAR="RT_vars",VAR_SZ=2,MENU=0) 
 i_desc=i_desc+1
 call io_elemental(ID,I0=RT_nk,&
&     VAR=' Number of RT-K points           :',CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,I0=RT_nstates,&
&     VAR=' Number of RT states             :',CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 !
 ! ELPH_vars
 !
 call io_elemental(ID,VAR="ELPH_vars",VAR_SZ=5,MENU=0) 
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_elph_scatt,&
&   VAR=" [E-P] Electron-phonon           :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_RT_CCA_Kernel,&
&   VAR=" [E-P] Completed Collision(CCA)  :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID, &
&   VAR=" [E-P] Branches                  :",I1=elph_branches,CHECK=.true.,OP=(/"==","=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID, &
&   VAR=" [E-P] Memory steps              :",I0=G_MEM_steps,CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 !
 ! E-E_vars
 !
 call io_elemental(ID,VAR="E-E_vars",VAR_SZ=2,MENU=0) 
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_elel_scatt,&
&   VAR=" [E-E] Coulomb scattering        :", CHECK=.true.,OP=(/"=="/),&
&   DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_elel_corr,&
&  VAR=" [E-E] Coulomb correlation       :", CHECK=.true.,OP=(/"=="/),&
&  DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if(.not.any((/l_J,l_G,l_occupations/))) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
 !
 if(any((/l_J,l_G,l_occupations/))) then
   !
   !
   ! RT_DYNAMICS
   !
   call io_elemental(ID,VAR="RT_DYNAMICS",VAR_SZ=4,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_NE_simulate_Irredux,&
&    VAR=" [RT] Simulate irredux           :", CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Time step              [fs]:",R0=RT_step,CHECK=.true.,OP=(/"=="/),UNIT=1./FS2AUT,&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Time steps    (planned)    :",I0=NE_steps,WARN=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] NE time       (reached)[fs]:",R0=NE_time,WARN=.true.,OP=(/"=="/),UNIT=1./FS2AUT,&
&    DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   !
   ! RT_life_interp_kind
   !
   call io_elemental(ID,VAR='RT_life_interp_kind',CH0="",VAR_SZ=1,MENU=0)
   i_desc=i_desc+1
   call io_elemental(ID,CH0=RT_life_interp_KIND, &
&        VAR=" [RT] Sampling                   :",WARN=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   !
   ! RT_life_interp_more
   !   
   call io_elemental(ID,VAR="RT_life_interp_more",VAR_SZ=3,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Inter/intra grps       [fs]:",R1=RT_life_interp_steps,WARN=.true.,OP=(/"==","=="/),&
&    DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   i_desc=i_desc+1
   call io_elemental(ID,&
&    VAR=" [RT] Time decay             [fs]:",R0=RT_life_interp_decay_time,WARN=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   !
   ! INTEGRATOR
   !   
   call io_elemental(ID,VAR="INTEGRATOR",CH0="",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,CH0=Integrator_name, &
&    VAR=' [RT] Integrator                 :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! RT_OTHERS
   !
   call io_elemental(ID,VAR="RT_OTHERS",VAR_SZ=8,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_diagonalize,&
&    VAR=' [RT] QP analysis                :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=RT_diagonalization_steps,&
&    VAR=' [RT] Diagonalization steps      :',CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   if (l_J.or.l_occupations) then
     i_desc=i_desc+1
     call io_elemental(ID,DB_I0=RT_IO_steps_DB,I0=RT_IO_JPO_steps,&
&      VAR=' [RT] IO steps                   :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   else if (l_G) then
     i_desc=i_desc+1
     call io_elemental(ID,I0=RT_IO_G_steps,&
&      VAR=' [RT] IO steps                   :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   endif
   i_desc=i_desc+1
   call io_elemental(ID,R0=RAD_LifeTime,&
&    VAR=' [RT] Radiative life time    [fs]:', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   i_desc=i_desc+1
   call io_elemental(ID,R0=Phase_LifeTime,&
&    VAR=' [RT] Dephasing life time    [fs]:', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_NE_with_fields,DB_L0=l_NE_with_fields_disk,&
&    VAR=' [RT] External Field             :', CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_diamagnetic,&
&    VAR=' [RT] Diamagnetic term           :',  CHECK=.false.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_induced_field,&
&    VAR=' [RT] Induced field              :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! G retarded variables
   !======================
   call io_elemental(ID,VAR="G_retarded_BASE",VAR_SZ=1,MENU=0) 
   call io_elemental(ID,I0=N_Gr_descs)
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   do i1=1,N_Gr_descs
     write (ch,'(a,i5.5)') 'G_ret_DESC_strings_',i1
     call io_elemental(ID,VAR=trim(ch),CH0="",VAR_SZ=1,MENU=0)
     i_desc=i_desc+1
     call io_elemental(ID,CH0=Gr_description(i1),VAR='',DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   enddo
   !
   ! RIM
   !======================
   call io_elemental(ID,VAR="RIM_support",VAR_SZ=3,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_uses_E_RIM,&
&    VAR=' [RIM] Random Energies support   :',  CHECK=.true.,OP=(/"=="/),&
&    DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=BZ_RIM_tot_nkpts,&
&    VAR=' [RIM] Random k-point            :',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,R0=RIM_EE_percent,&
&    VAR=' [RIM] Used for e-e         [o/o]:',  CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! CONSTRAINS
   !======================
   call io_elemental(ID,VAR="CONSTRAINS",VAR_SZ=3,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_impose_N_dN,&
&      VAR=' [CONSTRAINS] N (ee+ep) & dN (ep):',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_RT_impose_E,&
&      VAR=' [CONSTRAINS] IP Energy      (ee):',  CHECK=.true.,OP=(/"=="/),&
&      DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   ! FIELDS
   !======================
   if (l_NE_with_fields_disk) then
     !
     call io_elemental(ID,VAR="GAUGE_TYPE",CH0="",VAR_SZ=1,MENU=0)
     i_desc=i_desc+1
     call io_elemental(ID,CH0=global_gauge, &
&       VAR=' [FIELDs] Gauge Type             :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
     !
     do i1=1,n_ext_fields
       call io_elemental(ID,VAR="FIELD_NAME"//trim(intc(i1)),CH0="",VAR_SZ=1,MENU=0)
       i_desc=i_desc+1
       call io_elemental(ID,CH0=Efield(i1)%ef_name, &
&        VAR=' [FIELDs] Type                   :',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
       call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       !
       call io_elemental(ID,VAR="EXTERNAL_FIELD"//trim(intc(i1)),VAR_SZ=10,MENU=0)
       !
       i_desc=i_desc+1
       call io_elemental(ID,R1=Efield(i1)%versor,WARN=.true., &
&        VAR=" [FIELDs] Versor                 :",OP=(/"==","==","=="/),&
&        DESCRIPTION=local_description(i_desc))
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Intensity      [kWCMm2]:",R0=Efield(i1)%intensity,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=AU2KWCMm2)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Damping            [fs]:",R0=Efield(i1)%damping,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency range    [eV]:",R1=Efield(i1)%frequency,CHECK=.true.,OP=(/"==","=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency steps        :",I0=Efield(i1)%n_frequencies,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc))
       i_desc=i_desc+1
       call io_elemental(ID,&
&        VAR=" [FIELDs] Frequency step     [eV]:",R0=Efield(i1)%W_step,CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=HA2EV)
       i_desc=i_desc+1
       call io_elemental(ID,R0=Efield(i1)%t_initial,&
&        VAR=' [FIELDs] Initial time       [fs]:',CHECK=.true.,OP=(/"=="/),&
&        DESCRIPTION=local_description(i_desc),UNIT=1./FS2AUT)
       !                       
       if (i1/=n_ext_fields) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
       if (i1==n_ext_fields) call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
     enddo
     !
   else
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=1)
   endif
 endif
 !
 variables_RT=io_status(ID)
 n_RT_descriptions=i_desc
 RT_description(:i_desc)=local_description(:i_desc)
 !
end function
