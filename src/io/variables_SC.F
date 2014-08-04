!
!        Copyright (C) 2000-2014 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, DS
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
integer function variables_SC(ID,local_description,what)
 !
 use pars,           ONLY:lchlen
 use units,          ONLY:AU2Tesla,DEG2RAD,FS2AUT,AU2KWCMm2,HA2EV
 use D_lattice,      ONLY:atoms_string
 use R_lattice,      ONLY:nkibz
 use QP_m,           ONLY:SC_band_mixing,COHSEX_use_empties
 use SC,             ONLY:SC_bands,it_now,SC_potential,SC_kind,SC_perturbation,l_sc_diagonal,&
&                         SC_xc_functional,l_NSC_shot,SC_fft_size,SC_up_W_iters,&
&                         SC_ng_Sx,SC_X_bands,SC_X_size,SC_description,n_SC_descriptions,l_sc_pot_is_local
 use IO_m,           ONLY:io_elemental,io_status,ver_is_gt_or_eq,IO_INCOMPATIBLE_VAR,&
&                         variable_is_found
#if defined _MAGNETIC
 use magnetic,       ONLY:MAG_hamiltonian_type,MAG_B,MAG_radius,MAG_gauge,MAG_theta,MAG_psi
 use global_XC,      ONLY:EXT_B_LANDAU,EXT_B_PAULI,EXT_B_MAGNETISM
#endif
 use global_XC,      ONLY:SC_EXX,SC_HF,SC_COHSEX,SC_EXXC
 !
 implicit none
 !
 integer            ::ID
 character(lchlen)  ::local_description(100)
 character(*)       ::what
 !
 ! Work Space
 !
 integer               :: SC_kind_disk,SC_perturbation_disk,&
&                         io_status_save,i_desc
 logical               :: CHECK_,l_MP
 !
 l_MP =what=="MP"
 !
 i_desc=0
 local_description=' '
 !
 CHECK_=.TRUE.
 if (l_NSC_shot) CHECK_=.FALSE.
 !
 call io_elemental(ID,VAR='ATOMS_STRING',CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=atoms_string,&
&     VAR=' Unit cell atoms                 :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR='XC_FUNCTIONAL',CH0="",VAR_SZ=1,MENU=0)
 i_desc=i_desc+1
 call io_elemental(ID,CH0=SC_potential,&
&     VAR=' xc Functional                   :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 call io_elemental(ID,VAR="XC_GLOBALS",VAR_SZ=3,MENU=0)
 call io_elemental(ID,I0=SC_perturbation,DB_I0=SC_perturbation_disk)
 call io_elemental(ID,I0=SC_kind,DB_I0=SC_kind_disk)
 call io_elemental(ID,I0=SC_xc_functional)
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 io_status_save=io_status(ID)
 !
 call io_elemental(ID,VAR="LOCAL_ITER_BANDS_MIX",VAR_SZ=6,MENU=0) 
 !
 i_desc=i_desc+1
 call io_elemental(ID,L0=l_sc_pot_is_local,&
&     VAR=' The potential is local          :',DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I0=it_now,DB_I0=it_now,&
&     VAR=' Iteration                       :',DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I1=SC_bands,&
&     VAR=' Bands                           :',CHECK=CHECK_,OP=(/"==","=="/),&
&     DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,I0=nkibz,&
&     VAR=' Number of K-points              :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 i_desc=i_desc+1
 call io_elemental(ID,R0=SC_band_mixing,&
&     VAR=' Bands mixing                    :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
 call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 !
 if (any((/SC_kind_disk==SC_EXX,SC_kind_disk==SC_EXXC,SC_kind_disk==SC_HF,SC_kind_disk==SC_COHSEX/))) then
   call io_elemental(ID,VAR="SIGMAx_G",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_ng_Sx,&
&       VAR=' Sx RL components                :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 if ( SC_kind_disk==SC_COHSEX   ) then
   call io_elemental(ID,VAR="X_bands_size",VAR_SZ=6,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,I1=SC_X_bands,&
&       VAR=' Response function bands         :',CHECK=CHECK_,OP=(/"==","=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_X_size,&
&       VAR=' Response function size          :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=COHSEX_use_empties,&
&     VAR=" COHSEX use empty bands          :", CHECK=.true.,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,I0=SC_up_W_iters,&
&     VAR=' Updating W every                :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,L0=l_sc_diagonal,&
&     VAR=' Diagonal approximation          :',CHECK=CHECK_,OP=(/"=="/),&
&     DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
 endif
 !
 if (ver_is_gt_or_eq(ID,revision=2877)) then
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_COHSEX/)).or.l_MP) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0,DESCRIPTION=local_description(i_desc))         
     i_desc=i_desc+1
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
 if (io_status_save==0.and.io_status(ID)<0) then
   io_status(ID)=IO_INCOMPATIBLE_VAR
   if (l_NSC_shot) io_status(ID)=0
 endif
 !
 if (.not.ver_is_gt_or_eq(ID,revision=2877)) then
   if (all((/SC_kind_disk/=SC_HF,SC_kind_disk/=SC_COHSEX/)).or.l_MP) then
     call io_elemental(ID,VAR="FFT_SIZE",VAR_SZ=1,MENU=0,DESCRIPTION=local_description(i_desc))         
     i_desc=i_desc+1
     call io_elemental(ID,I0=SC_fft_size,&
&         VAR=' FFT mesh points                 :',CHECK=.true.,OP=(/"=="/),&
&         DESCRIPTION=local_description(i_desc))
     call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   endif
 endif
 !
#if defined _MAGNETIC
 if (any((/SC_perturbation_disk==EXT_B_LANDAU,SC_perturbation_disk==EXT_B_PAULI,&
&          SC_perturbation_disk==EXT_B_MAGNETISM/))) then
   !
   call io_elemental(ID,VAR="MAG_hamiltonian",CH0="",VAR_SZ=1,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,CH0=MAG_hamiltonian_type, &
&       VAR=' [MAG] Hamiltonian kind          :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='B_FIELD_parameters',VAR_SZ=4,MENU=0) 
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=AU2Tesla,R0=MAG_B,&
&       VAR=' [MAG] B modulus              [T]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   !
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_theta,& 
&       VAR=' [MAG] B theta angle    [degrees]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   i_desc=i_desc+1
   call io_elemental(ID,UNIT=(1./DEG2RAD),R0=MAG_psi,& 
&       VAR=' [MAG] B psi angle      [degrees]:',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   !
   if (SC_perturbation_disk/=EXT_B_PAULI) then
     i_desc=i_desc+1
     call io_elemental(ID,R0=MAG_radius,&
&       VAR=' [MAG] B flux radius             :',CHECK=.true.,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   endif
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
   call io_elemental(ID,VAR='MAG_GAUGE',CH0="",VAR_SZ=1,MENU=0)
   i_desc=i_desc+1
   call io_elemental(ID,CH0=MAG_gauge,&
&       VAR=' [MAG] Gauge                     :',CHECK=CHECK_,OP=(/"=="/),&
&       DESCRIPTION=local_description(i_desc))
   call io_elemental(ID,VAR="",VAR_SZ=0,MENU=0)
   !
 endif
 !
#endif
 !
 variables_SC=io_status(ID)
 n_SC_descriptions=i_desc
 SC_description(:i_desc)=local_description(:i_desc)
 !
end function
