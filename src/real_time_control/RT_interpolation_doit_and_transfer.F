!
!        Copyright (C) 2000-2022 the YAMBO team
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
subroutine RT_interpolation_doit_and_transfer(OBJ,K,E,nb,l_B,N_carriers,df_Max,dE_Max)
 !
 use pars,           ONLY:SP
 use electrons,      ONLY:levels
 use R_lattice,      ONLY:bz_samp,bz_samp_reset
 use real_time,      ONLY:RT_carriers
 use RT_control,     ONLY:RT_carriers_to_RT_k
 use interpolate,    ONLY:INTERP_obj,INTERPOLATION_driver_seed,INTERPOLATION_driver_do
 !
 implicit none
 !
 character(*)  :: OBJ
 type(levels),  intent(inout) :: E
 type(bz_samp), intent(inout) :: K
 logical,       intent(in)    :: l_B
 integer       :: nb(2)
 real(SP)      :: df_Max,dE_Max,N_carriers(2)
 !
 ! Work Space
 !
 logical       :: HAS_DBGD
 integer       :: ik,nb_max,nb_max_FineGd,ID
 type(bz_samp) :: RT_k
 !
 ID=0
 HAS_DBGD=allocated(K%FGibz%pt)
 !
 nb_max=    nb(2)
 if (HAS_DBGD) nb_max_FineGd=min(nb(2),E%FG%nb)
 !
 call bz_samp_reset(RT_k)
 call RT_carriers_to_RT_k(RT_k)
 !
 call INTERPOLATION_driver_seed(OBJ//"_from_DB",ID,nb,RT_k,SEED_RT=RT_carriers,NEAREST=.not.l_B)
 !
 call INTERPOLATION_driver_do(OBJ//"_from_DB",ID,K)
 !
 if (OBJ=="df") then
   !
   ! Metallic and Insulating bands update
   !
   E%nbc=nb
   E%nbf(1)=nb(1)-1
   E%nbm(1)=nb(2)
   !
   E%df( nb(1):nb(2),:,:)=INTERP_obj(ID)%INTERP(nb(1):nb(2),:,:)
   df_Max=maxval(abs(INTERP_obj(ID)%INTERP(nb(1):nb(2),:,:)))
   !
   do ik=1,K%nibz
     N_carriers(1)=N_carriers(1)+sum(abs(INTERP_obj(ID)%INTERP(nb(1):nb_max,ik,:)))*K%weights(ik)/2._SP
   enddo
   !
   if (HAS_DBGD) then
     E%FG%df( nb(1):nb_max_FineGd,:,:)=INTERP_obj(ID)%INTERP_FineGd(nb(1):nb_max_FineGd,:,:)
     do ik=1,K%FGbare%N
       N_carriers(2)=N_carriers(2)+sum(abs(INTERP_obj(ID)%INTERP_FineGd(nb(1):nb_max_FineGd,ik,:)))*K%FGbare%weights(ik)/2._SP
     enddo
   endif
   !
 else if (OBJ=="dE") then
   !
   E%E( nb(1):nb(2),:,:)=E%E(nb(1):nb(2),:,:)+INTERP_obj(ID)%INTERP(nb(1):nb(2),:,:)
   dE_Max=maxval(abs(INTERP_obj(ID)%INTERP(nb(1):nb(2),:,:)))
   !
   if (HAS_DBGD) then
     E%FG%E(nb(1):nb_max_FineGd,:,:)=E%FG%E(nb(1):nb_max_FineGd,:,:)+&
&                                    INTERP_obj(ID)%INTERP_FineGd(nb(1):nb_max_FineGd,:,:)
   endif
   !
 endif
 !
 call INTERPOLATION_driver_end(ID)
 !
end subroutine 
