!
!        Copyright (C) 2000-2020 the YAMBO team
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
integer function ELPH_databases_IO_elemental(what,IDB)
 !
 ! s.dbph_ structure
 !
 !   read (99) ph_modes,elph_nk,elph_nb
 !   read (99) alat,Q,K
 !   read (99) ph_freqs_sq
 !   DO ik=1,nksq
 !     read (99) elph_gkkp_disk
 !     read (99) polarization_vec
 !     read (99) grad_at_gamma (only at Gamma)
 !     read (99) Ek
 !     read (99) Ek_plus_q
 !   ENDDO
 !
 use pars,           ONLY:DP,schlen,SP
 use ELPH,           ONLY:PH_W_debye
 use YPP_ELPH,       ONLY:DB_kind,DB_ph_modes,DB_nk,DB_nb,DB_ph_kpt,DB_PH_qpt,&
&                         DB_ph_pol_vec,DB_ph_freq_sq,DB_alat,DB_E_k,DB_E_k_plus_q,&
&                         DB_grad_at_gamma,DB_gkkp,elph_dbs_path
 use IO_m,           ONLY:io_control,OP_RD,RD_CL,io_connect,io_unit,io_disconnect
 use D_lattice,      ONLY:alat
 !
 implicit none
 !
 character(*)      ::what
 integer           ::IDB
 !
 ! Work Space
 !
 character(schlen) :: dbph_name
 real(DP)          :: dVRY2Ha
 integer           :: ID,ik,ib,il
 !
 ELPH_databases_IO_elemental=-1
 !
 call io_control(ACTION=OP_RD,ID=ID)
 !
 if (index(what,"dV_bare")>0) then
   write (dbph_name,'(a,i6.6)') 's.dbph_bare_',iabs(IDB)
 else
   write (dbph_name,'(a,i6.6)') 's.dbph_',iabs(IDB)
 endif
 !
 ELPH_databases_IO_elemental=io_connect(desc=trim(elph_dbs_path)//"/"//trim(dbph_name),type=-2,ID=ID)
 !
 if (ELPH_databases_IO_elemental/=0) then
   call local_return()
   return
 endif
 !
 read (io_unit(ID)) DB_ph_modes,DB_nk,DB_nb
 !
 if (index(what,"EXIST")>0) then
   read (io_unit(ID)) DB_alat
   DB_kind="PWscf"
   if (DB_alat<0._DP) DB_kind="ABINIT"
   call local_return()
   return
 endif
 !
 if (index(what,"KPT")>0) then
   read (io_unit(ID)) DB_alat,DB_PH_qpt(IDB,:),DB_ph_kpt(:,:DB_nk)
   DB_PH_qpt(IDB,:)=DB_PH_qpt(IDB,:)/DB_alat*alat(:)
   do ik=1,DB_nk
     DB_ph_kpt(:,ik)=DB_ph_kpt(:,ik)/DB_alat*alat(:)
   enddo
 else
   read (io_unit(ID)) 
 endif
 !
 ! GS energies are in Rydbergs(PW)/Hartree(Abinit). Here we have the phonon frequencies square
 !
 if (index(what,"FREQS")>0) then
   read (io_unit(ID)) DB_ph_freq_sq(IDB,:)
   if (trim(DB_kind)=="PWscf") DB_ph_freq_sq(IDB,:)= DB_ph_freq_sq(IDB,:)/4._DP
   do il=1,DB_ph_modes
     PH_W_debye=max(PH_W_debye,real(sqrt(abs(DB_ph_freq_sq(IDB,il))),SP))
   enddo
 else
   read (io_unit(ID)) 
 endif
 !
 if (index(what,"POL_VEC")>0) then
   read (io_unit(ID)) 
   read (io_unit(ID)) DB_ph_pol_vec(IDB,:,:,:)
 endif
 !
 ! External POL_VEC/FREQS files?
 !
 if (index(what,"FREQS")>0 .or. index(what,"POL_VEC")>0) call ELPH_databases_IO_pol_and_freqs(IDB) 
 !
 if (index(what,"GKKP")==0) then
   call local_return()
   return
 endif
 !
 ! PW     energies are in Rydbergs.
 ! Abinit energies are in Hartrees.
 !
 ! YAMBO is in HARTREE
 !
 ! Thus, in the case of PW I need to rescale the gkkp
 !
 !   Here we have <dV/dr>. Now [<V>]=[E] but [<r>]=[E^{-1/2}] so
 !
 !   [<dV/dr>] = [E^{3/2}]   
 !
 dVRY2Ha=2._DP**(-3._DP/2._DP)
 if (trim(DB_kind)=="ABINIT") dVRY2Ha=1._DP
 !
 ! GKKP
 !
 do ik=1,DB_nk
   !
   read (io_unit(ID)) DB_gkkp(:,:,:,ik)
   read (io_unit(ID)) 
   !
   if (IDB==1) read (io_unit(ID)) DB_grad_at_gamma(:,:,:,:,ik)
   !
   DB_gkkp(:,:,:,ik)=DB_gkkp(:,:,:,ik)*dVRY2Ha
   DB_grad_at_gamma(:,:,:,:,ik)=DB_grad_at_gamma(:,:,:,:,ik)*dVRY2Ha
   !
   if (trim(DB_kind)=="PWscf") then
     !
     read (io_unit(ID)) DB_E_k(:,ik)
     read (io_unit(ID)) DB_E_k_plus_q(:,ik)
     !
     do ib=1,DB_nb
       DB_E_k(ib,ik)=DB_E_k(ib,ik)/2.
       DB_E_k_plus_q(ib,ik)=DB_E_k_plus_q(ib,ik)/2.
     enddo
     !
   endif
   !
 enddo
 !
 call local_return()
 !
 contains
   !
   subroutine local_return()
     call io_control(ACTION=RD_CL,ID=ID)
     call io_disconnect(ID,FORCE=.TRUE.)
   end subroutine
   !
end function
