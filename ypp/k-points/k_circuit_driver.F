! 
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): DS, CA, AM
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
subroutine k_circuit_driver(PATH,n_points)
 !
 use pars,           ONLY:SP
 use D_lattice,      ONLY:alat,a
 use R_lattice,      ONLY:k_the_nearest
 use electrons,      ONLY:n_sp_pol
 use com,            ONLY:msg
 use interfaces,     ONLY:io_DB1_selective_scan
 use YPP_interfaces, ONLY:k_special
 use YPPm,           ONLY:BANDS_steps,&
&                         CIRCUIT_E_DB_path,CIRCUIT_E_db,USER_k,CIRCUIT_k,&
&                         CIRCUIT_made_of_SPECIAL_k,BANDS_from_db,SPECIAL_k,&
&                         BANDS_path,CIRCUIT_k_label,SPECIAL_k_label
 !
#include<memory.h>
 !
 character(*) :: PATH
 integer      :: n_points
 !
 ! Work space
 !
 real(SP)              :: v(3),db_a(3,3),db_alat(3)
 integer               :: ik,i_err,ik_special
 !
 if (BANDS_steps==0) return
 !
 call section('=',"Circuit analyzer/generator") 
 !
 ! Circuit made of specials?
 !===========================
 call k_circuit_made_of_special_points(PATH,n_points)
 !
 if (CIRCUIT_made_of_SPECIAL_k) return
 !
 ! Circuit made of USER points?
 !==============================
 call k_circuit_made_of_user_points( )
 !
 if (USER_k%nbz>0) then
   call label_the_circuit()
   return
 endif
 !
 ! Load the path from a pre-existing SAVE
 !========================================
 if (.not.trim(CIRCUIT_E_DB_path)=="none") then
   i_err=io_DB1_selective_scan("nk nb alat lattice_vectors",trim(CIRCUIT_E_DB_path),&
&                                CIRCUIT_E_db,CIRCUIT_k,db1_a=db_a,db1_alat=db_alat)
   if (i_err/=0) call error("Reading the "//trim(CIRCUIT_E_DB_path)//" SAVE database")
   CIRCUIT_k%nbz=CIRCUIT_k%nibz
   if (any(alat/=db_alat).or.any(a/=db_a)) call warning("Lattice constants mismatch")
   call msg("s",'GS databases         : '//trim(CIRCUIT_E_DB_path))
   call msg("s",'bands/k-points       :',(/CIRCUIT_E_db%nb, CIRCUIT_k%nbz/))
   YAMBO_ALLOC(CIRCUIT_k%pt,(CIRCUIT_k%nbz,3))
   YAMBO_ALLOC(CIRCUIT_E_db%E,(CIRCUIT_E_db%nb,CIRCUIT_k%nbz,n_sp_pol))
   i_err=io_DB1_selective_scan('E K',trim(CIRCUIT_E_DB_path),CIRCUIT_E_db,CIRCUIT_k)
   BANDS_from_db=.TRUE.
   call label_the_circuit()
   return
 endif
 !
 ! No circuit
 !============
 if (CIRCUIT_k%nbz==0) then
   call k_special(.TRUE.)
   call error(' No BZ circuit defined in input')
 endif
 !
 contains
   !
   subroutine label_the_circuit
     !
     ! Find the Special K belonging to the circuit
     !=============================================
     !
     allocate(CIRCUIT_k_label(CIRCUIT_k%nbz))
     CIRCUIT_k_label=" "
     BANDS_path=" "
     if (SPECIAL_k%nbz>0) then
       do ik=1,CIRCUIT_k%nbz
         ik_special=k_the_nearest(CIRCUIT_k%pt(ik,:),SPECIAL_k%pt(:SPECIAL_k%nbz,:),SPECIAL_k%nbz,i_err=i_err)
         if (i_err==0) then
           CIRCUIT_k_label(ik)=trim(SPECIAL_k_label(ik_special))
           BANDS_path=trim(BANDS_path)//" "//trim(SPECIAL_k_label(ik_special))
         endif
       enddo
       if (len_trim(BANDS_path)>0) call msg("s",'Circuit path         :',trim(BANDS_path))
     endif
     !
   end subroutine
   !
end subroutine k_circuit_driver
