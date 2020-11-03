!
!        Copyright (C) 2000-2020 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM, FP
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
subroutine ELPH_databases_IO_pol_and_freqs(IDB)
 !
 use pars,           ONLY:SP 
 use D_lattice,      ONLY:n_atoms
 use units,          ONLY:HA2CMm1
 use YPP_ELPH,       ONLY:ph_freqs_file,ph_modes_file,DB_ph_modes,DB_PH_qpt,&
&                         use_ext_ph_freqs,use_ext_ph_modes,DB_ph_freq_sq,DB_ph_pol_vec
 use vec_operate,    ONLY:v_is_zero
 use zeros,          ONLY:k_iku_zero
 !
 implicit none
 !
 integer :: IDB
 !
 ! Work Space
 !
 integer           :: i1,ic,ia
 character(1)      :: ws_ch(2)
 real(SP)          :: ph_q(3),w_vec_pot(6)
 !
 if (use_ext_ph_freqs) then
   open(unit=99,file=trim(ph_freqs_file))
   read(99,*)
   do i1=1,(IDB-1)*(1+ceiling(real(DB_ph_modes)/6.))
     read(99,*) ph_q 
   enddo
   read(99,*) ph_q 
   if (.not.v_is_zero(ph_q(:)-real(DB_PH_qpt(IDB,:)),zero_=k_iku_zero).and.&
&      .not.v_is_zero(ph_q(:)+real(DB_PH_qpt(IDB,:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_freqs_file))
   do i1=1,DB_ph_modes,6
     read(99,*) DB_ph_freq_sq(IDB,i1:min(i1+5,DB_ph_modes))
   enddo
   DB_ph_freq_sq(IDB,:)=(abs(DB_ph_freq_sq(IDB,:))/HA2CMm1)**2.
   close(99)
   !DEBUG>
   !if (ik==1) then
   !  write (*,'(a,i3)') 'q=',iabs(IDB)
   !  do il=1,ph_modes
   !    pol_vec_norm=0._SP
   !    do ia=1,n_atoms
   !      do ic=1,3
   !        pol_vec_norm=pol_vec_norm+conjg(pol_vec(il,ia,ic))*pol_vec(il,ia,ic)
   !      enddo
   !      write (*,'(a,i2,6f10.6)') 'Matd file ia',ia,pol_vector(il,ia,:)/159.25556471558120362737
   !      write (*,'(a,i2,6f10.6)') 'DBPH file ia',ia,pol_vec(il,ia,:)
   !    enddo
   !    write (*,'(a,i3,a,f9.6)') 'mode=',il,' norm=',pol_vec_norm
   !  enddo
   !endif
   !DEBUG<
 endif
 !
 if (use_ext_ph_modes) then
   open(unit=99,file=trim(ph_modes_file))
   read(99,*)
   read(99,*)
   read(99, *) ws_ch(1),ws_ch(2),ph_q 
   if (.not.v_is_zero(ph_q(:)-real(DB_PH_qpt(IDB,:)),zero_=k_iku_zero).and.&
&      .not.v_is_zero(ph_q(:)+real(DB_PH_qpt(IDB,:)),zero_=k_iku_zero)) call error("Incorrect q-point in "//trim(ph_modes_file))
   read(99,*)
   do i1=1,DB_ph_modes
     read(99,*) 
     do ia=1,n_atoms
       read(99,*) ws_ch(1),w_vec_pot
       forall(ic=1:3) DB_ph_pol_vec(IDB,i1,ia,ic)=cmplx(w_vec_pot(2*(ic-1)+1),w_vec_pot(2*ic),SP)
     enddo
   enddo
   close(99)
 endif
 !
end subroutine
