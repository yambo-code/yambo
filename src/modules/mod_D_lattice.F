!
! Copyright (C) 2000-2008 A. Marini and the YAMBO team 
!              http://www.yambo-code.org
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
module D_lattice
 !
 use pars, ONLY:SP,lchlen
 implicit none
 !
 ! Temperature
 !
 real(SP) :: Tel
 real(SP) :: default_Tel
 logical  :: input_Tel_fixed
 !
 ! SYMs
 !
 integer :: nsym
 integer :: i_time_rev  ! =1 yes TR , =0 no TR
 integer :: i_space_inv ! =1 yes SI,  =0 no SI
 integer :: inv_index ! Index of the symmetry corresponding to -I (independently 
                      ! on the value of i_time_rev and i_space_inv
 real(SP),allocatable :: dl_sop(:,:,:)
 integer, allocatable :: sop_tab(:,:)
 integer, allocatable :: sop_inv(:)
 !
 ! Cell & atoms
 !
 integer  :: n_atoms_max
 integer  :: n_atom_species
 real(SP) :: DL_vol
 real(SP) :: a(3,3)
 real(SP) :: alat(3)
 integer,  allocatable :: n_atoms(:)
 integer,  allocatable :: znucl(:)
 real(SP), allocatable :: atom_pos(:,:,:)
 character(lchlen)     :: atoms_string
 !
 ! Periodic Table
 character(len=2)      :: PT_elements(103)
 !
 ! KB Pseudo
 !
 integer :: pp_n_l_comp_max
 integer :: pp_kbv_dim
 integer,  allocatable:: pp_n_l_comp(:)
 real(SP), allocatable:: pp_kbs(:,:)
 real(SP), allocatable:: pp_kb(:,:,:)
 real(SP), allocatable:: pp_kbd(:,:,:)
 !
 contains
   !
   subroutine PP_alloc()
     use memory_m,   ONLY:mem_est
     use wave_func,  ONLY:wf_ng
     integer  :: alloc_err(3)
     allocate(pp_kbs(n_atom_species,pp_n_l_comp_max),stat=alloc_err(1))
     allocate(pp_kb(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(2))
     allocate(pp_kbd(wf_ng,n_atom_species,pp_n_l_comp_max),stat=alloc_err(3))
     call mem_est("pp_kbs pp_kb pp_kbd",&
&                 (/size(pp_kbs),size(pp_kb),size(pp_kbd)/),&
&                 elements_kind=(/SP,SP,SP/),errors=alloc_err)
   end subroutine
   !
   subroutine PP_free()
     use memory_m,   ONLY:mem_est
     if (.not.allocated(pp_kbs)) return
     deallocate(pp_kbs,pp_kb,pp_kbd)
     call mem_est("pp_kbs pp_kb pp_kbd")
   end subroutine
   !
   subroutine atoms_spatial_invertion()
     use pars,           ONLY:zero_dfl
     use matrix_operate, ONLY:m3inv
     !
     ! Work Space
     !
     integer  :: ia,iap,is
     real(SP) :: at_diff(3),a_m1(3,3)
     !
     if (n_atoms_max==0) return
     !
     i_space_inv=1
     !
     call m3inv(transpose(a),a_m1)
     !
     do is=1,n_atom_species
       atoms_loop: do ia=1,n_atoms(is)
         do iap=1,n_atoms(is) 
           !
           at_diff=matmul(a_m1,atom_pos(:,ia,is)+atom_pos(:,iap,is))
           !
           at_diff(1)=abs(at_diff(1)-nint(at_diff(1)))     
           at_diff(2)=abs(at_diff(2)-nint(at_diff(2)))     
           at_diff(3)=abs(at_diff(3)-nint(at_diff(3)))     
           !
           if (all(at_diff<=zero_dfl)) cycle atoms_loop
           !
         enddo 
         !
         i_space_inv=0
         return
         !
       enddo atoms_loop
     enddo
     !
   end subroutine
   !
   integer function atomic_number(atom_name)
     character(len=2) :: atom_name
     integer  :: ia ! Work Space
     call load_PT_elements()
     do ia = 1, 103
      if ( trim(atom_name) == trim(PT_elements(ia)) ) then
        atomic_number = ia
        return
      end if
     end do
   end function
   !
   subroutine load_PT_elements()
     PT_elements(1:2)  =(/'H ',                              'He'/)
     PT_elements(3:10) =(/'Li','Be','B ','C ','N ','O ','F ','Ne'/)
     PT_elements(11:18)=(/'Na','Mg','Al','Si','P ','S ','Cl','Ar'/)
     PT_elements(19:25)=(/'K ','Ca','Sc','Ti','V ','Cr','Mn'/)
     PT_elements(26:30)=(/'Fe','Co','Ni','Cu','Zn'/)
     PT_elements(31:36)=(/'Ga','Ge','As','Se','Br','Kr'/)
     PT_elements(37:43)=(/'Rb','Sr','Y ','Zr','Nb','Mo','Tc'/)
     PT_elements(44:48)=(/'Ru','Rh','Pd','Ag','Cd'/)
     PT_elements(49:54)=(/'In','Sn','Sb','Te','I ','Xe'/)
     PT_elements(55:63)=(/'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu'/)
     PT_elements(64:71)=(/'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu'/)
     PT_elements(72:76)=(/'Hf','Ta','W ','Re','Os'/)
     PT_elements(77:80)=(/'Ir','Pt','Au','Hg'/)
     PT_elements(81:86)=(/'Tl','Pb','Bi','Po','At','Rn'/)
     PT_elements(87:94)=(/'Fr','Ra','Ac','Th','Pa','U ','Np','Pu'/)
     PT_elements(95:103)=(/'Am','Cm','Bk','Cf','Es','Fm','Md','No', 'Lr'/)
   end subroutine
   !
end module D_lattice
