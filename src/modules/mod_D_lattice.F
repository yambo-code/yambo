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
module D_lattice
 !
 use pars,        ONLY:SP,lchlen,cONE,cZERO,cI,schlen
 !
#include<dev_defs.h>
#include<memory.h>
 !
 ! Non periodic directions
 !
 character(lchlen) :: non_periodic_directions
 !
 !
 ! Temperature
 !
 real(SP) :: Tel
 real(SP) :: T_elecs
 real(SP) :: T_holes
 real(SP) :: Bose_Temp
 real(SP) :: input_GS_Tel
 logical  :: input_Tel_is_negative
 !
 ! SYMs
 !
 integer :: nsym
 integer :: i_time_rev  ! =1 yes TR , =0 no TR
 integer :: i_space_inv ! =1 yes SI,  =0 no SI
 integer :: inv_index   ! Index of the symmetry corresponding to -I (independently 
                        ! on the value of i_time_rev and i_space_inv
 integer :: idt_index   ! Indentity I  index
 logical :: mag_syms
 real(SP),allocatable    :: dl_sop(:,:,:)
 complex(SP),allocatable :: spin_sop(:,:,:)
 integer, allocatable    :: sop_tab(:,:)
 integer, allocatable    :: sop_inv(:)
 integer, allocatable    :: irt(:,:,:)     ! (nsym,nat,n_at_species) gives the index of a rotated atomic position
 integer, allocatable    :: icell(:,:,:,:) ! (3,nsym,nat,n_at_species) gives the referene cell after a symm is operated
 ! device memory
 complex(SP),allocatable DEV_ATTRIBUTE :: spin_sop_d(:,:,:)
 integer,    allocatable DEV_ATTRIBUTE :: sop_inv_d(:)
 !
 ! Pauli matrices
 !
 complex(SP), dimension(2,2), parameter :: sigma_0=reshape((/ cONE , cZERO, cZERO, cONE /),(/2,2/))
 complex(SP), dimension(2,2), parameter :: sigma_x=reshape((/ cZERO, cONE , cONE , cZERO/),(/2,2/))
 complex(SP), dimension(2,2), parameter :: sigma_y=reshape((/ cZERO,-cI   , cI   , cZERO/),(/2,2/))
 complex(SP), dimension(2,2), parameter :: sigma_z=reshape((/ cONE , cZERO, cZERO,-cONE /),(/2,2/))
 !
 ! Cell & atoms
 !
 logical  :: l_0D,l_1D,l_2D,l_3D
 integer  :: n_atoms
 integer  :: n_atoms_species_max
 integer  :: n_atomic_species
 real(SP) :: DL_vol
 real(SP) :: a(3,3)
 real(SP) :: alat(3)
 integer,  allocatable :: PW_atomic_kind(:)
 integer,  allocatable :: n_atoms_species(:)
 integer,  allocatable :: Z_species(:)
 real(SP), allocatable :: atom_pos(:,:,:)
 character(lchlen)     :: atoms_string
 character(schlen)     :: lattice
 !
 ! Centered cartesian coordinates inside the cell
 !
 real(SP),    allocatable :: x_cc(:,:,:)
 real(SP)                 :: x_CM(3) ! position of the center of mass
 !
 ! Gradients
 !
 integer, allocatable  :: R_m_R(:,:)
 complex, allocatable  :: Grad_R(:,:,:)
 !
 ! Periodic Table
 !
 character(len=2)      :: PT_elements(0:103)
 !
 contains
   !
   subroutine symmetry_group_table(msg_where)
     !
     use pars,           ONLY:SP,IP
     use com,            ONLY:msg
     implicit none
     !
     character(*) :: msg_where
     !
     ! Work Space
     !
     integer  :: i1,i2,i3
     real(SP) :: m(3,3)
     !
     YAMBO_FREE(sop_tab)
     YAMBO_FREE(sop_inv)
     YAMBO_ALLOC(sop_tab,(nsym,nsym))
     YAMBO_ALLOC(sop_inv,(nsym))
     sop_tab=0
     sop_inv=0
     do i1=1,nsym
       do i2=1,nsym
         m=matmul(dl_sop(:,:,i1),dl_sop(:,:,i2))
         do i3=1,nsym
           if (all(abs(m-dl_sop(:,:,i3))<=1.E-5)) then
             if (sop_tab(i1,i2)/=0) call error('[SYMs] check the input symmetries!')        
             sop_tab(i1,i2)=i3
             if (sop_tab(i1,i2)==1) sop_inv(i1)=i2                
           endif
         enddo
         if (sop_tab(i1,i2)==0) call error('[SYMs] check the input symmetries!')    
       enddo
     enddo
     if (any(sop_inv==0)) call error('[SYMs] check the input symmetries!')
     call msg(msg_where,'Group table correct   ',.true.)
     !
#ifdef _CUDA
     if(allocated(sop_inv_d)) deallocate(sop_inv_d)
     allocate(sop_inv_d,source=sop_inv)
#endif
     !
   end subroutine symmetry_group_table
   !
   subroutine atoms_spatial_inversion()
     use pars,           ONLY:zero_dfl
     use matrix_operate, ONLY:m3inv
     implicit none
     !
     ! Work Space
     !
     integer  :: ia,iap,is
     real(SP) :: at_diff(3),a_m1(3,3)
     !
     if (n_atoms_species_max==0) return
     !
     i_space_inv=1
     !
     call m3inv(transpose(a),a_m1)
     !
     do is=1,n_atomic_species
       atoms_loop: do ia=1,n_atoms_species(is)
         do iap=1,n_atoms_species(is) 
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
   end subroutine atoms_spatial_inversion
   !
   subroutine symmetry_equiv_atoms()
     use pars,           ONLY:zero_dfl
     use matrix_operate, ONLY:m3inv
     implicit none
     !
     ! Work Space
     !
     integer  :: ia,iap,is,isym,itmp(3)
     real(SP) :: v1(3),at_diff(3),a_m1(3,3)
     logical  :: lfound
     !
     if (n_atoms_species_max==0) return
     !
     YAMBO_FREE(irt)
     YAMBO_FREE(icell)
     YAMBO_ALLOC(irt,(nsym,maxval(n_atoms_species),n_atomic_species))
     YAMBO_ALLOC(icell,(3,nsym,maxval(n_atoms_species),n_atomic_species))
     !
     call m3inv(transpose(a),a_m1)
     !
     do isym=1,nsym
         !
         ! handle time-reversal
         if(isym>nsym/(1+i_time_rev)) then
           !
           irt(isym,:,:)=irt(isym-nsym/(1+i_time_rev),:,:)
           icell(:,isym,:,:)=icell(:,isym-nsym/(1+i_time_rev),:,:)
           !
           cycle
         endif
         !
         do is=1,n_atomic_species
         do ia=1,n_atoms_species(is)
           !
           v1=matmul(dl_sop(:,:,isym),atom_pos(:,ia,is))
           !
           lfound=.false.
           !
           search_loop:&
           do iap=1,n_atoms_species(is)
             !
             at_diff=matmul(a_m1,atom_pos(:,iap,is)-v1(:))
             itmp=nint(at_diff)
             !
             if (all(abs(at_diff-itmp)<=zero_dfl)) then
                lfound=.true.
                irt(isym,ia,is)=iap
                ! The - sign comes from a convention definition
                ! shared with WanT
                icell(:,isym,ia,is)=-itmp(:)
                exit search_loop
             endif
             !
           enddo search_loop
           !
           if (.not.lfound) call error(' [SYMs] equivalent atom not found')
           !
         enddo
         enddo
         !
     enddo
     !
   end subroutine symmetry_equiv_atoms

   subroutine atom_mapper(nat,tau,tau_units,map)
     use pars,           ONLY:zero_dfl
     implicit none
     !
     integer, intent(in) :: nat
     real(SP),intent(in) :: tau(3,nat)
     character(*), intent(in) :: tau_units  ! "c" | "a" | "i"
     integer, intent(out):: map(maxval(n_atoms_species),n_atomic_species)
     !
     ! Work Space
     !
     integer  :: ia,iap,is
     real(SP), allocatable :: tau_(:,:)
     real(SP) :: at_diff(3)
     !
     YAMBO_ALLOC(tau_,(3,nat))
     !
     select case(trim(tau_units))
     case("c","cc") ! cartesian units in bohr
       tau_=tau
     !case("a","rlu") 
     !  do ia = 1, nat
     !    call c2a(a,tau(:,ia),tau_(:,ia),"a2c")
     !  enddo
     !case("i","iku")
     !  do ia = 1, nat
     !    call c2a(a,tau(:,ia),tau_(:,ia),"i2c")
     !  enddo
     case default
       call error(' [Atoms] Invalid tau_units = '//trim(tau_units))
     end select
     !
     do is=1,n_atomic_species
     do ia=1,n_atoms_species(is)
        !
        map(ia,is)=0
        !
        search_loop:&
        do iap = 1, nat
          !
          at_diff=atom_pos(:,ia,is)-tau_(:,iap)
          !
          if (all(abs(at_diff)<=zero_dfl)) then
            map(ia,is)=iap
            exit search_loop
          endif
          !
        enddo search_loop
        !
        if (any(map(:,:)==0)) call error(' [Atoms] incomplete mapping found')
        !
     enddo
     enddo
     !
     YAMBO_FREE(tau_)
     !
   end subroutine atom_mapper
   !
   integer function atomic_number(atom_name)
     character(len=2) :: atom_name
     integer  :: ia ! Work Space
     call load_PT_elements()
     do ia = 0, 103
      if ( trim(atom_name) == trim(PT_elements(ia)) ) then
        atomic_number = ia
        return
      end if
     end do
     atomic_number = 0
   end function
   !
   subroutine load_PT_elements()
     PT_elements(0)    ='**'
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
