  integer function atomic_number(atom_name)
    implicit none
    integer, parameter           :: number_of_elements = 105
    character(len=*), intent(in) :: atom_name
    character(len=2)             :: elements(number_of_elements)
    integer                      :: i
            
    elements(1:2)=(/'H',                              'He'/)
    elements(3:10)=(/'Li','Be','B','C','N','O','F','Ne'/)
    elements(11:18)=(/'Na','Mg','Al','Si','P','S','Cl','Ar'/)
    elements(19:25)=(/'K','Ca','Sc','Ti','V','Cr','Mn'/)
    elements(26:30)=(/'Fe','Co','Ni','Cu','Zn'/)
    elements(31:36)=(/'Ga','Ge','As','Se','Br','Kr'/)
    elements(37:43)=(/'Rb','Sr','Y','Zr','Nb','Mo','Tc'/)
    elements(44:48)=(/'Ru','Rh','Pd','Ag','Cd'/)
    elements(49:54)=(/ 'In','Sn','Sb','Te','I','Xe'/)
    elements(55:63)=(/'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu'/)
    elements(64:71)=(/'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu'/)
    elements(72:76)=(/'Hf','Ta','W','Re','Os'/)
    elements(77:80)=(/'Ir','Pt','Au','Hg'/)
    elements(81:86)=(/'Tl','Pb','Bi','Po','At','Rn'/)
    elements(87:94)=(/'Fr','Ra','Ac','Th','Pa','U','Np','Pu'/)
    elements(95:103)=(/'Am','Cm','Bk','Cf','Es','Fm','Md','No', 'Lr'/)
    !
    ! Special atoms
    !
    elements(104:105)=(/'hn','Hn'/)
      
    do i = 1, 103
      if ( trim(atom_name) == trim(elements(i)) ) then
        atomic_number = i
        return
      end if
    end do
    do i = 104,105
      if ( trim(atom_name) == trim(elements(i)) ) then
        atomic_number = 1
        return
      end if
    end do
    atomic_number = -1

    return 
  end function atomic_number
