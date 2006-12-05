module qexml_data
  use qexml_module,      ONLY : dbl
  private                    :: dbl 
  save
  public
  
! Cell
  real(dbl)                  :: alat_, a1_(3), a2_(3), a3_(3)

! Atoms
  integer                    :: nat_, nsp_
  real(dbl), allocatable     :: tau_(:,:)
  integer, allocatable       :: ityp_(:)
  character(3), allocatable  :: atm_(:)

! Symmetry
  integer, allocatable       :: s_(:,:,:)
  integer                    :: nsym_
  logical                    :: invsym_

! Planewaves
  integer                    :: ngm_, npwx_
  integer, allocatable       :: igv_(:,:)

! Bandstructure
  integer                    :: nbnd_, num_k_points_, nspin_
  real(dbl)                  :: nelec_
  real(dbl), allocatable     :: eig_s_(:,:,:), eig_(:,:)
  real(dbl), allocatable     :: xk_(:,:)

! igk grids
  integer                    :: npwk_
  integer, allocatable       :: index_(:), igk_(:,:)

! Wavefunctions
  complex(dbl), allocatable  :: wf_kindip_(:,:)

end module qexml_data
