module reels_module
  use pars,                 ONLY : SP
  integer                       :: npol, nfull
  logical                       :: lepsmix
  logical                       :: lreels
  complex(SP), allocatable,save :: epsm1_avA(:,:,:)
  real(SP), allocatable         :: loss(:,:,:)

contains

subroutine init_reels(defs)
  use pars,                  ONLY : SP, schlen
  use it_m,                  ONLY : it, initdefs, V_more
  implicit none
  type(initdefs), intent(inout)  :: defs

  call it('f',defs,'EpsMix'  , '[REELS] DF mixing (`on`;`off`)', verb_level=V_more )
  call it(defs,'NumPol'  , '[REELS] Number of polarizations (`1`,`2`)', npol )
  call it(defs,'ETStpsLs'  , '[REELS] Total Energy Steps in Loss function', nfull, verb_level=V_more )

  return
end subroutine init_reels

end module reels_module
