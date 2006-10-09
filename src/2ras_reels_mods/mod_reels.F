module reels_module
  use pars,                 ONLY : SP
  integer                       :: npol
  logical                       :: lepsmix
  logical                       :: lreels

contains

subroutine init_reels(defs)
  use pars,                  ONLY : SP, schlen
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit
  implicit none
  type(initdefs), intent(inout)  :: defs
  integer, parameter             :: V_more=1, V_qp=2, V_io=3, V_debug=4
  real(SP)                       :: limits(3)
  character(schlen)              :: inttype, mixing

  call it('f',defs,'EpsMix'  , '[REELS] DF mixing (`on`;`off`)', lepsmix )
  call it(defs,'npol'  , '[REELS] Number of polarizations (`1`,`2`)', npol )

  return
end subroutine init_reels


end module reels_module
