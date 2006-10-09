!
! Copyright (C) 2000-2005 C. Hogan  and the SELF team 
!         http://www.fisica.uniroma2.it/~self
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
module model_loss_function
  !
  ! Model forms of the loss function
  !
  use pars,                   ONLY : SP
  character(2), public            :: model
  integer, parameter              :: nlossform = 11
  integer                         :: lossfm
  character(2)                    :: lossformsh(nlossform)
  character(70)                   :: lossformlg(nlossform)
  data lossformsh/'1b','2b','2e','aq','az','ar','ig','iq','id','ip','ap'/
  data lossformlg/&
& 'isotropic bulk continuum',                       & ! 1  1b
& 'semi-infinite continuum',                        & ! 2  2b
& 'semi-infinite continuum for extended code',      & ! 3  2e
& 'anisotropic form of 3-layer, in q.d -> 0 limit', & ! 4  aq
& 'anisotropic form of 3-layer, complex form',      & ! 5  az
& 'anisotropic form of 3-layer, real form',         & ! 6  ar
& 'isotropic form of 3-layer, general',             & ! 7  ig
& 'isotropic form of 3-layer, in q.d -> 0 limit',   & ! 8  iq
& 'isotropic form of 3-layer, dielectric theory',   & ! 9  id
& 'isotropic form of 3-layer, parallel trajectory', & ! 10 ip
& 'anisotropic form of 3-layer, parallel trajectory'/ ! 11 ap

contains


subroutine init_loss_function(defs) ! REELS exclusive to 3LM
  use it_m,                  ONLY : it, initdefs, E_unit,G_unit,T_unit
  implicit none
  integer, parameter             :: V_more=1, V_qp=2, V_io=3, V_debug=4
  type(initdefs), intent(inout)  :: defs

  call it(defs,'LossForm', '[REELS] Functional form of loss (1b/2b/2e/aq/az/ar/ig/iq/id/ip/ap)', model )
! 
  lossfm=index_eels_form(model)

  return
end subroutine init_loss_function

  integer function index_eels_form(ctmp) 
    implicit none
    integer                        :: itmp,itmp2
    character(2)                   :: ctmp
    itmp = 0
    do  itmp = 1,nlossform
      if(ctmp.eq.lossformsh(itmp)) itmp2 = itmp
    enddo
    index_eels_form = itmp2
    return
  end function index_eels_form

  subroutine print_eels_form
    implicit none
    return
  end subroutine print_eels_form

  real(SP) function model_Img(iform, ebulk, qparm, qpar, bparm, dvac, esurf, dsurf)
    implicit none
    integer,     intent(in)           :: iform
    complex(SP), intent(in)           :: ebulk
    real(SP),    intent(in), optional :: bparm, dsurf, dvac, qparm, qpar(2)
    complex(SP), intent(in), optional :: esurf(3)
!   ws
    real(SP)                          :: fac1, fac2, fac3, qparm_
    complex(SP)                       :: cfac1, cfac2, cfac3, cfac4, esavg
    complex(SP)                       :: esurf_mean, qzetad, eps_eff
    !
    ! qzeta = wavevector of the surface excitation
    !
    
    ! NB: Qpar must be in the reference frame of scattering (ie along X)
    ! qpar and esurf in same reference frame.

    if(present(qpar))  qparm_ = sqrt(qpar(1)**2 + qpar(2)**2)
    if(present(qparm)) qparm_ = qparm_

    select case (iform)
    case(1)
      !
      ! Bulk scattering: continuum picture
      !
      model_Img =  aimag( -1.0_SP / ebulk ) 
      !
    case(2)
      !
      ! Surface scattering: semi-infinite continuum
      !
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + ebulk ) )     
      !
    case(3)
      !
      ! Bulk, far from surface (for new EELS model)
      !
      fac1 = qparm_*cos(bparm*dvac) - bparm*sin(bparm*dvac)
      fac2 = exp(-qparm_*dvac )
      fac3 = aimag( -2.0_SP / ( 1.0_SP + ebulk ) ) 
      model_Img =  fac1 * fac2 * fac3
      !
    case(4)
      !
      ! Three layer model, in limit q_z dsurf -> 0 (anisotropic form)
      !
      esurf_mean = geometric_mean_epsilon(esurf(1), esurf(2), esurf(3), qpar)
      cfac1 = (esurf_mean**2 - ebulk**2) / esurf(3) ! ok
      eps_eff = ebulk + qparm_ * dsurf * cfac1
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + eps_eff ) )     
      !
    case(5)
      !
      ! Three layer model: RDS 86 paper (complex arguments)
      ! Note "-esurf(3)"; qzetad defined from esurf_mean
      !
      esurf_mean = geometric_mean_epsilon(esurf(1), esurf(2), -esurf(3), qpar)
      qzetad = qparm_ * dsurf * esurf_mean / esurf(3)
      eps_eff = eps_effective_DS( esurf_mean, ebulk, qzetad )  
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + eps_eff ) )     
      !
    case(6)
      !
      ! Three layer model: RDS 86 paper/Noguez (real arguments)
      !
      esurf_mean = geometric_mean_epsilon(esurf(1), esurf(2), esurf(3), qpar)
      qzetad = qparm_ * dsurf * esurf_mean / esurf(3) ! ok
      eps_eff = eps_eff_anisotropic_3_layer( esurf_mean, ebulk, qzetad )  
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + eps_eff ) )     
      !
    case(7)
      !
      ! Three layer model: isotropic surface dielectric function (Mills)
      !
      esavg = ( esurf(1) + esurf(2) + esurf(3) )/ 3.0_SP
      qzetad = qparm_ * dsurf 
      eps_eff = eps_eff_anisotropic_3_layer( esavg, ebulk, qzetad )  
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + eps_eff ) )     
      !
    case(8)
      !
      ! Three layer model, isotropic, in limit q dsurf -> 0  (Mills)
      !
      esavg = ( esurf(1) + esurf(2) + esurf(3) )/ 3.0_SP
      cfac1 = esavg - (ebulk**2 / esavg) 
      eps_eff = ebulk + qparm_ * dsurf * cfac1
      model_Img =  aimag( -2.0_SP / ( 1.0_SP + eps_eff ) )     
      !
    case(9)
      !
      ! Three layer model from dielectric theory
      ! Assumes bulk fields are not perturbed.
      !
      esavg = ( esurf(1) + esurf(2) + esurf(3) )/ 3.0_SP
      fac1 = dsurf * aimag(esavg) / abs(ebulk+1.0_SP)**2
      fac2 = dsurf * abs(ebulk)**2 / abs(ebulk+1.0_SP)**2 &
&                   * aimag(-1.0_SP/esavg)
      model_Img = fac1 + fac2
      !
    case(10)
      !
      ! Three layer model, parallel (non reflecting) trajectory
      ! Howie/Batson: isotropic surface layer
      ! Here dvac is the impact parameter
      !
      esavg = ( esurf(1) + esurf(2) + esurf(3) )/ 3.0_SP
      cfac1 = ebulk + esavg
      cfac2 = esavg - 1.0_SP
      cfac3 = ebulk - esavg
      cfac4 = esavg + 1.0_SP
      fac1  = exp(-2.0_SP*qparm_*dsurf)    ! check K/lambda
      fac2  = exp(-2.0_SP*qparm_*dvac)/qparm_ ! decay term
      model_Img = fac2 * &
&     aimag( ( cfac1*cfac2 + cfac3*cfac4*fac1 ) /  &
&            ( cfac1*cfac4 + cfac3*cfac2*fac1 ) )
      !
    case(11)
      !
      ! Three layer model, parallel (non reflecting) trajectory
      ! Lambin, Henrard, etc, anisotropic surface layer
      ! Here dvac is the impact parameter
      !
      esurf_mean = geometric_mean_epsilon(esurf(1), esurf(2), esurf(3), qpar)
      qzetad = qparm_ * dsurf * esurf_mean / esurf(3) ! ok
      cfac1 = ebulk + esurf_mean
      cfac2 = esurf_mean - 1.0_SP
      cfac3 = ebulk - esurf_mean
      cfac4 = esurf_mean + 1.0_SP
      ! Check qzetad=complex; fac1=real !
      fac1  = exp(-2.0_SP*real(qzetad)*dsurf)    ! check K/lambda
!     fac1  = exp(-2.0_SP*qzetad*dsurf)    ! check K/lambda
      fac2  = exp(-2.0_SP*qparm_*dvac)     ! decay term
      model_Img = fac2 * &
&     aimag( ( cfac1*cfac2 + cfac3*cfac4*fac1 ) / &
&            ( cfac1*cfac4 + cfac3*cfac2*fac1 ) )
      !
    end select 
    return 
  end function model_Img

  complex(SP) function eps_effective_DS( esurf_mean, ebulk, qzetad )
    !
    ! The Del Sole + Selloni 86 result (complex qzeta)
    !
    implicit none
    complex(SP), intent(in)         :: esurf_mean, ebulk, qzetad
    complex(SP)                     :: esurf_mean_, qzetad_

    esurf_mean_ = esurf_mean
    qzetad_     = qzetad
!<-- DEBUG
!   if (aimag(qzetad) < 0.0_SP) then 
!     esurf_mean_ = -esurf_mean
!     qzetad_     = -qzetad_
!   endif
!<-- DEBUG
    eps_effective_DS = esurf_mean_ * &
&                       (ebulk*cos(qzetad_) - esurf_mean_*sin(qzetad_) )/ &
&                 (esurf_mean_*cos(qzetad_) +       ebulk*sin(qzetad_) )
    return
  end function eps_effective_DS

  complex(SP) function eps_eff_anisotropic_3_layer( esurf_mean, ebulk, qzetad )
    !
    ! The Noguez/Del Sole + Selloni 86 result (real qzeta).
    ! The two forms are formally equivalent.
    !
    implicit none
    complex(SP), intent(in)         :: esurf_mean, ebulk, qzetad
    complex(SP)                     :: cfac1, cfac2

    cfac1 = (esurf_mean + ebulk)*exp(qzetad)
    cfac2 = (esurf_mean - ebulk)*exp(-qzetad)
    eps_eff_anisotropic_3_layer = esurf_mean * (cfac1 - cfac2) / (cfac1 + cfac2)

    return
  end function eps_eff_anisotropic_3_layer

  complex(SP) function geometric_mean_epsilon(exx, eyy, ezz, qpar)
    implicit none
    complex(SP), intent(in)         :: exx, eyy, ezz
    real(SP),    intent(in)         :: qpar(2)
    real(SP)                        :: qparm2
    complex(SP)                     :: esurf_par

    qparm2 = qpar(1)**2 + qpar(2)**2
    esurf_par = qpar(1)**2 / qparm2 * exx + & ! cos^2(T) e_xx +
&               qpar(2)**2 / qparm2 * eyy     ! sin^2(T) e_yy

    geometric_mean_epsilon = sqrt( esurf_par * ezz ) 

    return
  end function geometric_mean_epsilon

end module model_loss_function
