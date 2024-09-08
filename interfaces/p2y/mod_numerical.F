! Self-energies and eXcitations (SaX)
! Copyright (C) 2006 SaX developers team
! 
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
module numerical_module
  use pars,  ONLY:SP
! use num_interpolation_module
  implicit none
  private
  public :: num_ylm,        &
            num_xlylm,      &
            num_xlylm_grad, &
            num_xmlsphbes, &
            num_simpson
!           num_interpolation_init, &
!           num_interpolation_destroy, &
!           num_interpolation
public :: num_2pi_i, num_4pi

real(SP),    parameter :: num_sqrt2   = 1.41421356237309504880
real(SP),    parameter :: num_pi      = 3.14159265358979323844
real(SP),    parameter :: num_2pi     = num_pi * 2.0
real(SP),    parameter :: num_4pi     = num_pi * 4.0
real(SP),    parameter :: num_8pi     = num_pi * 8.0
real(SP),    parameter :: num_sqrtpi  = 1.77245385090551602729
real(SP),    parameter :: num_sqrt8pi = 2.0 * num_sqrt2 * num_sqrtpi
complex(SP), parameter :: num_2pi_i   = num_pi * (0.0,2.0)
complex(SP), parameter :: num_pi_i    = num_pi * (0.0,1.0)


! Private parameters for ylm
  real(SP), parameter :: c00 =  .28209479177387814347, &
                   c11 =  .34549414947133547925, &
                   c10 =  .48860251190291992158, &
                   c22 =  .38627420202318958029, &
                   c21 =  .77254840404637916067, &
                   c20 =  .63078313050504001206, &
                   c33 =  .41722382363278408972, &
                   c32 = 1.02198547643328236339, &
                   c31 =  .32318018411415065300, &
                   c30 =  .74635266518023078282
!@ END MANUAL
!type num_interpolation
!  integer      :: n
!  real         :: xmin,xmax
!  real,pointer :: y(:),x(:)
!  integer      :: parity
!end type num_interpolation 

  contains

!subroutine num_interpolation_init(interpolation,xmin,xmax,delta,parity)
!! Initialise the table.
!! delta is an excess approximation to the interpoint distance
!! (the point are equally spaced and span the interval [xmin,xmax]).
!! If parity/=0, then xmin should be 0.0 and xmax should be positive.
!  type (num_interpolation), intent(out) :: interpolation
!  real,                     intent(in)  :: xmin,xmax,delta
!  integer, optional,        intent(in)  :: parity
!  integer :: i
!  interpolation%n = ceiling(abs(xmax - xmin)/delta) * sign(1.0,(xmax - xmin))
!  allocate(interpolation%x(0:interpolation%n))
!  allocate(interpolation%y(0:interpolation%n))
!  interpolation%xmin = xmin
!  interpolation%xmax = xmax
!  do i=0,interpolation%n
!    interpolation%x(i) = xmin + (xmax - xmin) * real(i) / interpolation%n
!  end do
!  interpolation%y=0.0
!  if(present(parity)) then
!    interpolation%parity = parity
!  else
!    interpolation%parity = 0
!  end if
!  if(interpolation%parity /= 0 .and. (xmin /= 0.0 .or. xmax<0.0)) then
!!   WARNING("")
!    interpolation%parity = 0
!  end if
!end subroutine num_interpolation_init
!
!subroutine num_interpolation_destroy(interpolation)
!! Destroys the table
!  type (num_interpolation), intent(inout) :: interpolation
!  deallocate(interpolation%x)
!  deallocate(interpolation%y)
!end subroutine num_interpolation_destroy
!
function num_simpson(f)
! Perform simple simpson integral
  real(SP), intent(in) :: f(:)
  real(SP) :: num_simpson
  integer :: n,i
  real(SP) :: sum
  n = size(f)
  sum = 0.0
  do i=2,n-1,2
    sum = sum + f(i-1) + 4.0*f(i) + f(i+1)
  end do
  num_simpson = sum / 3.0
end function num_simpson

function num_xlylm_grad(x,l,m)
! Returns the gradient of xlylm
  complex(SP)             :: num_xlylm_grad(3)
  real(SP), intent(in)    :: x(3)
  integer, intent(in) :: l,m
  if(abs(m)>l) call errore("num_xlylm_grad","m>l",1)
  select case(l)
  case(0)
    num_xlylm_grad(:) = 0.0
  case(1)
    select case(m)
    case(+1)
      num_xlylm_grad(1) = - c11
      num_xlylm_grad(2) = - c11 * (0.0,1.0)
      num_xlylm_grad(3) = 0.0
    case(0)
      num_xlylm_grad(1) = 0.0
      num_xlylm_grad(2) = 0.0
      num_xlylm_grad(3) = + c10
    case(-1)
      num_xlylm_grad(1) = + c11
      num_xlylm_grad(2) = + c11 * (-(0.0,1.0))
      num_xlylm_grad(3) = 0.0
    end select
  case(2)
    select case(m)
    case(+2)
      num_xlylm_grad(1) = + c22 * 2.0 * (x(1) + (0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c22 * 2.0 * (x(1) + (0.0,1.0)*x(2)) * (0.0,1.0)
      num_xlylm_grad(3) = 0.0
    case(+1)
      num_xlylm_grad(1) = - c21 * x(3)
      num_xlylm_grad(2) = - c21 * x(3) * (0.0,1.0)
      num_xlylm_grad(3) = - c21 * (x(1) + (0.0,1.0)*x(2))
    case(0)
      num_xlylm_grad(1) = + c20 * (-x(1))
      num_xlylm_grad(2) = + c20 * (-x(2))
      num_xlylm_grad(3) = + c20 * 2.0 * x(3)
    case(-1)
      num_xlylm_grad(1) = + c21 * x(3)
      num_xlylm_grad(2) = + c21 * x(3) * (-(0.0,1.0))
      num_xlylm_grad(3) = + c21 * (x(1) - (0.0,1.0)*x(2))
    case(-2)
      num_xlylm_grad(1) = + c22 * 2.0 * (x(1) - (0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c22 * 2.0 * (x(1) - (0.0,1.0)*x(2)) * (-(0.0,1.0))
      num_xlylm_grad(3) = 0.0
    end select
  case(3)
    select case(m)
    case(+3)
      num_xlylm_grad(1) = - c33 * (3.0*x(1)**2+(0.0,1.0)*6.0*x(1)*x(2)-3.0*x(2)**2)
      num_xlylm_grad(2) = - c33 * (3.0*(0.0,1.0)*x(1)**2-6.0*x(1)*x(2)-3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = 0.0
    case(+2)
      num_xlylm_grad(1) = + c32 * x(3)*(2.0*x(1)+2.0*(0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c32 * x(3)*(2.0*(0.0,1.0)*x(1)-2.0*x(2))
      num_xlylm_grad(3) = + c32 * (x(1) + (0.0,1.0)*x(2))**2
    case(+1)
      num_xlylm_grad(1) = - c31 * (4.0*x(3)**2-3.0*x(1)**2-x(2)**2-2.0*(0.0,1.0)*x(1)*x(2))
      num_xlylm_grad(2) = - c31 * (-2.0*x(2)*x(1)+(0.0,1.0)*4.0*x(3)**2- &
                                  (0.0,1.0)*x(1)**2-3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = - c31 * (x(1)+(0.0,1.0)*x(2))*8.0*x(3)
    case(0)
      num_xlylm_grad(1) = - c30*0.5*6.0*x(3)*x(1)
      num_xlylm_grad(2) = - c30*0.5*6.0*x(3)*x(2)
      num_xlylm_grad(3) = + c30*0.5*(6.0*x(3)**2-3.0*(x(1)**2+x(2)**2))
    case(-1)
      num_xlylm_grad(1) = + c31 * (4.0*x(3)**2-3.0*x(1)**2-x(2)**2+2.0*(0.0,1.0)*x(2)*x(1))
      num_xlylm_grad(2) = + c31 * (-2.0*x(2)*x(1)-(0.0,1.0)*4.0*x(3)**2+ &
                                  (0.0,1.0)*x(1)**2+3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = + c31 * (x(1)-(0.0,1.0)*x(2))*8.0*x(3)
    case(-2)
      num_xlylm_grad(1) = + c32 * x(3)*(2.0*x(1)-2.0*(0.0,1.0)*x(2))
      num_xlylm_grad(2) = + c32 * x(3)*(-2.0*(0.0,1.0)*x(1)-2.0*x(2))
      num_xlylm_grad(3) = + c32 * (x(1) - (0.0,1.0)*x(2))**2
    case(-3)
      num_xlylm_grad(1) = + c33 * (3.0*x(1)**2-(0.0,1.0)*6.0*x(1)*x(2)-3.0*x(2)**2)
      num_xlylm_grad(2) = + c33 * (-3.0*(0.0,1.0)*x(1)**2-6.0*x(1)*x(2)+3.0*(0.0,1.0)*x(2)**2)
      num_xlylm_grad(3) = 0.0
    end select
  case default
    call errore("num_xlylm_grad","No case",1)
  end select
end function num_xlylm_grad

function num_xlylm(x,l,m)
! Returns |x|^l * y_lm(x).
! With this definitions, the spherical harmonics are well
! defined also for null vectors, continuous and derivable everywhere
  complex(SP)             :: num_xlylm
  real(SP), intent(in)    :: x(3)
  integer, intent(in) :: l,m
  if(abs(m)>l) call errore("num_xlylm","m>l",1)
  select case(l)
  case(0)
    num_xlylm = c00
  case(1)
    select case(m)
    case(+1)
      num_xlylm = - c11 * (x(1) + (0.0,1.0)*x(2))
    case(0)
      num_xlylm = + c10 * x(3)
    case(-1)
      num_xlylm = + c11 * (x(1) - (0.0,1.0)*x(2))
    end select
  case(2)
    select case(m)
    case(+2)
      num_xlylm = + c22 * (x(1) + (0.0,1.0)*x(2))**2
    case(+1)
      num_xlylm = - c21 * (x(1) + (0.0,1.0)*x(2)) * x(3)
    case(0)
      num_xlylm = + c20 * (2.0*x(3)**2 - x(1)**2 - x(2)**2) * 0.5
    case(-1)
      num_xlylm = + c21 * (x(1) - (0.0,1.0)*x(2)) * x(3)
    case(-2)
      num_xlylm = + c22 * (x(1) - (0.0,1.0)*x(2))**2
    end select
  case(3)
    select case(m)
    case(+3)
      num_xlylm = - c33 * (x(1) + (0.0,1.0)*x(2))**3
    case(+2)
      num_xlylm = + c32 * (x(1) + (0.0,1.0)*x(2))**2*x(3)
    case(+1)
      num_xlylm = - c31 * (x(1) + (0.0,1.0)*x(2)) * (5.0*x(3)**2-(x(1)**2 + x(2)**2 + x(3)**2))
    case(0)
      num_xlylm = + c30 * (5.0*x(3)**3 - 3.0*x(3)*(x(1)**2 + x(2)**2 + x(3)**2)) * 0.5
    case(-1)
      num_xlylm = + c31 * (x(1) - (0.0,1.0)*x(2)) * (5.0*x(3)**2-(x(1)**2 + x(2)**2 + x(3)**2))
    case(-2)
      num_xlylm = + c32 * (x(1) - (0.0,1.0)*x(2))**2*x(3)
    case(-3)
      num_xlylm = + c33 * (x(1) - (0.0,1.0)*x(2))**3
    end select
  case default
    call errore("num_xlylm","No case",1)
  end select
end function num_xlylm

function num_ylm(x,l,m)
! spherical harmonics
! NOTE ill defined for x=0.0
  complex(SP)             :: num_ylm
  real(SP), intent(in)    :: x(3)
  integer, intent(in) :: l,m
  select case(l)
  case(0)
    num_ylm = num_xlylm(x,l,m)
  case(1)
    num_ylm = num_xlylm(x,l,m) / sqrt(sum(x**2))
  case(2)
    num_ylm = num_xlylm(x,l,m) / sum(x**2)
  case default
    call errore("num_ylm","No case",1)
  end select
end function num_ylm

function num_xmlsphbes(x,l)
  use numrec_kinds
! Returns x^(-l) * j_l(x)
  real(SP) :: num_xmlsphbes
  real(SP),    intent(in) :: x
  integer, intent(in) :: l
! Note: the following lines can be generated with GNU bc -m
! and the following input <<EOF
!  scale = 60
!  lmax  = 3
!  smax  = 15
!  define fact (i) {
!    auto p,j;
!    p = 1;
!    for (j=1; j<=i; j++) p = p*j
!    return (p) ;
!  }
!  define coeff (s,l)
!  {
!    auto num,den;
!    num = 2^l * (-1)^s * fact(s+l) ;
!    den = fact(s) * fact(2*s+2*l+1) ;
!    return (num/den) ;
!  }
!  print "  real, parameter :: coeff(0:",smax,",0:",lmax,")=reshape((/ &\n" ;
!  for ( l=0; l<=lmax; l++)
!  for ( s=0; s<=smax; s++) {
!    print coeff(s,l) ;
!    if( (s != smax) || (l != lmax)) print ", &\n" ;
!  }
!  print "/), (/",smax+1,",",lmax+1,"/))\n" ;
!  quit
!  EOF
  real(dbl), parameter :: coeff(0:15,0:3)=reshape((/ &
1.000000000000000000000000000000000000000000000000000000000000_dbl, &
-.166666666666666666666666666666666666666666666666666666666666_dbl, &
.008333333333333333333333333333333333333333333333333333333333_dbl, &
-.000198412698412698412698412698412698412698412698412698412698_dbl, &
.000002755731922398589065255731922398589065255731922398589065_dbl, &
-.000000025052108385441718775052108385441718775052108385441718_dbl, &
.000000000160590438368216145993923771701549479327257105034882_dbl, &
-.000000000000764716373181981647590113198578807044415510023975_dbl, &
.000000000000002811457254345520763198945583010320016233492735_dbl, &
-.000000000000000008220635246624329716955981236872280749220738_dbl, &
.000000000000000000019572941063391261230847574373505430355287_dbl, &
-.000000000000000000000038681701706306840377169119315228123231_dbl, &
.000000000000000000000000064469502843844733961948532192046872_dbl, &
-.000000000000000000000000000091836898637955461484257168364739_dbl, &
.000000000000000000000000000000113099628864477169315587645769_dbl, &
-.000000000000000000000000000000000121612504155351794962997468_dbl, &
.333333333333333333333333333333333333333333333333333333333333_dbl, &
-.033333333333333333333333333333333333333333333333333333333333_dbl, &
.001190476190476190476190476190476190476190476190476190476190_dbl, &
-.000022045855379188712522045855379188712522045855379188712522_dbl, &
.000000250521083854417187750521083854417187750521083854417187_dbl, &
-.000000001927085260418593751927085260418593751927085260418593_dbl, &
.000000000010706029224547743066261584780103298621817140335658_dbl, &
-.000000000000044983316069528332211183129328165120259735883763_dbl, &
.000000000000000147971434439237934905207662263701053485973301_dbl, &
-.000000000000000000391458821267825224616951487470108607105749_dbl, &
.000000000000000000000850997437538750488297720624935018711099_dbl, &
-.000000000000000000000001547268068252273615086764772609124929_dbl, &
.000000000000000000000000002387759364586841998590686377483217_dbl, &
-.000000000000000000000000000003166789608205360740836454081542_dbl, &
.000000000000000000000000000000003648375124660553848889924057_dbl, &
-.000000000000000000000000000000000003685227398647024089787802_dbl, &
.066666666666666666666666666666666666666666666666666666666666_dbl, &
-.004761904761904761904761904761904761904761904761904761904761_dbl, &
.000132275132275132275132275132275132275132275132275132275132_dbl, &
-.000002004168670835337502004168670835337502004168670835337502_dbl, &
.000000019270852604185937519270852604185937519270852604185937_dbl, &
-.000000000128472350694572916795139017361239583461805684027906_dbl, &
.000000000000629766424973396650956563810594311683636302372685_dbl, &
-.000000000000002367542951027806958483322596219216855775572829_dbl, &
.000000000000000007046258782820854043105126774461954927903490_dbl, &
-.000000000000000000017019948750775009765954412498700374221989_dbl, &
.000000000000000000000034039897501550019531908824997400748443_dbl, &
-.000000000000000000000000057306224750084207966176473059597219_dbl, &
.000000000000000000000000000082336529813339379261747806120110_dbl, &
-.000000000000000000000000000000102154503490495507768917873598_dbl, &
.000000000000000000000000000000000110556821959410722693634062_dbl, &
-.000000000000000000000000000000000000105292211389914973993937_dbl, &
.009523809523809523809523809523809523809523809523809523809523_dbl, &
-.000529100529100529100529100529100529100529100529100529100529_dbl, &
.000012025012025012025012025012025012025012025012025012025012_dbl, &
-.000000154166820833487500154166820833487500154166820833487500_dbl, &
.000000001284723506945729167951390173612395834618056840279062_dbl, &
-.000000000007557197099680759811478765727131740203635628472229_dbl, &
.000000000000033145601314389297418766516347069035980858019615_dbl, &
-.000000000000000112740140525133664689682028391391278846455849_dbl, &
.000000000000000000306359077513950175787179424976606735995803_dbl, &
-.000000000000000000000680797950031000390638176499948014968879_dbl, &
.000000000000000000000001260736944501852575255882407311138831_dbl, &
-.000000000000000000000000001976076715520145102281947346882662_dbl, &
.000000000000000000000000000002656017090752883201991864713551_dbl, &
-.000000000000000000000000000000003095591014863500235421753745_dbl, &
.000000000000000000000000000000000003158766341697449219818116_dbl, &
-.000000000000000000000000000000000000002845735442970674972809_dbl/), (/16,4/))

  integer :: is
  real(dbl) :: t
  if(l>3 .or. l<0) call errore("num_sphbes","Bad l value",1)
  if(abs(x) < .01) then
    t = coeff(0,l)
    do is=15,1,-1
      t = t + x**(2*is) * coeff(is,l)
    end do
    num_xmlsphbes = real(t,sgl)
  else
    select case(l)
    case(0)
      num_xmlsphbes = sin(x)/x
    case(1)
      num_xmlsphbes = (sin(x)-x*cos(x))/x**3
    case(2)
      num_xmlsphbes = ((3.0/x - x)*sin(x) - 3.0*cos(x)) / x**4
    case(3)
      num_xmlsphbes = ((15.0/x - 6.0*x)*sin(x) + (x**2-15.0)*cos(x))/x**6
    case default
      call errore("num_xmlsphbes","No case",1)
    end select
  end if
end function num_xmlsphbes

end module numerical_module

