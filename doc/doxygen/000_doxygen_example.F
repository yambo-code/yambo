!
!        Copyright (C) 2000-2019 the YAMBO team
!              http://www.yambo-code.org
!
! Authors (see AUTHORS file for details): AM
!
!> @ch   (Name added through macro expansion)
!> @author Mr. Ed
!> - 10/01/2015 Added bug
!> - 11/01/2015 Fixed bug
!> @brief Wrappers for QEXML calls, including unit conversions
!>
!> @details 
!! ## Testing out markdown: a large header ##
!! Some text
!! ### A sub header ###
!! Some more text
!>
!> A blank line is often necessary to revert to normal text or a newline.
!>
!> Here is a test of some LaTeX:
!! \f$ \sum_{i=0}^\infty f(i) = [\log(i)]^{\sin(i)} \f$
!!
!> Here is some *emphasised* text on a new line.
!! This is a continuation so it appears on the same line.
!>
!> Here is a fenced code block
!! ~~~~~~~~~~~~~~~~~~~~~
!! a one-line code block
!! ~~~~~~~~~~~~~~~~~~~~~
!!
!! A citation: @cite Marini2009 
!! ... and that is the end of the details section.
!!
!> @note This is a note
!> @warning This is a warning
!> @bug This warns of a bug. It gets collected with the rest.
!> @todo This is a todo list. It gets collected with the rest.
!> @see See other examples of markdown language 
!! at [this inline link](http://www.stack.nl/~dimitri/doxygen/manual/markdown.html)
!>
!> -----
!> Finally, a ruler should appear above!
!>
module doxymod
 !
#if defined _P2Y_EXPORT
 use pw_export   !< A comment here doesn't work
#else 
 use qexml_module
#endif
 use pw_data
 use pars,                  ONLY : lchlen,SP, DP
 use electrons,             ONLY : levels
 use R_lattice,             ONLY : bz_samp
 use mod_com2y,             ONLY : verboseIO
 ! 
 implicit none
 !
 character(lchlen) :: index_filename  !< a comment here does
 !
 integer, public  :: pw_unit 
 integer, private :: i1,i2,ierr
 !
 ! Memory saving tools
 !
 integer, parameter       :: max_blocksize = 9         !< Maximum number of wfc blocks per k-point
 integer                  :: blocksize(max_blocksize)  
 !
contains
 !
 !---------------------------------------------------------------------*
 !    Select and open XML files                                        *
 !---------------------------------------------------------------------*
 !
 !> @brief Identify input XML filename and open it
 !> @param instr I forget what this is
 !> @param inf String containing the _optional_ filename
 subroutine pw_init(instr,inf)
   !
   use pars,  ONLY:lchlen
   use com,   ONLY:msg
   character(*)  :: instr,inf
   !
   ! Work Space
   !
   integer           :: rhounit_ 
   character(lchlen) :: lch
   logical           :: lexist
   !
   ! index filenames
   !
#if defined _P2Y_EXPORT

   index_filename = 'index.xml'

#elif defined _P2Y_V31

   index_filename = 'data-file.xml'
   pwversion = 31 
   write(lch,'(a,i3,a)') '== PWscf v.',pwversion,' generated data =='

#elif defined _P2Y_V311

   index_filename = 'data-file.xml'
   pwversion = 311
   write(lch,'(a,i3,a)') '== PWscf v.',pwversion,' generated data =='

#elif defined _P2Y_V32

   index_filename = 'data-file.xml'
   write(lch,'(a,i3,a)') '== PWscf v.3.2 generated data =='

#elif defined _P2Y_V40 

   index_filename = 'data-file.xml'
   write(lch,'(a,i3,a)') '== PWscf v.4.x generated data =='

#elif defined _P2Y_V50 

   index_filename = 'data-file.xml'
   write(lch,'(a,i3,a)') '== PWscf v.5.0 generated data =='

#endif

   if (trim(inf).ne."p2y.in") index_filename = inf

   call msg('s','Index file set to ',trim(index_filename))
   inquire(file=trim(index_filename),exist=lexist)
   if(.not.lexist) then
     call msg('s','Index file not found! Check p2y version...')
     stop ' '
   endif
   !
   ! Open XML index files and units
   !
   pw_unit = 10
#if defined _P2Y_EXPORT
    
   call msg('s','== pw_export generated data ==')
   call pw_openindex(pw_unit,index_filename) ! open index.xml file
    
#elif defined _P2Y_V31  || defined _P2Y_V311

   call msg('s',trim(lch))
   rhounit_ = 12
   call qexml_init(pw_unit,rhounit_,.true.) ! sets unit numbers
   call qexml_openfile( index_filename, "read", .false., ierr)

#elif defined _P2Y_V32

   rhounit_ = 12
   call qexml_init(pw_unit,"./", rhounit_,.true.) ! sets unit numbers
   call qexml_openfile( index_filename, "read", .false., ierr)
    
#elif defined _P2Y_V40 || defined _P2Y_V50

   rhounit_ = 12
   call qexml_init(pw_unit) 
   call qexml_openfile( index_filename, "read", .false., ierr)
    
#endif
   !
   !  qexml_init(_,_,.true.)               : rho file is binary
   !  qexml_openfile(_,"read"/"write",_,_) : read only from existing files
   !  qexml_openfile(_,_,.false.,_)        : data-file.xml is not binary
   !
 end subroutine pw_init
 !
 !---------------------------------------------------------------------*
 !    Close the XML files                                              *
 !---------------------------------------------------------------------*
 !
 !> @brief Close the main XML file
 subroutine pw_close
   !
#if defined _P2Y_EXPORT

   call pw_closeindex(pw_unit) ! close index.xml file

#elif defined _P2Y_V31 || defined _P2Y_V32 || defined _P2Y_V311 || defined _P2Y_V40 || defined _P2Y_V50

   call qexml_closefile("read",ierr) ! close index.xml file

#endif
   return
 end subroutine pw_close

 !
 !---------------------------------------------------------------------*
 !    Read dimensions                                                  *
 !---------------------------------------------------------------------*
 !
 !> @brief Read dimensions of main arrays
 subroutine get_dimensions(en,k)
   !
   use electrons,             ONLY : default_nel, n_spin, n_sp_pol, &
&                                    n_spinor, n_spin_den, l_spin_orbit
   use R_lattice,             ONLY : ng_vec
   use D_lattice,             ONLY : input_GS_Tel, n_atomic_species
   use wave_func,             ONLY : wf_ncx 
   use com,                   ONLY : msg, error
   use LIVE_t,                ONLY : live_timing_is_on
   use units,                 ONLY : HA2KEL
   type(levels),     intent(out)  :: en     ! Energies
   type(bz_samp),    intent(out)  :: k      ! K/Q points
   !
   ! Call the version dependent routines
   !
   gamma_only_ = .false.
#if defined _P2Y_EXPORT

   call pw_dimensions(pw_unit)
    
#elif defined _P2Y_V31 || defined _P2Y_V311

   call qexml_read_bands(nbnd=nbnd_, num_k_points=num_k_points_, &
&                        nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)

#elif defined _P2Y_V32

   call qexml_read_bands_info(nbnd=nbnd_, num_k_points=num_k_points_, &
&                        nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)

#elif defined _P2Y_V40 || defined _P2Y_V50

   call qexml_read_bands_info(nbnd=nbnd_, num_k_points=num_k_points_, &
&                        nspin=n_spin_pw_, nelec=nelec_, ierr=ierr)

#endif

   if (ierr.ne.0) then
     live_timing_is_on=.FALSE.
     call msg('ms','Error reading data: most likely you are using an incompatible')
     call msg('s','version of p2y with your data.')
     call msg('s','Action: Compile a compatible version of p2y.')
     call errore('qexml_read_bands.','IOTK error',ABS(ierr)) 
   endif

#if defined _P2Y_V31 || defined _P2Y_V32 || defined _P2Y_V311 || defined _P2Y_V40 || defined _P2Y_V50

   call qexml_read_symmetry(nsym=nsym_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_symmetry','IOTK error',ABS(ierr))
   call qexml_read_spin(lspinorb=l_spin_orbit, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_spin','IOTK error',ABS(ierr))
   call qexml_read_planewaves(gamma_only=gamma_only_, npwx=npwx_,ngm=ngm_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_planewaves','IOTK error',ABS(ierr))
   call qexml_read_ions(nat=nat_, nsp=nsp_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_ions','IOTK error',ABS(ierr))
   !
   default_nel = nelec_
   n_atomic_species = nsp_     

#endif

   k%nibz      = num_k_points_
   en%nb       = nbnd_
   !
   ! YAMBO presently does not make use of GAMMA_ONLY option, hence
   ! the wfc's and G's must be doubled in this case.
   ! Note: the quantities in the PW files are still dimensioned ngm_ and npwx_
   !
   if(gamma_only_) then
     ng_vec      = 2*(ngm_ -1) + 1
     wf_ncx      = 2*(npwx_-1) + 1
   else
     ng_vec      = ngm_
     wf_ncx      = npwx_ 
   endif
   !
   ! Set miscellanous YAMBO data: dimensions
   !
   n_spin_den = n_spin_pw_
   select case(n_spin_pw_)
   case(1)
     n_sp_pol  = 1
     n_spinor  = 1
     n_spin    = 1
   case(2)
     n_sp_pol  = 2
     n_spinor  = 1
     n_spin    = 2
   case(4)
     n_sp_pol  = 1
     n_spinor  = 2
     n_spin    = 2
   end select
   !
   input_GS_Tel = 0.d0
   !
#if defined _P2Y_V40 || defined _P2Y_V50
   !
   call qexml_read_occ(lgauss=lgauss_,ngauss=ngauss_,degauss=degauss_,&
&                               degauss_units=degauss_units_, ierr=ierr)
   if (ierr.ne.0) call errore('qexml_read_occ','IOTK error',ABS(ierr))  
   !
   if (lgauss_) input_GS_Tel=300._SP/HA2KEL
   !
#endif
   !
   return
 end subroutine get_dimensions
 !
end module doxymod
