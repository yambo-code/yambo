c

      subroutine write_wavef_n_cell(                                       &
     &     ecut, xk, wkpt, igk, n1, n2, n3, nr1, nr2, nr3,                 &  ! basis
     &     nel, ngw, c0, eig, focc, zeta, efermi,                          &  ! electrons
     &     pgind, ibrav, alat, omega,                                      &  ! lattice
     &     nsp, na, atom, tau0,                                            &  ! atoms 
     &     ng,g,gg)                                                        &  ! reciprocal lattice

c
c     Subroutine
c     Name       :   write_wavef_n_cell 
c     Function   :   output electronic and ionic information
c                    necessary for continuing this calculation
c                    (i.e. electronic wavefunctions, basis set, atomic positions,
c                    lattice type, ...)
c     Corresponding Input Routine :
c                    read_wavef_n_cell
c
c


      implicit none

      include 'parameter.h'
c     variables nsx, nax, nx, ngwx, ngx, nnrx, nkptx, nspin, nspin_pt


c local toggles

      logical     tspin                                                    ! = spin polarization ?
      logical     t_double                                                 ! whether c0 is cmpl8/16
      parameter  (t_double=.false.)
      logical     t_orig_prec                                              ! whether c0 should be
      parameter  (t_orig_prec=.true.)                                      ! reduced c16->c8


c parameters                                                                  ! data to be written

      integer   n1(ngx), n2(ngx), n3(ngx), igk(ngwx,nkptx)
      integer   nr1, nr2, nr3
      integer   ngw(nkptx)
      integer   pgind, ibrav
      integer   nsp, na(nsx)
      integer   ng
      real*4    g(ngx)
      real*4    gg(ngx+1,3)
      real*8    ecut, efermi
      real*8    xk(3,nkptx), wkpt(nkptx)
      real*8    nel
      real*8    eig((nspin_pt+1)*nx,nkptx), focc((nspin_pt+1)*nx,nkptx)
      real*8    zeta(nr1x+1,nr2x,nr3x)
      real*8    alat, omega
      real*8    tau0(3,nax,nsx)
      complex*8 c0(ngwx,(nspin+1)*nx,nkptx)
      character*10 atom(nsx)

c local variables

      character*1 cspin                                                       ! kind of spin polarization
      complex*8   c0_single(ngwx)                                             ! tramp variable 
      integer     ik, ig, ii, is, ia, i1, i2, i3                              ! indices



c     determine type of polarisation
      if ( nspin_pt.eq.1 ) then
        tspin=.true.
      else
        tspin=.false.
      end if

      if      (nspin.eq.0 .and.nspin_pt.eq.0) then
        cspin='n'                                                             ! no   spin polarization
      else if (nspin.eq.0 .and.nspin_pt.eq.1) then
        cspin='p'                                                             ! partial -"-
      else if (nspin.eq.1 .and.nspin_pt.eq.1) then
        cspin='s'                                                             ! full    -"-
      else
        write (6,*) 'nspin = ', nspin, ' , nspin_pt = ', nspin_pt             ! fatal error
        call error("write_wavef_n_cell",
     &       "Invalid combination of nspin / nspin_pt",0)
      end if




c   start writing base set, wave functions, lattice and ions

      open(unit=71,form='unformatted')
      rewind(71)


c   toggles

c     type of polarisation, wave function precision
      write(71) cspin, t_double


c   electronic base set

c     maximum basis size (=numb. plane waves), energy cutoff, number of k-points
      write(71) ngwx, ecut, nkptx
c     k-point positions/weights, index of G's used at k, those G's position in reciprocal space
      write(71) xk,wkpt,igk, n1,n2,n3

c tmp
c      write (6,*) 'n1   n2   n3'
c      do ii=1,ngx
c        write (6,*) n1(ii), n2(ii), n3(ii)
c      enddo
c tmp
c tmp
c      write (6,*) 'igk'
c      do ik=1,nkptx
c        do ig=1,ngwx
c          write(6,*) ik, ig, igk(ig,ik)
c        enddo
c      enddo
c tmp

c   position and reciprocal space (identical sizes)

c     size of reciprocal lattice (3d), (actual ?) number of reciproc. latt. points
      write(71) nr1,nr2,nr3, nnrx


c   electrons

c     number of electrons, number of electronic wavefunctions (= max.nr for historical reasons)
      write(71) nel, nx
c     per k-point :
      do ik=1,nkptx
c       number of electronic states
        write(71) ngw(ik)
c       wave functions
        if (t_orig_prec) then
          do ii=1,nx*(nspin+1)
            write(71) (c0(ig,ii,ik),ig=1,ngwx)
          enddo
        else
          do ii=1,nx*(nspin+1)
            do ig=1,ngwx
c             casting from double to single precision
              c0_single(ig) = c0(ig,ii,ik)
            enddo
c           writing out in reduced precision
            write(71) (c0_single(ig), ig=1,ngwx)
          enddo
        endif
c       energy eigen values in Hartree
        write(71) (eig(ii,ik), ii=1,nx)
        if (tspin) then
          write(71) (eig(ii,ik), ii=nx+1,2*nx)
        endif
      enddo
c     occupation numbers + (if so) spin polarization density
      do ik=1,nkptx
c        do ii=1,nx
          write(71) (focc(ii,ik), ii=1,nx)
c        enddo
      enddo

      if (tspin) then
        do ik=1,nkptx
c          do ii=nx+1,2*nx
            write(71) (focc(ii,ik), ii=nx+1,2*nx)
c          enddo
        enddo

c        do i1=1,nr1x+1
c          do i2=1,nr2x
            do i3=1,nr3x
              write(71) ((zeta(i1, i2, i3), i1=1,nr1x+1), i2=1,nr2x)
            enddo
c          enddo
c        enddo

      endif

c     fermi energy
      write(71) efermi


c   lattice / cell

c     symmetry search, lattice type
      write(71) pgind,ibrav
c     lattice constant, cell volume, cell dimensions
      write(71) alat,omega


c   ions

c     maximum number species/atoms per species
      write(71) nsx,nax
c     act. number of species, act. number of atoms and label per species
      write(71) nsp
      do is=1,nsp
        write(71) na(is),atom(is)
      enddo
c     atomic positions (3d)
      do is=1,nsp
        do ia=1,na(is)
          write(71) (tau0(ii,ia,is),ii=1,3)
        enddo
      enddo

c      write(6,*) '(output of wavefunction to fort.71 completed )
c   '

c   Done
      close(71)

      open(71,file='r-lattice',form='unformatted',status='unknown')
      write(71) ngwx,ng
      write(71) nr1,nr2,nr3
      write(71) ngw
      write(71) n1,n2,n3
      do ig=1,ngx
       write(71) g(ig),(gg(ig,ii),ii=1,3)
      enddo
      close(71) 
      return
      end



c Old comments:

c output of wave-function and all the other stuff to unit 71
c
c t_single: if true then use single precision for wave c0
c
c Revised version for spin-polarized systems. 
c Aug. 18, 1997. 
c
c Adapted to no, partial and full spin polarisation 
c June 8, 1998.
c
