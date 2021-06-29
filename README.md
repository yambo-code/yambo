## Yambo
This is the distribution of the Yambo code.
Yambo doesn't stand for anything like "Yet Another Many-Body cOde", for instance.  Unless you really want it to. 

## Installation
Quick installation instructions for the impatient:
`./configure [options]`
` make all`
"make" alone prints a list of acceptable targets. Binaries go in bin/.

## Want to know more?
For more information, see the specific documentation on the [educational web-site](http://www.yambo-code.org/wiki/) and general informations about the code on the
[main web-site](http://www.yambo-code.org/)

* [Getting started](http://www.yambo-code.org/wiki/index.php?title=Tutorials)
* [Download](http://www.yambo-code.org/wiki/index.php?title=Download)
* [Install](http://www.yambo-code.org/wiki/index.php?title=Installation)

## License
All the material included in this distribution is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

These programs are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU General Public License  for more details (see COPYING file).

## Known Issues
###### NetCDF/HDF5 in Ubuntu 20.4
Internal NetCDF and HDF5 libraries in LinuxMint 20.1/Ubuntu 20.4 have problems with Yambo please use internal Yambo libraries

###### gfortran 4.4 and earlier versions 
Unfortunatelly gfortran 4.4 and earlier version do not support the construct allocate(x, source=y) and 
allocate(x, mold=y), please update to a newer version in order to compile Yambo

###### Quantum-Espresso at GAMMA point
In Quantum-Espresso if you perform a SCF calculation using the option "KPOINTS gamma" you should use the same option for the NSCF otherwise Yambo gets confused
with the g-vectors If you need more k-points in the NSCF just re-run the SCF with "KPOINTS automatic /1 1 1 0 0 0"  and then run NSCF with a finite k-grid

###### Internal MAC-OSX libraries
At present it is no possible to compile Yambo with internal macos libraries. Indeed, no include/system/netcdf.mod file is present in the system.  The problem is
due to  Autoconf setting which automatically searches for versions  of the needed libraries in your system despite the specific options given to configure.
Then, it would compile the internal hdf5, then find an external netcdf,  then try to compile netcdf-fortran creating various conflicts. 

###### gfortran compiler on MAC-OSX
There are different problems compiling Yambo wiht "gfortran"  variant of the compiler/libraries with MacPorts,  a possible solution it is to install the "gcc8"
variant of these libraries and compilters More details on the [Yambo Forum](http://www.yambo-code.org/forum/viewtopic.php?t=1767)

###### Input file generation on MacOSX
There are two issues on input file generation:
* when you generate an input file on MacOSX Yambo is not able to read values already present in the input file, and reset them to the default value; 
* if yambo and ypp have the same option to generate an input file the code get confused. Please use long input strings, for example "yambo -optics" instead of "yambo -o".
