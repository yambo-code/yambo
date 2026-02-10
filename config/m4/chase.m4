
AC_DEFUN([AC_CHASE_SETUP],[

AC_ARG_ENABLE(chase,   AS_HELP_STRING([--enable-chase],[Use ChASE eigensolver. Default is no]))
AC_ARG_WITH(chase_libdir,AS_HELP_STRING([--with-chase-libdir=<path>],[Path to the ChASE lib directory],[32]))
AC_ARG_WITH(chase_includedir,AS_HELP_STRING([--with-chase-includedir=<path>],[Path to the ChASE include directory],[32]))

compile_chase="no"
internal_chase="no"
external_chase="yes"
AC_SUBST(compile_chase)
AC_SUBST(internal_chase)
AC_SUBST(external_chase)

if test "$enable_chase" = "yes"
then
	if test x"$with_chase_libdir" != "x"
	then
		echo "LIBDIR = ${with_chase_libdir}"
	else
		with_chase_libdir="${ChASEROOT}/build/lib64"
	fi
	
	if test x"$with_chase_includedir" != "x"
	then
		echo "INCLUDEDIR = ${with_chase_includedir}"
	else
		with_chase_includedir="${ChASEROOT}/build/include"
	fi

	AC_SUBST([CHASE_LIBS_R],["$with_chase_libdir"])
	AC_SUBST([CHASE_INCS_R],["$with_chase_includedir"])

        #if test "$have_chase" = no
        #then
        #        echo "Downloading ChASE..."
        #        git clone https://github.com/ChASE-library/ChASE.git
        #        cd ChASE
        #        ChASEROOT=$(pwd)
        #        mkdir build
        #        cd build
        #        cmake .. -DCMAKE_INSTALL_PREFIX=${ChASEROOT} #We assume CMake exists
        #        make install
        #        cd ../..
        #fi

        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_c.a"    ,[],[AC_ERROR([lchase_c not found   ])])
        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_f.a"    ,[],[AC_ERROR([lchase_f not found   ])])
        AC_CHECK_FILES("${CHASE_LIBS_R}/libchase_cuda_kernels.a" ,[],[AC_ERROR([lchase_cuda not found])])

        AC_SUBST([CHASE_INCS],["${CHASE_INCS} -I${CHASE_INCS_R}"])
        AC_SUBST([CHASE_LIBS],["${CHASE_LIBS} -L${CHASE_LIBS_R} -lchase_c -lchase_f -lchase_cuda_kernels "])
        AC_SUBST([CHASE_LIBS],["${CHASE_LIBS} -lstdc++ "])
	
        AC_SUBST([CHASE_LIBS],["${CHASE_LIBS}  "])
        #-lcudart -lcublas -lcusolver -lcudart -lcurand -lnccl
        #LIBS="${LIBS} ${CHASE_LIBS} "
        #INCS="${INCS} ${CHASE_INCS} "

	#AC_SUBST(LIBS)
	#AC_SUBST(INCS)

        #Assuming a CUDA compiler...
        #LIBS+="-lcudart -lcublas -lcusolver -lcudart -lcurand -lnccl -lstdc++ "

        def_chase="-D_CHASE"
else
        def_chase=""
fi

AC_SUBST(def_chase)

dnl Process Makefile.in to create Makefile

])
