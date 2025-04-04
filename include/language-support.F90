! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef HAVE_MULTI_IMAGE_SUPPORT
  ! Define whether the compiler supports the statements and intrinsic procedures that support
  ! multi-image execution, e.g., this_image(), sync all, etc.
#if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__GFORTRAN__)
#define HAVE_MULTI_IMAGE_SUPPORT 1
#else
#define HAVE_MULTI_IMAGE_SUPPORT 0
#endif
#endif
