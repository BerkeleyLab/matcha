! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

module gridr_m
 
  use subdomain_m, only : subdomain_t
  implicit none
  
  private
  public :: gridr_t
  
  type gridr_t
    private
  contains
    procedure :: gridparameters
    procedure :: gradient
  end type  

  interface gridr_t
  
   pure module function construct() result(gridr)
     implicit none
     type(gridr_t) gridr
   end function
    
  end interface
  
  interface

    pure module function gridparameters(self,gb,ge,ng,ndim) result(gridp)
     implicit none
     class(gridr_t), intent(in) :: self
     integer, intent(in) :: ndim
     integer, intent(in) :: ng(:)
     double precision, intent(in) :: gb,ge
     double precision gridp(2*ndim+1)
    end function gridparameters     
  
    module function gradient(self, my_num_cells, ng, dx, gb, tconc, x, concentration_subgrid) result(gx)      
      implicit none
      class(gridr_t), intent(in) :: self
      integer, intent(in) :: my_num_cells
      integer, intent(in) :: ng(:)
      double precision, intent(in) :: dx(:)
      double precision, intent(in) :: gb,tconc
      double precision, intent(in) :: x(:,:)
      double precision, allocatable :: gx(:,:)
      type(subdomain_t) concentration_subgrid
    end function gradient

  end interface

end module gridr_m
