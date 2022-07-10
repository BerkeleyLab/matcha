program matcha_app
  !! Motility Analysis of T-Cell Histories in Activation (Matcha)
  use matcha_m, only : matcha
  implicit none

  associate(history => matcha())
  end associate

  print *
  print *,"----> Matcha done. <----"

end program
