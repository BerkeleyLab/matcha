  use input_m, only : input_t
  use matcha_m, only : matcha
  implicit none
  associate(history => matcha(input_t()))
  end associate
end
