my_shared_function <- function(x, y){
  description_of_x <- paste('x has', length(x), 'elements')
  description_of_y <- paste('y has', length(y), 'elements')
  return(paste(description_of_x, 'and', description_of_y))
}

my_shared_function(2, 3)
