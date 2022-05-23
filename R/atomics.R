# A bunch of functions to use the epsilon datbase outputs.

#' Cmy Print Method
#'
#' @param ... as many strings as one wants
#' @return NULL
print_timed <- function(...){
  print(paste0(Sys.time()," - ", ...))
}
