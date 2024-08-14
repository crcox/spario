
default_value_size <- function(x) {
  if (is.logical(x@x)) {
    1
  } else if (is.integer(x@x)) {
    4
  } else if (is.numeric(x@x)) {
    8
  }
}

default_header_sizes <- function(x) {
  c(
    int_size    = 1,
    value_size  = 1,
    nrow        = 4,
    ncol        = 4,
    nnzero      = 4,
    spmat_repr  = 1,
    value_dtype = 1
  )
}
