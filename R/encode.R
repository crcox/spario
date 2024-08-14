encode_value_dtype <- function(x) {
  if (is.logical(x@x)) {
    0
  } else if (is.integer(x@x)) {
    1
  } else if (is.numeric(x@x)) {
    2
  } else {
    -1
  }
}

decode_value_dtype <- function(value_dtype_ind) {
  c("logical", "integer", "numeric")[value_dtype_ind + 1]
}


encode_value_size_strings <- function(x, sizes = NULL) {
  m <- .Machine
  m$sizeof.single <- 4
  m$sizeof.double <- 8
  w <- intersect(names(m), names(sizes))
  for (i in seq_along(w)) {
    m[w[i]] <- sizes[w[i]]
  }
  switch(x,
    long = m$sizeof.long,
    single = m$sizeof.single,
    double = m$sizeof.double,
    "long long" = m$sizeof.longlong,
    "long double" = m$sizeof.longdouble
  )
}

encode_sparse_matrix_representation <- function(x) {
  switch(class(x),
    dgTMatrix = 0,
    dgRMatrix = 1,
    dgCMatrix = 2
  )
}
