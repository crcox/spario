#' Serialize and deserialize a sparse matrix
#'
#' This returns a list of raw vectors that can be passed to
#' `write_sparse_matrix()` to generate a file that can be read back into R using
#' `read_sparse_matrix`. The serialized list of raw vectors can also be passed
#' to `deserialize_sparse_matrix()` to recover the original matrix.
#'
#' @param x Sparse matrix of class CsparseMatrix or RsparseMatrix
#' @param int_size Number of bytes required to represent each integer in the
#'   sparse matrix representation (i.e., row/col indices and "pointers").
#' @param value_size Number of bytes required to represent each value stored in
#'   the matrix. If the value dtype is logical, this defaults to 1 byte; if
#'   integer, this defaults to 8 bytes; and if numeric, this defaults to 16
#'   bytes. Rather than a number of bytes, the strings "single" and "double" may
#'   be provided, which correspond to 8 and 16 bytes.
#' @returns A list of raw vectors
#'
#' @details
#' The serialization begins with a 30-byte header that consists of:
#'
#' 1. Number of bytes per integer in the sparse matrix representation (not within
#' the header, wherein all values are of fixed size). (2 bytes)
#' 2. Number of bytes per value in the sparse matrix. (2 bytes)
#' 3. Number of rows (8 bytes)
#' 4. Number of columns (8 bytes)
#' 5. Number of non-zero values in the matrix (8 bytes)
#' 6. The sparse matrix representation (0: triplets; 1: compressed row; 2:
#' compressed column) (1 byte)
#' 7. The data type of the values stored in the matrix (0: logical; 1: integer; 2:
#' float) (1 byte)
#'
#' The serialization continues with the body. In the following description,
#' `nnzeros` stands for the number of non-zero values stored in the sparse
#' matrix, `nrows` and `ncols` are the number of rows and columns, and `dtype`
#' refers to whatever data type was actually stored, which should be encoded in
#' the header. The body differs based on the sparse matrix representation:
#'
#' If TMatrix:
#'
#' 8. Row indices (integers; nnzeros)
#' 9. Column indices (integers; nnzeros)
#' 10. Matrix data values (dtype; nnzeros)
#'
#' If RMatrix (see \code{?RsparseMatrix}):
#'
#' 8. Col indices (integers; nnzeros)
#' 9. Pointers to first element in each row (integers; nrows)
#' 10. Matrix data values (dtype; nnzeros)
#'
#' If CMatrix (see \code{?CsparseMatrix}):
#'
#' 8. Row indices (integers; nnzeros)
#' 9. Pointers to first element in each column (integers; ncols)
#' 10. Matrix data values (dtype; nnzeros)
#'
#' @export
serialize_sparse_matrix <- function(x, int_size = NULL, value_size = NULL) {
  nr <- nrow(x)
  nc <- ncol(x)
  nnz <- Matrix::nnzero(x)
  repr <- encode_sparse_matrix_representation(x)
  bit_sizes <- cumprod(rep(2,6))[-1]
  if (is.null(int_size)) {
    int_size <- bit_sizes[which.min(log2(max(c(nr, nc, nnz))) < bit_sizes)]
  }
  value_size <- if (is.null(value_size)) {
    default_value_size(x)
  } else if (is.character(value_size)) {
    encode_value_size_strings(value_size)
  }
  value_dtype <- encode_value_dtype(x)
  stopifnot(int_size %in% bit_sizes)
  stopifnot(value_size %in% bit_sizes)
  header_sizes <- default_header_sizes()
  header <- list(
    int_size = int_size,
    value_size = value_size,
    nrow = nr,
    ncol = nc,
    nnzero = nnz,
    spmat_repr = repr,
    value_dtype = value_dtype
  ) |> lapply(as.integer)
  body_sizes <- switch(class(x),
    dgTMatrix = list(i = int_size, j = int_size, x = value_size),
    dgRMatrix = list(j = int_size, p = int_size, x = value_size),
    dgCMatrix = list(i = int_size, p = int_size, x = value_size)
  )
  body <- switch(class(x),
    dgTMatrix = list(i = as.integer(x@i), j = as.integer(x@j), x = x@x),
    dgRMatrix = list(j = as.integer(x@j), p = as.integer(x@p), x = x@x),
    dgCMatrix = list(i = as.integer(x@i), p = as.integer(x@p), x = x@x)
  )
  list(
    header = mapply(
      writeBin,
      header,
      header_sizes,
      MoreArgs = list(con = raw(), endian = "little"),
      SIMPLIFY = FALSE
    ),
    body = mapply(
      writeBin,
      body,
      body_sizes,
      MoreArgs = list(con = raw(), endian = "little"),
      SIMPLIFY = FALSE
    )
  )
}

#' @rdname serialize_sparse_matrix
#' @export
deserialize_sparse_matrix <- function(x, value_dtype = NULL) {
  header_sizes <- c(
    int_size    = 2,
    value_size  = 2,
    nrow        = 8,
    ncol        = 8,
    nnzero      = 8,
    spmat_repr  = 1,
    value_dtype = 1
  )
  header <- mapply(readBin, x$header, header_sizes, MoreArgs = list(what = "integer", n = 1L))
  spmat_repr <- c("T", "R", "C")[header["spmat_repr"] + 1]
  if (header["value_dtype"] < 0 && is.null(value_dtype)) {
    stop("value_dtype is unknown.")
  }
  value_dtype <- c("logical", "integer", "numeric")[header["value_dtype"] + 1]
  body_sizes <- switch(spmat_repr,
    "T" = list(i = header["int_size"], j = header["int_size"], x = header["value_size"]),
    "R" = list(j = header["int_size"], p = header["int_size"], x = header["value_size"]),
    "C" = list(i = header["int_size"], p = header["int_size"], x = header["value_size"])
  )
  nr  <- header["nrow"]
  nc  <- header["ncol"]
  nnz <- header["nnzero"]
  body <- setNames(list(
    readBin(x$body[[1]], what = "integer", n = nnz, size = body_sizes[[1]], endian = "little"),
    readBin(x$body[[2]], what = "integer", n = switch(spmat_repr, T = nnz, R = nr + 1, C = nc + 1), size = body_sizes[[2]], endian = "little"),
    readBin(x$body[[3]], what = value_dtype, n = nnz, size = body_sizes[[3]], endian = "little")
  ), names(body_sizes))
  list(header = header, body = body)
}
