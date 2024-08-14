% This matrix is Csparse, so `i` contains the row index of each value and `p` contains "pointers" to the initial
% (zero-based) index of elements in the column. See `?CsparseMatrix` in R for details.
file = fopen('../data/raw.spario', 'rb');
x = read_sparse_matrix(file);
fclose(file);

file = fopen('../data/raw_matlab.spario', 'wb');
write_sparse_matrix(x, file);
fclose(file);

