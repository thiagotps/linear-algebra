#include <Eigen/Sparse>
#include <Eigen/SparseCore>

using namespace Eigen;

using RowSparseMatrix = SparseMatrix<double, RowMajor>;
using VectorDouble = Eigen::VectorXd;
using VectorComplex = Eigen::VectorXcd;

extern "C" {

RowSparseMatrix *new_sparse_matrix(int nrow, int ncol, const Triplet<double> *t, int n) {
  RowSparseMatrix *s = new RowSparseMatrix(nrow, ncol);
  if (s != nullptr)
    s->setFromTriplets(t, t + n);

  return s;
}

int non_zeros(const RowSparseMatrix *m) { return m->nonZeros(); }

int sparse_to_list(const RowSparseMatrix *m, Triplet<double> *t, int s) {
  int n = 0;
  for (int k = 0; k < m->outerSize(); ++k) {
    for (typename RowSparseMatrix::InnerIterator i(*m, k); i; ++i) {
      if (n >= s)
        return -1;
      t[n++] = Triplet<double>(i.row(), i.col(), i.value());
    }
  }
  return 0;
};

void free_sparse_matrix(RowSparseMatrix *s) { delete s; }

double dot(const VectorDouble *a, const VectorDouble *b) { return a->dot(*b); }

void mul(const RowSparseMatrix *m, const VectorDouble *b, VectorDouble *dest) {
  *dest = (*m) * (*b);
}

void add(const VectorDouble *a, const VectorDouble *b, VectorDouble *dest) {
  *dest = (*a) + (*b);
}

VectorDouble *new_vector(int n, const double *elems) {
  auto v = new VectorDouble(n);
  if (elems != nullptr)
    for (int i = 0; i < n; i++)
      (*v)[i] = elems[i];

  return v;
}

void free_vector(VectorDouble *v) { delete v; }

int vector_size(VectorDouble *v) { return v->size(); }

void vector_to_list(VectorDouble *v, double *dest) {
  int n = v->size();
  for (int i = 0; i < n; i++)
    dest[i] = (*v)[i];
}



}
