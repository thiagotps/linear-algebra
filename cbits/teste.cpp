#include <Eigen/Sparse>
#include <Eigen/SparseCore>
#include <Spectra/GenEigsSolver.h>
#include <Spectra/GenEigsComplexShiftSolver.h>
#include <Spectra/MatOp/SparseGenMatProd.h>

using namespace Eigen;
using namespace Spectra;
using namespace std;

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

  int sparse_rows(const RowSparseMatrix * m) {return m->rows();}
  int sparse_cols(const RowSparseMatrix * m) {return m->cols();}

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

double get_vector_elem(VectorDouble *v, int idx) {return (*v)[idx];}

double dot(const VectorDouble *a, const VectorDouble *b) { return a->dot(*b); }

VectorDouble * mul(const RowSparseMatrix *m, const VectorDouble *b) {
  return new VectorDouble{(*m) * (*b)};
}

VectorDouble * add(const VectorDouble *a, const VectorDouble *b, VectorDouble *dest) {
  return new VectorDouble{(*a) + (*b)};
}

  VectorDouble * mul_vector_by_scalar (double s, const VectorDouble * a) {
    return new VectorDouble{s*(*a)};
  }



int largest_eigen_value(const RowSparseMatrix *m, int ncv, int iterations, double precision, double * dest) {
  SparseGenMatProd<double, RowMajor> op(*m);
  static const int nev = 1;
  GenEigsSolver<SparseGenMatProd<double, RowMajor>> eigs(op, nev, ncv);

  eigs.init();
  eigs.compute(SortRule::LargestMagn, iterations, precision);

  auto info = eigs.info();
  if (info == CompInfo::Successful) {
    auto v = eigs.eigenvalues();
    *dest = abs(v[0]);
    for (int i = 0; i < v.size(); i++)
      *dest = max(*dest, abs(v[i]));
  }

  return (int)info;
}

}
