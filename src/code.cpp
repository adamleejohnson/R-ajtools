#include <Rcpp.h>
using namespace Rcpp;

// // [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <progress_bar.hpp>

// [[Rcpp::export]]
RObject largest_na_block(const NumericMatrix M, SEXP max_perim = R_NilValue, int num_results = 1) {
   const int n = M.nrow();
   const int max_perim_int = Rf_isNull(max_perim) ? INFINITY : Rf_asInteger(max_perim);

   List results = List(num_results, R_NilValue);

   int num_big_iter = num_results * n * (n + 1) / 2;
   if (num_big_iter > 3000) REprintf("\nPopulating snp search table\n");
   Progress p(num_big_iter, num_big_iter > 3000);

   // populate M_score
   LogicalMatrix M_score = no_init_matrix(n, n);
#pragma omp parallel for collapse(2)
   for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j) {
         M_score(i, j) = R_IsNA(M(i, j));
      }
   }

   if (is_false(any(M_score))) return results;


   // Clean up M_score to remove small islands of 0's
   int min_island_size = std::floor(std::min(max_perim_int / 4, n / 10));
   for (int i = 0; i < n; ++i) {
      int cnt = 0;
      for (int j = 0; j < n; ++j) {
         int cnt_i = (cnt + 1) * !M_score(i, j);
         if (cnt_i == 0 && cnt <= min_island_size) {
            for (int k = j - cnt; k < j; ++k) M_score(i, k) = TRUE;
         }
         cnt = cnt_i;
      }
   }
   for (int j = 0; j < n; ++j) {
      int cnt = 0;
      for (int i = 0; i < n; ++i) {
         int cnt_i = (cnt + 1) * !M_score(i, j);
         if (cnt_i == 0 && cnt <= min_island_size) {
            for (int k = i - cnt; k < i; ++k) M_score(k, j) = TRUE;
         }
         cnt = cnt_i;
      }
   }

   IntegerMatrix M_score_col = no_init_matrix(n, n);

   // LOOP OVER NUMBER OF BLOCKS TO RETURN
   for (int res_i = 0; res_i < num_results; ++res_i) {

      // populate M_score_col
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j < n; ++j) {
            if (j == 0)
               M_score_col(i, j) = 1;
            else
               M_score_col(i, j) = M_score(i, j - 1) * M_score_col(i, j - 1) + 1;
         }
      }
#pragma omp parallel for collapse(2)
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j < n; ++j) {
            if (!M_score(i, j))
               M_score_col(i, j) = 0;
         }
      }

      // calculate dynamic min row heights
      IntegerMatrix M_score_row_mins = no_init_matrix(n, n);
#pragma omp parallel for
      for (int k = 0; k < n * (n + 1) / 2; ++k) {
         int i = int(std::floor((std::sqrt(8 * k + 1) - 1) / 2));
         int j = k - i * (i + 1) / 2;
         p.increment();

         const int row_score_start = j - M_score_col(i, j) + 1;
         if (row_score_start >= j) {
            M_score_row_mins(i, j) = M_score_col(j, i);
         }
         else {
            IntegerMatrix::const_iterator iter_start = M_score_col.cbegin() + n * i + row_score_start;
            IntegerMatrix::const_iterator iter_end = iter_start - row_score_start + j;
            int min_val = n;
            for (IntegerMatrix::const_iterator iter = iter_start; iter != iter_end; iter++) {
               if (*iter < min_val) min_val = *iter;
            }
            M_score_row_mins(i, j) = min_val;
         }
      }

      // multiply to get areas
      IntegerMatrix M_score_prod = no_init_matrix(n, n);
#pragma omp parallel for collapse(2)
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j <= i; ++j) {
            M_score_prod(i, j) = M_score_col(i, j) * M_score_row_mins(i, j);
            M_score_prod(j, i) = M_score_prod(i, j);
         }
      }

      // calculate unique perimeters
      IntegerMatrix M_score_perim = no_init_matrix(n, n);
#pragma omp parallel for collapse(2)
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j <= i; ++j) {
            M_score_perim(i, j) = std::min(M_score_row_mins(i, j), i - j) + M_score_col(i, j);
            // M_score_perim(j, i) = M_score_perim(i, j);
         }
      }

      // apply cutoff for max and min perimeters to max prod
#pragma omp parallel for collapse(2)
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j <= i; ++j) {
            if (M_score_perim(i, j) > max_perim_int) {
               M_score_prod(i, j) = 0;
               M_score_prod(j, i) = 0;
            }
         }
      }
      M_score_perim.~IntegerMatrix();

      // get max
      int max_row;
      int max_col;
      int max_score = 0;
      for (int i = 0; i < n; ++i) {
         for (int j = 0; j <= i; ++j) {
            int score = M_score_prod(i, j);
            if (score >= max_score) {
               max_score = score;
               max_row = i;
               max_col = j;
            }
         }
      }

      if (max_score == 0) break;

      int startRow = max_row - M_score_row_mins(max_row, max_col) + 1;
      int endRow = max_row;
      int startCol = max_col - M_score_col(max_row, max_col) + 1;
      int endCol = max_col;

      IntegerVector result_rows = Range(startRow, endRow) + 1;
      IntegerVector result_cols = Range(startCol, endCol) + 1;
      results[res_i] = List::create(Named("rows") = result_rows, _["cols"] = result_cols);

      // erase entries in the score matrix before recalculating for next iteration
      for (int i = startRow; i <= endRow; ++i) {
         for (int j = startCol; j <= endCol; ++j) {
            M_score(i, j) = 0;
            M_score(j, i) = 0;
         }
      }
   }

   return results;
}


// [[Rcpp::export]]
RObject sort_blocks_diagonally(NumericMatrix M) {
   if (Rf_isNull(M)) return(NULL);

   const int n = M.nrow();
   NumericMatrix M_sorted = no_init_matrix(n, n);
   NumericMatrix M_sorted_temp = no_init_matrix(n, n);
   CharacterVector names = colnames(M);
   CharacterVector names_sorted = no_init_vector(n);

   // sort by number of non - NA entries
   LogicalVector M_is_na_vec = !is_na(M);
   IntegerVector M_row_sums = rowSums(IntegerMatrix(n, n, M_is_na_vec.begin()));
   M_is_na_vec.~LogicalVector();
   IntegerVector sortOrder = no_init_vector(n);
   R_orderVector1(sortOrder.begin(), n, M_row_sums, TRUE, TRUE);

   for (int i = 0; i < n; ++i) {
      M_sorted_temp(_, i) = M(_, sortOrder[i]);
      names_sorted[i] = names[sortOrder[i]];
   }
   for (int i = 0; i < n; ++i) {
      M_sorted(i, _) = M_sorted_temp(sortOrder[i], _);
      names[i] = names_sorted[i];
   }
   M = M_sorted;

   // sort by position of non - NA entry
   IntegerVector idx_of_first_entry = no_init_vector(n);
   for (int i = 0; i < n; ++i) {
      int min_idx = 0;
      auto col_i = M.column(i);
      while (min_idx < n && R_IsNA(col_i[min_idx])) ++min_idx;
      if (min_idx == n) min_idx = R_NaInt;
      idx_of_first_entry[i] = min_idx;
   }
   R_orderVector1(sortOrder.begin(), n, idx_of_first_entry, TRUE, FALSE);

   for (int i = 0; i < n; ++i) {
      M_sorted_temp(_, i) = M(_, sortOrder[i]);
      names_sorted[i] = names[sortOrder[i]];
   }
   for (int i = 0; i < n; ++i) {
      M_sorted(i, _) = M_sorted_temp(sortOrder[i], _);
   }

   colnames(M_sorted) = names_sorted;
   rownames(M_sorted) = names_sorted;
   return M_sorted;
}
