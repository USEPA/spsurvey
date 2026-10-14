# Single sparse Schur-complement solve of the original least-squares constraints.
localmean_solve <- function(x, y, prb, nbh) {
  n <- length(x)
  nn <- localmean_neighbors(x, y, nbh)
  i <- rep(seq_len(n), each = nbh)
  j <- as.vector(t(nn))
  key <- unique(c((i - 1) * as.double(n) + j, (j - 1) * as.double(n) + i)) - 1
  i <- key %/% n + 1L
  j <- key %% n + 1L
  distance <- sqrt((x[i] - x[j])^2 + (y[i] - y[j])^2)
  o <- order(i, distance, j)
  i <- i[o]
  j <- j[o]
  degree <- tabulate(i, n)
  w <- (1 - (sequence(degree) - 1) / degree[i]) / prb[j]
  w <- w / as.numeric(rowsum(w, i))[i]
  column_weight_sums <- numeric(n)
  grouped_column_sums <- rowsum(w, j)
  column_weight_sums[as.integer(rownames(grouped_column_sums))] <- grouped_column_sums[, 1]
  A <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(n, n))

  adjacency_list <- split(j, i)
  component <- integer(n)
  reference_sites <- integer(0)
  queue <- integer(n)
  for (v in seq_len(n)) if (component[v] == 0L) {
    reference_sites <- c(reference_sites, v)
    component[v] <- length(reference_sites)
    head <- tail <- 1L
    queue[1] <- v
    while (head <= tail) {
      u <- queue[head]
      head <- head + 1L
      unvisited_neighbors <- adjacency_list[[u]][component[adjacency_list[[u]]] == 0L]
      if (length(unvisited_neighbors)) {
        component[unvisited_neighbors] <- component[v]
        queue[tail + seq_along(unvisited_neighbors)] <- unvisited_neighbors
        tail <- tail + length(unvisited_neighbors)
      }
    }
  }
  L <- Matrix::forceSymmetric((Matrix::Diagonal(x = degree) -
    A %*% Matrix::Diagonal(x = 1 / degree) %*% A) / 2)
  free_sites <- setdiff(seq_len(n), reference_sites)
  g <- numeric(n)
  if (length(free_sites)) {
    g[free_sites] <- as.numeric(Matrix::solve(
      L[free_sites, free_sites, drop = FALSE], (1 - column_weight_sums)[free_sites]))
  }
  ell <- -as.numeric(A %*% g) / degree
  w <- w + (ell[i] + g[j]) / 2
  if (any(!is.finite(w)) || max(abs(as.numeric(rowsum(w, i)) - 1)) >= 1e-7 ||
      max(abs(as.numeric(rowsum(w, j)) - 1)) >= 1e-7) return(NULL)
  list(ij = unname(cbind(i, j)), gwt = w)
}
