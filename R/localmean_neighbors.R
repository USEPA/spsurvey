# distance/index ordering
localmean_neighbors <- function(x, y, nbh, max_neighbor_entries = 1000000L) {
  n <- length(x)
  xy <- cbind(x, y)
  nbh <- min(nbh, n)
  out <- matrix(0L, n, nbh)
  rows <- seq_len(n)
  k <- min(n, 2 * nbh)
  repeat {
    batch_size <- max(1L, floor(max_neighbor_entries / k))
    unresolved <- integer(0)
    for (start in seq.int(1L, length(rows), by = batch_size)) {
      ii <- rows[seq.int(start, min(length(rows), start + batch_size - 1L))]
      idx <- RANN::nn2(xy, xy[ii, , drop = FALSE], k = k, eps = 0)$nn.idx
      distance <- sqrt((x[ii] - x[idx])^2 + (y[ii] - y[idx])^2)
      dim(distance) <- dim(idx)
      o <- order(rep(seq_along(ii), k), distance, idx)
      candidates <- matrix(idx[o], ncol = k, byrow = TRUE)
      distance <- matrix(distance[o], ncol = k, byrow = TRUE)
      out[ii, ] <- candidates[, seq_len(nbh), drop = FALSE]
      if (k < n) {
        needs_more_neighbors <- !(distance[, k] > distance[, nbh] * (1 + 1e-12))
        unresolved <- c(unresolved, ii[needs_more_neighbors])
      }
    }
    if (!length(unresolved)) break
    rows <- unresolved
    k <- min(n, 2 * k)
  }
  out
}
