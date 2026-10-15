# Sum v within site groups, preserving sapply(split(v, i), sum).
# For ordinary numeric indices covering sites 1..n, construct the known
# factor directly. Each site supplies a neighborhood row.
# Other groupings use the original expression/positional behavior.
localmean_site_sums <- function(v, i, n) {
  # Use the shortcut only for ordinary, valid site indices.
  valid_indices <- (is.integer(i) || is.double(i)) && !is.object(i) &&
    length(i) && length(n) == 1L && is.finite(n) &&
    n >= 1L && n <= .Machine$integer.max && n == floor(n) &&
    all(is.finite(i)) && all(i >= 1 & i <= n & i == floor(i))
  if (!valid_indices) {
    return(sapply(split(v, i), sum))
  }

  site_index <- as.integer(i)
  if (any(tabulate(site_index, n) == 0L)) {
    return(sapply(split(v, i), sum))
  }

  # Match factor() labels: a large double can format as "1e+05",
  # whereas the corresponding integer formats as "100000".
  sites <- seq_len(n)
  if (is.double(i)) sites <- as.double(sites)
  groups <- structure(site_index, levels = as.character(sites), class = "factor")
  sapply(split(v, groups), sum)
}
