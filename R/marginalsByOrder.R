marginalsByOrder <- function(f, ell, L){
  if (ell > 0 && ell < L){
    tuples <- combn(seq_len(L), ell)
    marginals <- apply(
      tuples, 2,
      function(tuple){
        apply(f, tuple, sum)
      }
    )
    list <- lapply(seq_len(ncol(tuples)), function(i) tuples[, i])
    
    list(marginals = marginals, list = list)
  } else{
    list(marginals = f, list = list(seq_len(L)))
  }
}