
getCombinations <- function(L, upTo){

  nn <- lapply(
    seq_len(upTo),
    function(ell){
      n <- choose(L, ell)
      c <- combn(seq_len(L), ell)
      cOc <- unlist(
        lapply(
          c(0L, seq_len(ncol(c))),
          function(m){
            res <- combn(seq_len(n), m)
            lapply(
              seq_len(ncol(res)),
              function(i) res[, i]
            )
            
          }
        ),
        recursive = FALSE
      )
      list(ell = ell, n = n, c = c, cOc = cOc)
    }
  )
  makeCombinations <- function(ell, cOc){
    if (ell == minEll ){
      newC <- which(
        apply(
          nn[[ell]]$c, 2,
          function(cc){
            all(
              sapply(
                (minEll + 1):upTo,
                function(j){
                  all(
                    sapply(
                      cOc[[j]],
                      function(k)   
                        sum(cc %in% nn[[j]]$c[, k]) < ell
                    )
                  )
                }
              )
            )
          }
        )
      )
      cOc[[ell]] <- sort(unlist(newC))
      combinations[[k]] <<- cOc
      k <<- k + 1
    } else{        
      newC <- which(
        apply(
          nn[[ell]]$c, 2,
          function(cc){
            all(
              sapply(
                (ell + 1):upTo,
                function(j){
                  all(
                    sapply(
                      cOc[[j]],
                      function(k)   
                        sum(cc %in% nn[[j]]$c[, k]) < ell
                    )
                  )
                }
              )
            )
          }
        )
      )
      if (length(newC) > 1){
        newcOc <- unlist(
          lapply(
            c(0L, seq_len(length(newC))),
            function(m){
              res <- combn(newC, m)
              lapply(
                seq_len(ncol(res)),
                function(i) res[, i]
              )
              
            }
          ),
          recursive = FALSE
        )
      } else {
        if(length(newC) == 1){
          newcOc <- list(which(1 == 0), newC)
        } else{
          newcOc <- list(which(1 == 0))
        }
      }
      for (i in seq_along(newcOc)){
        cOc[[ell]] <- sort(newcOc[[i]])
        makeCombinations(ell - 1, cOc)
      }
    }
  }
  minEll <- 1
  k  <- 1
  combinations <- list()
  
  for (i in seq_along(nn[[upTo]]$cOc)){
    tmp <- list()
    tmp[[upTo]] <- sort(nn[[upTo]]$cOc[[i]] )
    makeCombinations(upTo - 1, tmp)
  }
  list(
    combinations = combinations,
    nn = nn
  )
}
