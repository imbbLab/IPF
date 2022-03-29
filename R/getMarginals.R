getMarginals <- function(c, alphas, nn, margByOrder){

  maxOrder = length(c)
  m <- lapply(
    seq_len(maxOrder),
    function(ell){
      
      m <- NULL
      for(j in c[[ell]]){
        cat("ell =", ell, "j = ", j, "\n")
        microstates <- as.matrix(expand.grid(alphas[nn[[ell]]$c[, j]]))
        mm = paste0("m_", paste(nn[[ell]]$c[, j], collapse = ","), "(", apply(microstates, 1, function(x) paste(x, collapse = ",")),  ")")
        sel = which(margByOrder[[ell]]$marginals[[j]][microstates + 1] != 0)
        m = c(m,  mm[sel])
        
      }
      m
      
    }
    
  )
  unlist(m) 
}

getMarginalsState <- function(c, alphas, nn, margByOrder, L){
  
  maxOrder = length(c)
  m <- lapply(
    seq_len(maxOrder),
    function(ell){
      
      m <- NULL
      rows = NULL
      cols = list()
      for(j in c[[ell]]){
        cat("ell =", ell, "j = ", j, "\n")
        microstates <- as.matrix(expand.grid(alphas[nn[[ell]]$c[, j]]))
        mm = matrix(NA, ncol = L, nrow = nrow(microstates))
        mm[, nn[[ell]]$c[, j]] = microstates
        sel = which(margByOrder[[ell]]$marginals[[j]][microstates + 1] != 0)
        m = rbind(m,  mm[sel,])
        rows = c(rows, length(sel))
        cols = c(cols, list(nn[[ell]]$c[, j]))
      }
      list(m = m, rows = rows, cols = cols)
      
    }
    
  )
  list(
    rows = do.call("c", lapply(m, function(x) x$rows)),
    cols = do.call("c", lapply(m, function(x) x$cols)),
    m = do.call(rbind, lapply(m, function(x) x$m))
  )
}
