getCoefficientMatrix <- function(c, alphas, nn, margByOrder){
    maxOrder = length(c)
    C <- lapply(
        seq_len(maxOrder),
        function(ell){
            C <- NULL
            m <- NULL
            for(j in c[[ell]]){
                microstates <- as.matrix(expand.grid(alphas[nn[[ell]]$c[, j]]))
                m = c(m,  margByOrder[[ell]]$marginals[[j]][microstates + 1])
                microstates <- apply(microstates, 1, paste0, collapse = "")
                tmp <- matrix(A[, nn[[ell]]$c[, j]], ncol = ell)
                tmp <- apply(tmp, 1, paste0, collapse = "")
                
                mm <- match(tmp, microstates)
                
                CC <- matrix(0, ncol = prod(q), nrow = length(microstates))
                for( row in seq_len(nrow(CC))){
                    CC[row, which(mm == row)] <- 1
                }
                
                C <- rbind(C, CC)
            }
            list(C = C, m = m)
            
        }
        
    )
    m <- do.call("c", lapply(C, function(x) x$m)) 
    C <- do.call(rbind, lapply(C, function(x) x$C))
    
    
    
    ## remove rows with zero marginals
    zeroMarginals <- which(m == 0)
    if (length(zeroMarginals) > 0){
        removeCols <- apply(matrix(C[zeroMarginals, ], ncol = ncol(C)), 1, function(x) which(x == 1))
        removeCols <- sort(unique(as.integer(unlist(removeCols))))
    
        C <- C[-zeroMarginals, -removeCols]
    }
    
    C    
}
