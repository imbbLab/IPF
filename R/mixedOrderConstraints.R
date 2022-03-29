library(mipfp)
library(parallel)

## get the data
source("../R/getData.R")
## marginals
source("../R/marginalsByOrder.R")
## combinations
source("../R/getCombinations.R")

combinations <- getCombinations(L = L, upTo = L - 1)
combinations <- combinations$combinations

## subSampleMixed
subSampleMixed <- function(subSampleSize){
  ## define the multi-dimensional "empirical" distribution
  fs <- array(
    1,
    dim = q
  )
  ## fill it with multinomial samples according to the empirical
  ## multinomial probabilities stored in f
  fs[idx] <- rmultinom(
    n = 1, 
    size = subSampleSize, 
    prob = h
  )
  
  ## regularize all entries, where the original f was non-zero
  fs[f > 0] <- fs[f > 0] + 1
  margByOrder <- lapply(
    c(1,2,3,4),
    function(ell){
      marginalsByOrder(
        f = fs,
        ell = ell,
        L = L
      )
    }
  )
  
  ## estimate the MaxEnt distribution
  ## for each set of marginal constraints
  PP <- mclapply(
    combinations,
    function(comb){
      seed <- array(
        1,
        dim = q
      )
      ## run IPF
      target.list <- c(
        margByOrder[[1]]$list[comb[[1]]],
        margByOrder[[2]]$list[comb[[2]]],
        margByOrder[[3]]$list[comb[[3]]],
        margByOrder[[4]]$list[comb[[4]]]
      )
      target.data <- c(
        margByOrder[[1]]$marginals[comb[[1]]],
        margByOrder[[2]]$marginals[comb[[2]]],
        margByOrder[[3]]$marginals[comb[[3]]],
        margByOrder[[4]]$marginals[comb[[4]]]
      )
      
      Ipfp(
        seed = seed, 
        target.list = target.list,
        target.data = target.data,
        iter = 10000
      )$p.hat
      
    },
    mc.cores = 256
  )
  
  ## return KL Divergence 
  sapply(
    PP, 
    function(p){
      sum(ifelse(f > 0, f * (log(f) - log(p)), 0))
    }
  )
}

seed = 20220309
set.seed(seed)

## subSampleSize
subSampleSize = N

## number of samples
nSamples = 100

## subsample in chunks of 200 subsamples
## at a time
for (fold in seq_len(200)){
  cat("\n", sprintf("%03d", fold), "\n")
  dkl <- lapply(
    1:nSamples,
    function(i){
      cat(".")
      if (i %% 50 == 0){
        cat(" ", i, "\n")
      }
      subSampleMixed(subSampleSize)
    }
  )
  
  saveRDS(dkl, file = paste0("../data/mixedOrder.", sprintf("%03d", fold), ".rds"))
}
