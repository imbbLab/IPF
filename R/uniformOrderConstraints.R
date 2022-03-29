library(mipfp)
library(parallel)

dklUniformFile = "../data/dklUniform.RData"

if (! file.exists(dklUniformFile)){
  ## get the data
  source("../R/getData.R")
  ## marginals
  source("../R/marginalsByOrder.R")
  
  ## subsampling
  subSampleUniform <- function(subSampleSize, ells = c(1,2,3)){
    
    ## define the multi-dimensional "empirical" distribution
    fs <- array(
      1,
      dim = q
    )
    ## fill it with multinomial samples according to the empirical
    ## multinomial probabilitites stored in f
    fs[idx] <- rmultinom(
      n = 1, 
      size = subSampleSize, 
      prob = h
    )
    
    ## regularize all entries, where the original f was non-zero
    fs[f > 0] = fs[f > 0] + 1
    
    ## compute the IPF models for the orders stored in ells
    res <- sapply(
      ells,
      function(ell){
        ## order ell marginal constraints
        m <- marginalsByOrder(fs, ell, L)
        
        ## define the seed
        seed <- array(
          1,
          dim = q
        )
        
        ## run IPF
        p.hat = Ipfp(
          seed = seed, 
          target.list = m$list,
          target.data = m$marginals,
          iter = 10000
        )$p.hat
        
        ## calculate KL divergence
        sum(ifelse(f > 0, f * (log(f) - log(p.hat)), 0))
      }
    )
    ## "empirical"
    c(
      res,
      sum(ifelse(f > 0, f * (log(f) - log(fs / sum(fs))), 0))
    )
  }
  
  
  ### set seed
  seed = 20220308
  set.seed(seed)
  
  ### subsample sizes
  Ns <- c(5000, seq(10000, 390000, by = 10000), N)
  
  ### start sampling
  
  nSamples = 20000
  
  dklUniform <- lapply(
    Ns,
    function(subSampleSize){
      cat("start sampling ", subSampleSize, "\n")  
      res <- mclapply(
        1:nSamples,
        function(i){
          subSampleUniform(subSampleSize)
        },
        mc.cores = 256
      )
      cat("finished\n")
      matrix(unlist(res), ncol = 4, byrow = TRUE)
    }
  )
  
  save(seed, dklUniform, file = dklUniformFile)
} else{
  load(dklUniformFile)  
} 






