library(pracma)

dklMixedFile = "../data/mixedOrder.RData"


if (! file.exists(dklMixedFile) ){
    ## load DKLs
    dklMixed <- sapply(
        seq_len(200),
        function(fold){
            file <- paste0("../data/mixedOrder.", sprintf("%03d", fold), ".rds")
            readRDS(file)
        }
    )
    dklMixed <- matrix(unlist(dklMixed), ncol = length(combinations), byrow = TRUE)
    
    
    ## get the data
    source("../R/getData.R")
    ## getCombinations
    source("../R/getCombinations.R")
    
    combinations <- getCombinations(L, L - 1)
    nn <- combinations$nn
    combinations <- combinations$combinations
    
    ## determine the ell order all constraint sets
    allOne <- which(sapply(combinations, function(x) length(x[[1]]) == 5))
    allTwo <- which(sapply(combinations, function(x) length(x[[2]]) == 10))
    allThree <- which(sapply(combinations, function(x) length(x[[3]]) == 10))
    allFour <- which(sapply(combinations, function(x) length(x[[4]]) == 5))
    
    ## calculate the difference between the KL divergences
    ## between constraint set k and the all fourth order constraint set
    ## for each of the 20 000 subsamples
    deltas <- dklMixed - dklMixed[, allFour]
    
    ## calculate the median difference for each of the
    ## 6 893 sets of constraints
    deltasMedian <- apply(deltas, 2, median)
    
    ## order them
    deltasMedianOrder <- order(deltasMedian)
    ## determine top 10 models
    top10 <- tapply(
        deltasMedianOrder[1:1000],
        round(deltasMedian[deltasMedianOrder[1:1000]], 10),
        function(i) i
    )
    top10 <- sum(sapply(top10[1:10], length))
    
    
    
    
    ## get CoefficientMatirx
    source("../R/getCoefficientMatrix.R")
    
    ## marginals
    source("../R/marginalsByOrder.R")
    margByOrder <- lapply(
        c(1,2,3,4),
        function(ell){
            marginalsByOrder(
                f = f,
                ell = ell,
                L = L
            )
        }
    )
    alphas <- list(
        ageGroup = 0:5,
        sex = 0:1,
        arrivalByAmbulance = 0:1,
        hospitalization = 0:1,
        critical = 0:1
    )
    
    CReduced <- lapply(
        combinations,
        getCoefficientMatrix,
        alphas = alphas,
        nn = nn,
        margByOrder = margByOrder
        
    )
    ## determine rank
    ranks <- sapply(
        CReduced,
        Rank
    )
    
    dataMixed <- list(
        deltaDkl = deltas[,deltasMedianOrder[seq_len(top10)]],
        rank = ranks[deltasMedianOrder[seq_len(top10)]],
        combinations = combinations[deltasMedianOrder[seq_len(top10)]],
        coefficientMatrix = CReduced[deltasMedianOrder[seq_len(top10)]],
        allOne = deltasMedian[allOne],
        allTwo = deltasMedian[allTwo],
        allThree = deltasMedian[allThree],
        allFour = deltasMedian[allFour]
    )
    save(dataMixed, file = dklMixedFile)
} else{
    load(dklMixedFile)
}