## load the data
empirical <- read.csv("../data/empirical.csv")

## number of features
L <- 5

## number of states per feature
q <- c(6, 2, 2, 2, 2)

## microstrate
A <- as.matrix(
  empirical[, 
            c(
              "ageGroup", 
              "sex", 
              "arrivalByAmbulance", 
              "hospitalization", 
              "critical"
            )
  ]
)

## a table for indexing microstate in a multidimensional array
idx <- A + 1

## empirical counts for the microstates
h <- empirical$count

## the total number of records
N <- sum(h)

## the multidimensional array
f <- array(
  1,
  dim = q
)
f[idx] = h / N


