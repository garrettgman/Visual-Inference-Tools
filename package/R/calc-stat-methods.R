# CALC_STAT methods

calc_mean <- function(x) {
	mean(x)
}

calc_median <- function(x) {
	median(x)
}

calcCIWald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCIBootPerc <- function(x){
    n <- length(x)
    nboots <- 999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, mean)
    quantile(means, prob = c(0.025, 0.975))
}

calcCIBootSE <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, mean)
    se <- sd(means)
    mean(x) + c(-1, 1)*1.96*se
}

calcCIBootTSE <- function(x){
    n <- length(x)
    nboots <- 1000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, mean)
    se <- sd(means)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

loadStat <- function(stat, method){
    if (stat == "confidence interval"){
        CALC_STAT <<- c(calcCIWald, calcCIBootPerc, calcCIBootSE,
                        calcCIBootTSE)[[which(method == c("normal", "percentile bootstrap",
                                        "normal bootstrap", "t bootstrap"))]]}
    else{
        CALC_STAT <<- c(calc_mean,
                        calc_median)[[which(stat == c("mean", "median"))]]
    }
}


