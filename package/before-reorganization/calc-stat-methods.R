# CALC_STAT methods
calcCIWald <- function(x){
    n <- length(x)
    se <- sd(x)/sqrt(n)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}

calcCIBootPerc <- function(x){
    statfun <- eval(parse(text = e$cistat))
    statfun <- mean
    n <- length(x)
    nboots <- 9999
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    quantile(means, prob = c(0.025, 0.975))
}

calcCIBootSE <- function(x){
    statfun <- eval(parse(text = e$cistat))
    n <- length(x)
    nboots <- 10000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    se <- sd(means)
    mean(x) + c(-1, 1)*2*se
}

calcCIBootTSE <- function(x){
    statfun <- eval(parse(text = e$cistat))
    n <- length(x)
    nboots <- 10000
    samps <- matrix(sample(x, size = nboots*n, replace = TRUE), nrow = nboots,
                    ncol = n)
    means <- apply(samps, 1, statfun)
    se <- sd(means)
    mean(x) + c(-1, 1)*qt(0.975, n - 1)*se
}




