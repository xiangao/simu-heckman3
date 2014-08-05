library(MASS)
library(sampleSelection)

## In this version, we are calculating MSE comparison between three
## models, to see when Heckman w/o instruments performs better than
## OLS.  It takes a long time to run, about a week, since I make the
## nobs change by 100, from 100 to 10,000.



## In this version, I am controling the correlation between x1 and w,
## and x1 and u (so, x1 is endogenous if non-zero correlation to u)
## w and u are uncorrelated.


## DGP: data$y <- data$x1 +  data$u
##      data$select <- (data$y + data$w + data$rn > df['sel.cri'])
## sel.cri is to control how much selection


    set.seed(6)
    nDim = 3
    sd11 = 1
    sdww=1
    crwu=0
    sduu=1

heckit1 <- function(data) {
    fit <- heckit(selection = select ~ x1, outcome = y ~ x1 , method = "ml", data=data)
    heckit1.sum <- summary(fit)$estimate
    heckit1.x1 <- heckit1.sum[which(row.names(heckit1.sum)=='x1')[2],'Estimate']-1
    return(heckit1.x1)
}

heckit2 <- function(data) {
    fit <- heckit(selection = select ~ x1 + w, outcome = y ~ x1 , method = "ml", data=data)
    heckit2.sum <- summary(fit)$estimate
    heckit2.x1 <- heckit2.sum[which(row.names(heckit2.sum)=='x1')[2],'Estimate']-1
    return(heckit2.x1)
}

gen.sim <- function(df){
    # covariance matrix with cr1w, cr1u from input.
    covarMat = matrix( c(sd11^2, df['cr1w']^2, df['cr1u']^2, df['cr1w']^2, sdww^2, crwu^2,  df['cr1u']^2, crwu^2, sduu^2 ) , nrow=nDim , ncol=nDim )
    # this part is to make sure the covariance matrix be positive definite.
    eS <- eigen(covarMat, symmetric = TRUE)
    ev <- eS$values
    tol = 1e-06
    if (!all(ev >= -tol * abs(ev[1L])))
        return(c(lm=NA, heck.noinst=NA, heck.inst=NA))
else {
    # generate data based on covariance matrix.
    data  = as.data.frame(mvrnorm(n=df['nobs'] , mu=rep(0,nDim), Sigma=covarMat ))
    names(data) <- c('x1','w','u')
    # dgp
    data$y <- data$x1 +  data$u
    data$rn <- rnorm(dim(data)[1], 0,1)
    # selection process
    data$select <- (data$y + data$w + data$rn > df['sel.cri'])
    data$y <- ifelse(data$select==1,data$y,NA)
    heckit1.x1 <- tryCatch(heckit1(data), error=function(e) NA)
    heckit2.x1 <- tryCatch(heckit2(data), error=function(e) NA)

    lm1 <- lm(y ~ x1 , data=data)
    lm.x1 <- summary(lm1)$coefficients['x1','Estimate']-1
    return(c(lm=lm.x1, heck.noinst=heckit1.x1, heck.inst=heckit2.x1))
}
}

# set parameter space
sim.grid = seq(1,100,1)
corr.grid = seq(0, .9, .3)
#nobs.grid = ceiling(exp(seq(4, 9, 1))/100)*100
nobs.grid = seq(100, 10000, 100)
sel.grid = seq(0,1,1)
data.grid <- expand.grid(nobs.grid, sim.grid, corr.grid, corr.grid, sel.grid)
names(data.grid) <- c('nobs', 'nsim','cr1w','cr1u','sel.cri')
results <- t(apply(data.grid, 1, gen.sim))
forshiny <- cbind(data.grid, results)
# write out for use in shiny.
write.csv(forshiny, 'results.csv')
results2 <- subset(results, nobs<=8000)
write.csv(results2, 'results2.csv')
