require(ggplot2)
require(reshape)
require(RColorBrewer)
require(graphics)
### Create a collection of tables with the following structure
### list: one element for each GCAM model period.  for each period:
###      list:  one element for each variable type, for each variable
###           data frame: values for each market by iteration number

### Note that the ordering (and possibly number) of the markets
### changes each time the solver exits and restarts.
read.trace.log <- function(filename, nmkt=200) {
    ## unfortunately, the number of markets in play fluctuates as
    ## markets are added and subtracted from the solvable set. 200
    ## seems to be about the largest it gets.
    varn <- sapply(seq(1,nmkt), function(i) {paste("x",i, sep="")})
    colnames   <- c("period", "iter", "mode", "name", varn)
    colclasses <- c("factor", "integer", "factor", "factor", rep("numeric", nmkt))
    rawdata <- read.table(filename, sep=',', strip.white=TRUE, skip=2, fill=TRUE,
                          col.names=colnames, colClasses=colclasses)
    data.by.period <- split(rawdata, rawdata$period)
    lapply(data.by.period,
           function(df) {
               df$period <- NULL
               dfs <- split(df, df$name)
               lapply(dfs, function(d) {d$name <- NULL;d})
           }) 
}

### change a vector of component names (like "V26") into a vector of
### component numbers
component.to.int <- function(compvec) {
    as.integer(sapply(compvec, function(t){substr(t,2,6)}))
}

fxcolormap <- function(n=51, controlpts=c(-10,-3,0,3,10)) {
    xlo <- controlpts[1]
    x1  <- controlpts[2]
    xm  <- controlpts[3]
    x2  <- controlpts[4]
    xhi <- controlpts[5]
    
    x = seq(0,n-1) * (xhi-xlo)/(n-1) + xlo
    ## Hue is piecewise constant on three intervals
    H <- ifelse(x<x1, 0,
                ifelse(x>x2, 240/360, 120/360))
    ## Use "option 2 for the saturation"
    S <- ifelse(x<xm, 1-(x-xlo)/(xm-xlo), (x-xm)/(xhi-xm))
    ## Constant 1 for value

    hsv(H, S, 1)
}

### Transform F(x) values for better visualization
fxtransform <- function(x) {
    ftol  <- 1.0e-3                     # threshold for considering a market "solved"
    signx <- sign(x)
    magx  <- abs(x)
    xx    <- ifelse(magx < ftol, 0,
                    ifelse(magx < 1, log10(magx)-log10(ftol),
                           ifelse(magx < 10, (magx-1)-log10(ftol), 10+log10(ftol))))
    signx*xx                            # return value
}

### Transform deltax and deltafx values for better visualization
deltatransform <- function(x) {
    magx <- abs(x)
    ifelse(magx>10, 10, magx) 
}

### create a heat map of a single variable for a single period (i.e.,
### the bottom-level table in the list created by read.trace.log).
### This version is tuned for looking at fx.
heatmap.fx <- function(data) {
    nmkt  <- ncol(data) - 2
    niter <- max(data$iter)
    dm    <- melt(data, id=c("mode","iter"), var="component.name")
    dm$component <- component.to.int(dm$component.name)
    dm$value <- fxtransform(dm$value)
    ggplot(data=dm, aes(x=component, y=iter, fill=value)) + geom_raster() +
        scale_fill_gradientn(colours=fxcolormap(), na.value="black", breaks=c(-7, -3, 0, 3, 7)) +
            scale_x_continuous(breaks=seq(10,nmkt,10)) + scale_y_continuous(breaks=seq(0,niter,20))
}

### heat map for deltax and deltafx
heatmap.delta <- function(data, title="") {
    nmkt  <- ncol(data) - 2
    niter <- max(data$iter)
    dm    <- melt(data, id=c("mode","iter"), var="component.name")
    dm$component <- component.to.int(dm$component.name)
    ## Plot absolute values, cut off the distribution at 10
    dm$value <- deltatransform(dm$value)

    ggplot(data=dm, aes(x=component, y=iter, fill=value)) + geom_raster() +
        scale_fill_gradient(low="white", high="blue", na.value="black") +
            ggtitle(title) +
            scale_x_continuous(breaks=seq(10,nmkt,10)) + scale_y_continuous(breaks=seq(0,niter,20))
}

