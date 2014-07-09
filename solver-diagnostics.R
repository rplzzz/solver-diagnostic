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
    ## Hue is piecewise constant on four intervals
    h1 <- ifelse(x<x1, 0, 90/360)
    h2 <- ifelse(x>x2, 240/360, 180/360)
    H <- ifelse(x<xm, h1, h2)
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
                           ifelse(magx < 10, (magx-1)-log10(ftol), 9-log10(ftol))))
    signx*xx                            # return value
}

### Return a transform function that takes the magnitude and clips it to a maximum value
clipped.mag.transform <- function(maxmag=10) {
    function(x) {
        magx <- abs(x)
        ifelse(magx>maxmag, maxmag, magx)
    }
}

### Return a transform that clamps the values to two bounds
clamp.transform <- function(xmin=-100, xmax=100) {
    function(x) {
        pmax(xmin, pmin(x, xmax))
    }
}

### Return a transform function that gives sign(x)*log(x/xmin).
### x-values less than xmin are flushed to zero, and x-values greater
### than xmax are clipped to xmax
signed.log.transform <- function(xmin=1e-4, xmax=100) {
    function(x) {
        signx <- sign(x)
        absx  <- pmax( pmin(abs(x), xmax), xmin)
        signx * log10(absx/xmin)
    }
}

### Transform deltax and deltafx values for better visualization
deltatransform <- function(x, maxmag=10) {
    magx <- abs(x)
    pmin(magx, maxmag)
}

heatmap.gcam <- function(data, xform=identity, colors=c("white","blue"), title="", breaks=waiver()) {
    nmkt <- ncol(data) - 2
    niter <- max(data$iter)
    dm    <- melt(data, id=c("mode","iter"))
    dm$component <- component.to.int(dm$variable)
    dm$value <- xform(as.numeric(dm$value))

    ggplot(data=dm, aes(x=component, y=iter, fill=value)) + geom_raster() +
        scale_fill_gradientn(colours=colors, na.value="black", breaks=breaks) +
            ggtitle(title) +
                scale_x_continuous(breaks=seq(10,nmkt,10)) + scale_y_continuous(breaks=seq(0,niter,20))
}
        


### create a heat map of a single variable for a single period (i.e.,
### the bottom-level table in the list created by read.trace.log).
### This version is tuned for looking at fx.
heatmap.fx <- function(data, title="", breaks=c(-12, -7, -3, 0, 3, 7, 12)) {
    heatmap.gcam(data, xform=fxtransform, colors=fxcolormap(), title=title, breaks=breaks)
}

### heat map for deltax and deltafx
heatmap.delta <- function(data, title="", maxmag=5) {
    heatmap.gcam(data, xform=clipped.mag.transform(maxmag), title=title)
}

### heatmap for total derivative
heatmap.dfdx <- function(data, title="") {
    heatmap.gcam(data, xform=clamp.transform(-100,100),
                 colors=fxcolormap(101,c(-100, -20, 0, 20, 100)), title=title)
}

### calculate a total derivative from deltax and deltafx
calc.total.deriv <- function(deltafx, deltax) {
    dfxmat <- as.matrix(cast(melt(deltafx, id=c("iter","mode")), iter~variable))
    dxmat  <- as.matrix(cast(melt(deltax, id=c("iter","mode")), iter~variable))

    as.data.frame(dfxmat/dxmat)
}
