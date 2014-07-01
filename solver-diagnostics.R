require(ggplot2)
require(reshape)
require(RColorBrewer)
### Create a collection of tables with the following structure
### list: one element for each GCAM model period.  for each period:
###      list:  one element for each variable type, for each variable
###           data frame: values for each market by iteration number

### Note that the ordering (and possibly number) of the markets
### changes each time the solver exits and restarts.
read.trace.log <- function(filename, nmkt=200) {
    ## unfortunately, the number of markets in play fluctuates as
    ## markets are added and subtracted from the solvable set 200
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



### create a heat map of a single variable for a single period (i.e.,
### the bottom-level table in the list created by read.trace.log)

heatmap.var <- function(data, type="div", pal=1) {
    dm <- melt(data, id=c("mode","iter"), var="element")
    ggplot(data=dm, aes(x=element, y=iter, fill=value)) + geom_raster() +
        scale_fill_brewer(type=type, palette=pal)
}

