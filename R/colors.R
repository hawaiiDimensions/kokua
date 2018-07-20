#' @title Return colors based on a palette and quantitative variable
#'  
#' @description Assign colors to the values of a variable
#' 
#' @details This function linearly scales a variable to [0, 1] (potentially after a non-
#' linear transformation) and uses those scaled values as input into colorRamp
#' 
#' @param x the quantitative variable to be assigned colors
#' @param pal the colors to be supplied to colorRamp
#' @param trans the transformation to use on \code{x}
#' @param xlim limits on the variable x, can be omitted, in which case the range of x is used
#' 
#' @return A character vector of the desired colors
#' 
#' @examples
#' x <- 1:10
#' plot(x, col = quantCol(x, c('blue', 'red')))
#'
#' @author Andy Rominger <ajrominger@@gmail.com>
#' @seealso colorRamp
#' @export

quantCol <- function(x, pal, trans = c('linear', 'log', 'quadratic', 'cubic'), xlim = NULL) {
    trans <- match.arg(trans, c('linear', 'log', 'quadratic', 'cubic'))
    
    ## generate function to transform data
    tfun <- switch(trans,
                   'linear' = function(x) x,
                   'log' = function(x) log(x),
                   'quadratic' = function(x) x^2,
                   'cubic' = function(x) x^3)
    
    ## calculate limits on x variable
    if(is.null(xlim)) xlim <- range(x)
    
    ## linearly scale transformed x to be bounded [0, 1]
    y <- 1 / diff(range(tfun(xlim))) * (tfun(x) - min(tfun(xlim)))
    
    ## remove NAs to be dealt with later
    yna <- is.na(y)
    y[yna] <- 0
    
    ## generate colors
    # out <- rgb(grDevices::colorRamp(pal)(y), maxColorValue = 255)
    # grDevices -> scales::gradient_n_pal; also change rgb
    out <- scales::gradient_n_pal(pal)(y) # for kokua
    ## make missing values transparent
    out[yna] <- 'transparent'
    
    return(out)
}



#' @title anuenue
#' 
#' @description 
#' 
#' @details 
#' 
#' @param data Vector of numeric dat points
#' @param type String of data type
#' @param interval Optional two-element vector of start and endpoints
#' @param trans Optional string to force transformation
#' 
#' @return Vector of hexadecimal colors
#' 
#' @author Edward Greg Huang <eghuang@@berkeley.edu>
#' @seealso quantCol 
#' @export

anuenue <- function(data, type, trans = NULL, interval = NULL) {
    pal <- switch(type, # Currently implemented with toy palettes 
                  "age" = c("red", "purple"),
                  "precip" = c("cyan", "blue4"),
                  "temp" = c("yellow", "red"),
                  "elevation" = .elev()
    )
    if (is.null(trans)) {
        t <- "linear"
    } else {
        t <- trans
    }
    return(quantCol(data, pal, t, interval))
}

# hidden helper functions to create color pals for each data type
.age <- function() {
    cols <- viridis::magma(36)[-c(1:7, 34:36)]
    colsLab <- convertColor(t(col2rgb(cols)) / 255, from = 'sRGB', to = 'Lab')
    colsLab[, 1] <- seq(10, 95, length.out = nrow(colsLab))
    cols <- rgb(convertColor(colsLab, from = 'Lab', to = 'sRGB'))
    
    return(cols)
}

.precip <- function() {
    cols <- hsv(seq(0.45, 0.66, length.out = 20), seq(1, 0.5, length.out = 20), seq(0.1, 1, length.out = 20))
    colsLab <- convertColor(t(col2rgb(cols)) / 255, from = 'sRGB', to = 'Lab')
    colsLab[, 1] <- seq(10, 95, length.out = nrow(colsLab))
    cols <- rgb(convertColor(colsLab, from = 'Lab', to = 'sRGB'))
    
    return(cols)
}

.temp <- function() {
    cols <- heat.colors(30)[-c(1:3, 25:30)]
    colsLab <- convertColor(t(col2rgb(cols)) / 255, from = 'sRGB', to = 'Lab')
    colsLab[, 1] <- seq(35, 110, length.out = nrow(colsLab))
    cols <- rgb(convertColor(colsLab, from = 'Lab', to = 'sRGB'))
    
    return(cols)
}

.elev <- function() {
    cols <- terrain.colors(30)
    colsLab <- convertColor(t(col2rgb(cols)) / 255, from = 'sRGB', to = 'Lab')
    colsLab[, 1] <- seq(40, 96, length.out = nrow(colsLab))
    cols <- rgb(convertColor(colsLab, from = 'Lab', to = 'sRGB'))
    
    return(cols)
}


# plot(1:20, col = quantCol(1:20, .age()), pch = 16, cex = 3, ylim = c(-1, 20))
# points((1:20) - 2, col = quantCol(1:20, .temp()), pch = 16, cex = 3)
