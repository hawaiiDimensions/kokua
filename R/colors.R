sfiColors <- c('#D68F85', '#005D77', '#51661A', '#D49A34', 
               '#58455F', '#AF2F23', '#008E94', '#A27635', 
               '#636051', '#DECEA1', '#D15A2A', '#CCCEC8', '#322B29')

foo <- convertColor(t(col2rgb(sfiColors)) / 255, from = 'sRGB', to = 'Lab')


m <- persp(range(foo[, 2]), range(foo[, 3]), matrix(range(foo[, 1]), nrow = 2, ncol = 2), 
           col = 'transparent', border = 'transparent', xlab = 'a', ylab = 'b', zlab = 'L', 
           theta = 30, phi = 60)
points(trans3d(foo[, 2], foo[, 3], foo[, 1], pmat = m), col = sfiColors, pch = 16, cex = 2)
text(trans3d(foo[, 2], foo[, 3], foo[, 1], pmat = m), labels = 1:length(sfiColors), col = 'white', cex = 0.5)


# different color sets
warm <- c(1, 4, 6, 8, 10, 11, 13)
cool <- c(2, 3, 5, 7, 9, 12, 13)
bigGradient <- c(2, 7, 3, 8, 4, 11, 6)
divergent1 <- c(7, 12, 11)
divergent2 <- c(2, 12, 4)
divergent3 <- c(2, 12, 8)
linear1 <- c(7, 9, 11)
linear2 <- c(7, 13, 6)
linear3 <- c(2, 9, 8)
linear4 <- c(2, 9, 4)
linear5 <- c(2, 12, 10)
linear6 <- c(7, 10, 4)
lilGradient1 <- c(5, 6, 11, 4)
lilGradient2 <- c(5, 2, 7, 3)

# package `scales` could be useful for interpolating colors
foo <- scales::gradient_n_pal(sfiColors[lilGradient1])
plot(1:10, col = foo(seq(0, 1, length.out = 10)), pch = 16, cex = 2)


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

bar <- quantCol(1:10, c("red", "blue"))
plot(1:10, col = bar, pch = 16, cex = 5)



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

anuenue <- function(data, type, trans = NULL, interval = NULL) {
    pal <- switch(type, # Currently implemented with toy palettes 
                  "age" = c("red", "purple"),
                  "precip" = c("cyan", "blue4"),
                  "temp" = c("yellow", "red"),
                  "elevation" = c("grey48", "white") 
    )
    if (is.null(trans)) {
        t <- "linear"
    } else {
        t <- trans
    }
    return(quantCol(data, pal, t, interval))
}



