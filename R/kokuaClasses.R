createIke <- function(obj, envType) {
    class(obj) <- 'ike'
    attr(obj, 'envType') <- envType # like precip
}

plot.ike <- function(x, col) {
    if (is.missing(col)) {
        col <- switch(attr(x, 'envType'),
                      'precip' = x,
                      'substrate' = y)
    }
    # plot the ike object
}