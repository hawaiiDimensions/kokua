kokuaPlot <- function(dimensions = defaultDim, colorTemplate = defaultColors) {
    obj <- list(dimensions , colorTemplate)
    class(obj) <- append("kokuaPlot", class(obj))
    return(obj)
}

huki <- function(kokuaPlot) {
    UseMethod("huki", kokuaPlot)
}

huki.default <- function(kokuaPlot) {
    print("No such method defined for this object.")
    return(kokuaPlot)
}

huki.kokuaPlot <- function(KokuaPlot) {
    print("In huki.kokuaPlot")
    # kii <- plottingFunction()
    # # ...
    # # ...
    # # ...
    # return(kii)
}


