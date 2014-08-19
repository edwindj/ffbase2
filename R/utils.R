# adds view window to ffdf, return a new ffdf (pointing to same data!)
get_window <- function(x, begin=1L, end=nrow(x)){ 
  stopifnot(is.ffdf(x))
  
  view <- c( begin-1L, # zero based
             1L + end - begin, 
             nrow(x) - end
           )
  view <- as.integer(view)
  #TODO check view?
  y <- lapply(physical(x), function(v){
    vw(v) <- view
    v
  })
  do.call(ffdf, y)
}

# iris.ffdf <- as.ffdf(iris)
# get_window(iris.ffdf, 4, 10)