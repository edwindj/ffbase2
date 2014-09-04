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

# get chunks that always include whole groups
grouped_chunks <- function(x, groups=group_size(x), chunk_size=get_chunk_size(x)){
  N <- sum(groups)
  offset <- cumsum(groups)
  ch <- chunk(from=1, to=N, by=chunk_size, maxindex = N)
  end <- sapply(ch, function(i) i[2])
  M <- sapply(offset, function(o){o <= end})
  
  end <- offset[if(is.null(dim(M))) sum(M) else rowSums(M)]
  begin <- head(c(1, 1+end), -1)
  lapply(seq_along(begin), function(i){
    ri(begin[i], end[i], N)
  })
}

get_chunk_size <- function( x
                          , RECORDBYTES = sum(.rambytes[vmode(x)])
                          , BATCHBYTES = getOption("ffbatchbytes")){
    BATCHBYTES %/% RECORDBYTES
}

# group_size <- c(3,6,2)
# chunk_size <- 6
# group_chunks(group_size, chunk_size)

# # iris.ffdf <- as.ffdf(iris)
# xw<- get_window(iris.ffdf, 4, 10)