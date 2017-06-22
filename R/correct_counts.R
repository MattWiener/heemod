#' Title
#'
#' @param x a count table
#' @param method either a string or a function - see details.
#'
#' @details If `method` is a string, it should be either `life-table`,
#'   `beginning`, or `end`.  if `method = 'life-table'`, then the function
#'   returns, as the count for
#'   a cycle, the average of the count at the beginning and at the end
#'   of the cycle.   If `method = 'beginning'``, it considers transitions to
#'   take place at the beginning of each cycle, so it returns, for each cycle,
#'   the number of counts at the end of the cycle (or, equivalently,
#'   at the beginning of the next cycle).
#'   If `method = 'end'`, it considers transitions to take place at the end 
#'   of each cycle, so it returns, for each cycle, the number of counts at
#'   the beginning of the cycle (or, equivalently, at the end of the previous
#'   cycle).
#'   
#'   If `method` is a function, the `x` will be passed to that function.
#'   The function must return a data frame with one fewer row than `x`.
#' @return  a data frame with one fewer row than `x`.
#'
correct_counts <- function(x, method = c("life-table",
                                         "beginning",
                                         "end")) {
  
  if (! is.function(method)) {
    method <- match.arg(method)
    
    n0 <- x[- nrow(x), ]
    n1 <- x[-1, ]
    
    switch(
      method,
      "beginning" = {
        out <- n1
      },
      "end" = {
        out <- n0
      },
      "life-table" = {
        out <- (n0 + n1) / 2
      })
  } else {
    out <- method(x)
  }
  
  if (nrow(out) != nrow(x) - 1) {
    stop("State membership correction applied to an n-row table should return a table with n-1 rows.")
  }
  
  return(out)
}
