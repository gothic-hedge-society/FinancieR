#' @title Merge 2 XTS Tables According to Next Non-Missing Value
#' 
#' @description For two XTS objects \code{xts1} and \code{xts2}, let \code{A} 
#'   be any datetime index that appears in \code{index(xts2)}, falls between 
#'   \code{min(index(xts1))} and \code{max(index(xts1))}, but does \bold{not} 
#'   appear in \code{index(xts1)}. Let \code{B} be the earliest-occurring 
#'   datetime index in \code{index(xts1)} that occurs \bold{after} \code{A}. 
#'   
#'   \code{xts_merge_align_next(xts1, xts2)} will return the left-join
#'   merge of \code{xts1} and \code{xts2} with one additional operation: for 
#'   any/all \code{A} and \code{B} that occur for the XTS pair, the values at
#'   at index \code{B} for the columns in the merged XTS coming from \code{xts2}
#'   equal the aggregated value (as specified; e.g., \code{sum}) of the values 
#'   of the columns in \code{xts2} in the range \code{A:B}.
#'   
#' @param xts1 An XTS object
#' @param xts2 An XTS object that may or may not have indices that fall within
#'   the range of \code{xts1}, but do not appear in \code{xts1}
#' @param agg_function Any function that accepts a numeric vector as input and
#'   returns a single number as output; e.g., sum
#' @param ... Additional parameters to pass to \code{agg_function}
#'   
#' @export   

xts_merge_align_next <- function(xts1, xts2, agg_function, ...){
  
  loadNamespace("xts")
  
  agg_function <- match.fun(agg_function)
  
  xts2 <- xts2[paste(
    as.Date(min(zoo::index(xts1))),
    as.Date(max(zoo::index(xts1))),
    sep = "/"
  )]
  
  for(
    disjunction_date in setdiff(
      as.character(as.Date(zoo::index(xts2))), 
      as.character(as.Date(zoo::index(xts1)))
    )
  ){
    xts2 <- xts::rbind.xts(
      xts2,
      xts::as.xts(
        matrix(
          # vapply works column-wise when passed an XTS
          vapply(
            xts2[paste(
              disjunction_date,
              as.Date(min(zoo::index(xts1[paste0(disjunction_date, "/")]))),
              sep = "/"
            )],
            FUN = agg_function,
            FUN.VALUE = numeric(1),
            ...
          ),
          nrow = 1,
          ncol = ncol(xts2),
          dimnames = list(
            as.character(
              as.Date(min(zoo::index(xts1[paste0(disjunction_date, "/")])))
            ), 
            colnames(xts2)
          )
        ) 
      )
    )
  }
  
  zoo::merge.zoo(xts1, xts2, all = c(TRUE, FALSE))
  
}
