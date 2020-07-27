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
#' @example 
#' # 
#'   
#' @export   

xts_merge_align_next <- function(xts1, xts2, agg_function, na.fill){
  
  storage.mode(xts2) <- "numeric"
  
  merged_xts <- merge(
    xts1, 
    # Disregard dates in xts2 that fall outside the range of xts1
    xts2[paste(
      as.Date(min(zoo::index(xts1))),
      as.Date(max(zoo::index(xts1))),
      sep = "/"
    )] 
  )
  
  # If there are no dates that are in xts2 but not xts1, return the left-join.
  if(
    length(
      setdiff(
        as.character(as.Date(zoo::index(xts2))),
        as.character(as.Date(zoo::index(xts1)))
      )
    ) == 0
  ){
    return(zoo::merge.zoo(xts1, xts2, all = c(TRUE, FALSE), fill = na.fill))
  }
  
  agg_range <- merged_xts[,colnames(xts1)] %>%
    apply(
      MARGIN = 1, 
      FUN     = function(xts1_row){all(is.na(xts1_row))}
    ) %>% {
      stats::setNames(.[-length(.)] - .[-1], names(.)[-1])
    } %>% {
      tibble::tibble(
        "from"      = names(.[which(. == -1)]),
        "to"        = names(.[which(. == 1)]),
        "xts_range" = paste0(from, "/", to)
      )
    } 
  
  for(i in 1:nrow(agg_range)){
    for(xts2_col in colnames(xts2)){
      zoo::coredata(merged_xts[agg_range$to[i], xts2_col]) <- do.call(
        agg_function,
        list(xts2[agg_range$xts_range[i], xts2_col])
      )
      merged_xts <- merged_xts[
        setdiff(
          as.character(as.Date(zoo::index(merged_xts))),  
          agg_range$from[i]
        )
      ]
    }
  }
  
  merged_xts[
    which(is.na(merged_xts[,colnames(xts2)])), colnames(xts2)
  ] <- na.fill
  
  merged_xts
  
}
