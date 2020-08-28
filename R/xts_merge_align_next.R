#' @title Merge 2 XTS Tables According to Next Non-Missing Value
#'
#' @description You will probably never need to call this function directly, but
#'   it's included just in case the need arises to perform a custom merge.
#'
#'  For two XTS objects \code{xts1} and \code{xts2}, let \code{A} be any
#'  datetime index that appears in \code{index(xts2)}, falls between
#'  \code{min(index(xts1))} and \code{max(index(xts1))}, but does \bold{not}
#'  appear in \code{index(xts1)}. Let \code{B} be the earliest-occurring
#'  datetime index in \code{index(xts1)} that occurs \bold{after} \code{A}.
#'
#'   \code{xts_merge_align_next(xts1, xts2)} will return the left-join merge of
#'   \code{xts1} and \code{xts2} with one additional operation: for any/all
#'   \code{A} and \code{B} that occur for the XTS pair, the values at at index
#'   \code{B} for the columns in the merged XTS coming from \code{xts2} equal
#'   the aggregated value (as specified; e.g., \code{sum}) of the values of the
#'   columns in \code{xts2} in the range \code{A:B}.
#'
#' @param xts1 An XTS object
#' @param xts2 An XTS object that may or may not have indices that fall within
#'   the range of \code{xts1}, but do not appear in \code{xts1}
#' @param agg_function Any function that accepts a numeric vector as input and
#'   returns a single number as output; e.g., sum
#' @param na.fill Not all indices of \emph{xts2} may occur in \emph{xts1},
#'   therefore, the left join (\emph{xts1}, \emph{xts2}) will almost always
#'   result in missing values (NA) in \emph{xts2} columns of the merged xts.
#'   \emph{na.fill} specifies what value to fill in for those NA values.
#'
#' @example inst/examples/xts_merge_align_next_ex.R
#'
#' @export
#'
xts_merge_align_next <- function(xts1, xts2, agg_function, na.fill){

  if(setequal(find_numeric_columns(xts2), colnames(xts2))){
    storage.mode(xts2) <- "numeric"
  }

  # Disregard dates in xts2 that fall outside the range of xts1
  # Then, want to full merge them because we need the NAs, if any.

  merged_xts <- xts::merge.xts(
    xts1,
    xts2[
      paste0(zoo::index(xts::first(xts1)), "/", zoo::index(xts::last(xts1)))
    ]
  )

  mismatch_dates <- merged_xts[,colnames(xts1)] %>%
    apply(
      MARGIN = 1,
      FUN     = function(xts1_row){all(is.na(xts1_row))}
    ) %>% {
      stats::setNames(.[-length(.)] - .[-1], names(.)[-1])
    }

  if(all(mismatch_dates == 0)){
    return(xts::merge.xts(xts1, xts2, join = "left", fill = na.fill))
  }

  # agg range gives the date ranges that need to be aggregated by agg_function
  agg_range <-  tibble::tibble(
    "from"      = names(mismatch_dates[which(mismatch_dates == -1)]),
    "to"        = names(mismatch_dates[which(mismatch_dates == 1)]),
    "xts_range" = paste0(from, "/", to)
  )

  if(nrow(agg_range) == 0) return(NULL)

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
