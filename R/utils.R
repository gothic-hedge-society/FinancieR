# Turns a dataframe into a format that roxygen2 understands as a table.
#   Inspired by: http://r-pkgs.had.co.nz/man.html
tabular <- function(df, rowname_var = NULL, ...) {
  stopifnot(is.data.frame(df))
  
  if(!is.null(rowname_var)){
    df[rowname_var] <- paste0("\\emph{", rownames(df), "}")
    df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
    rownames(df) <- NULL
  }
  
  paste(
    "\\tabular{", 
    paste(
      vapply(
        df, 
        function(x){if(is.numeric(x)) "c" else "l"}, 
        FUN.VALUE = character(1)
      ), 
      collapse = ""
    ), 
    "}{ ",
    do.call(
      "paste",
      c(
        lapply(
          if(is.null(colnames(df))) df
          else rbind(paste0("\\strong{", c(colnames(df)), "}"), df),
          format,
          ...
        ),
        list(sep = " \\tab ", collapse = "\\cr  ")
      )
    ), 
    "}", 
    sep = ""
  )
  
}

compactify <- function(portfolio_vec, cpct, shorts){
  portfolio_vec %>% {
    if(cpct){
      . <- .[which(. != 0)]
      if(shorts){
        .[grep("^s_", names(.))] <- .[grep("^s_", names(.))] * -1
        names(.)[grep("^s_", names(.))] <- gsub(
          "^s_", "", grep("^s_", names(.), value = TRUE)
        )
        .
      }
      .
    } else {
      .
    }
  }
}
