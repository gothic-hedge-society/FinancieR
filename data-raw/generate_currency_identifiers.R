
currency_identifiers <- xml2::read_html("https://www.xe.com/symbols.php") %>%
  rvest::html_node("table") %>%
  rvest::html_table() %>%
  tibble::as_tibble() %>%
  dplyr::select(X1, X2, X5) %>% 
  dplyr::filter(X1 != "Country and Currency") %>%
  dplyr::transmute(
    "three_letter_code" = X2,
    "currency_name"     = regmatches(
      X1, 
      regexpr(
        "(?:.(?!(?<!Convertible|Yuan)\\s))+$", 
        X1, 
        perl = TRUE
      )
    ) %>%
      trimws(),
    "symbol"            = X5,
    "country"           = purrr::map2_chr(
      X1, currency_name,
      function(x1, cn){
        trimws(gsub(paste0(" ", cn, "$"), "", x1))
      }
    )
  )

currency_identifiers$symbol[currency_identifiers$country == "India"]  <- "₹"
currency_identifiers$symbol[currency_identifiers$country == "Turkey"] <- "₺"

usethis::use_data(currency_identifiers, overwrite = TRUE)
