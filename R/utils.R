year_to_fiscal <- function(year, direction = "back") {
  year_digits <- as.numeric(str_sub(year, 3, 4))

  if(direction == "back") {
  prev_year <-  year_digits - 1

  prev_year[prev_year == -1] <- 99

  fiscal <- paste0(str_pad(prev_year, width = 2, pad = "0"),
    str_pad(year_digits, width = 2, pad = "0")
         )
  } else if(direction == "forward") {
    next_year <-  year_digits + 1


    next_year[next_year == 100] <- 00

    fiscal <- paste0(str_pad(year_digits, width = 2, pad = "0"),
           str_pad(next_year, width = 2, pad = "0")
    )
  }
  return(fiscal)
}
