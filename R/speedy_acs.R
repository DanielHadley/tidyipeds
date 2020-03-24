

# To Do: 2009 acsvars has some problems:
# remove colons at the end of strings from levlab for 2009 variables in acsvars.
# Remove table numbers from concepts in acsvars for 2009
# for white alone tables in 2009 the levels are off as well




#' @import dplyr
#' @importFrom magrittr %>%
#'
get_acs_combo <- function(table = NULL, year = 2018, geography, state = NULL, county = NULL, survey = "acs5", which.races = NULL, use.parallel = T) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package \"dplyr\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # run in parellel unless specified by the user not to do so. set plan status back to what it was previous upon function end
  if (use.parallel){
  oplan <- future::plan("multisession")
  on.exit(future::plan(oplan), add = TRUE)
  }


  arguments <- as.list(environment(), all.names = T)

  # if(!arguments$table %in% race_tables){
  #   #TO DO: Figure out how to handle errors
  #   print("table is not a race table")
  # }
  years <- arguments$year

  # get a table with no by-race breakdown
  if(is.null(arguments$which.races)){
    # remove which.races so it doesn't filter down to si_acs()
    arguments <- arguments[which(names(arguments) != "which.races")]
    names(years) <- as.character(years)
    df <- furrr::future_map_dfr(years, .progress = T, .id = "year", .f = function(x){
      arguments$year <- x
      do.call(tidycensus::get_acs, args = arguments)
    })
    df$year <- as.integer(df$year)
  } else {
    # get a table with by-race breakdown
    races <- c(
      "White alone" = "A",
      "Black or African American Alone" = "B",
      "American Indian and Alaska Native Alone" = "C",
      "Asian Alone" = "D",
      "Native Hawaiian and Other Pacific Islander Alone" = "E",
      "Some Other Race Alone" = "F",
      "Two or More Races" = "G",
      "White Alone, Not Hispanic or Latino" = "H",
      "Hispanic or Latino" = "I"
    )
    races_input <- stringr::str_to_upper(arguments$which.races)
    arguments <- arguments[which(names(arguments) != "which.races")]

    if (races_input[1] == "ALL") {
      races <- races
    } else {
      races <- races[which(races_input %in% races)]
    }

    # this could be cleaned up a bit but its meant to preserve the names of races for use in row_binding in the future_map_dfr
    yr_l <- purrr::cross_df(list("races" = races, "years" = years))
    race_tibble <- tibble::tibble(race_code = races, race_name = names(races))
    race_set_names <- suppressMessages(tibble::tibble(race_code = yr_l$races) %>% left_join(race_tibble) %>% pull(race_name))

    race_named_vec <- yr_l$races %>% purrr::set_names(race_set_names)

    df <-  furrr::future_map2_dfr(.x = race_named_vec,
                           .y = yr_l$years,
                           .progress = T,
                           .id = c("race"),
                           .f = function(x,y){
                             arguments$table <- paste0(arguments$table, x)
                             arguments$year <- y
                             x <- do.call(tidycensus::get_acs, args = arguments)
                             x$year <- as.integer(y)
                             x
                           })

  }
  # this joins the variable strings to the downloaded table and give the column describing the geography a sensible name
  load("./data/acsvars.rda")
  acsvars <- acsvars %>% filter(year %in% !! arguments$year, survey == !! arguments$survey)

  df2 <- janitor::clean_names(df)
  df3 <- left_join(df2, acsvars, by = c("variable", "year"))
  df4 <- select(df3, -variable, -moe) %>%
    select(matches("[^(end_year|survey)]"), survey, year)
  if (arguments$geography == "county"){
    df5 <- tidyr::separate(df4, name, into = c("county", "state"), sep = ", ")
  } else {
    df5 <- rename(df4,!! arguments$geography := name)
  }
  df5
}

# debugonce(get_acs_combo)
# xx <- get_acs_combo(table = race_tables[1], year = 2017:2018, geography = "state", state = c("UT", "VT"),
#               survey = "acs5", which.races = "all")
#
# xx %>% select(year, state, race) %>% map(unique)
#
# check_time <- function(x){
#   start <- Sys.time()
#   out <- eval(x)
#   end <- Sys.time()
#   runtime <- end - start
#   print(runtime)
#   out
# }
#
# race_tables <- table_table %>% filter(is_race_table, !is_pr_table) %>% pull(table_name)
# non_race_tables <- table_table %>% filter(!is_race_table, !is_pr_table) %>% pull(table_name)
# # no_race_tables <- table_table %>% filter(!is_race_table, !is_pr_table) %>% pull(table_name)

