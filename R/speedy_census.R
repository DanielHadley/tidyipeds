# To Do on acsvars: 2009 acsvars has some problems:
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


#' @importFrom glue glue
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr



# gives list of levels of variables in table, tehe input might be better to come from acsvars and not the downloaded table.
var_levels <- function(table, year, survey, race.iteration = F){
  if(survey == "acs5") load("./data/acsvars_acs5.rda")
  if(survey == "acs1") load("./data/acsvars_acs1.rda")
  env <- environment()
  if(exists("acsvars_acs5")) {
    acsvars <- acsvars_acs5
    rm(acsvars_acs5)
  } else if (exists("acsvars_acs1")) {
    acsvars <- acsvars_acs1
    rm(acsvars_acs1)
    }
  xun <- acsvars %>%
    filter(table_num == {{table}}, year == {{year}}, survey == {{survey}}) %>%
    {if(T){  # the if is just bc for some reason it made the return(.) work
      assign("table_concept", filter(., is_race_table_var == F) %>% pull(concept) %>% head(1) %>% str_to_upper(), envir = env)
      return(.)}
      } %>%
      filter(., is_race_table_var == race.iteration) %>% # makes sure to get the right variable set bc race and non race tables differ sometimes
    {if(T){
      assign("race_message_flag", any(.$has_race_versions), envir = env)
      return(.)}
    }%>%
    distinct(levlab, level)

  xun <- xun %>% filter(str_detect(str_to_lower(str_squish(levlab)), "^total$", negate = T))

  lev_split <-split(xun, factor(xun$level))
  names(lev_split) <- paste0("var", 1:length(lev_split))

  u_vars <- purrr::map(lev_split, ~unique(.$levlab))

  if(race_message_flag & isFALSE(race.iteration)) {
    race_table_mesage <- "\n\nThis table also has iterations for distinct racial groups.\nTo see the variable levels for this group of tables specify `race.iteration = TRUE`"
    }else{race_table_mesage <- ""
    }

  message(glue("TABLE CONCEPT: {table_concept}"),"\nChoose which values of each variable you'd like to include, for example:", "\n", glue("widen_vars(.data, var1 ='{u_vars$var1[1]}', var2 = c('{u_vars$var2[1]}', '{u_vars$var2[2]}'), etc.)"), race_table_mesage)
  print(u_vars)
  invisible(xun)
}

# debugonce(var_levels)
# var_levels("B06009", 2018, "acs5", F)
#remember to figure out wether to group by county or state
# if you want to get a 'total' for a certain level (eg. values for all females of any age (in an 'sex by age' dataset ) then specify "females" in the appropriate variable arguement)
widen_vars <- function(x, col, ...){
  # TO DO: unnest column from joined data set; do the varx filtering (i.e. what this func is made for); then sum together the values; then allow them to be put into a new column which the user can specify the name of as one of the argument functions

  # this could cause problems that I cant forsee. It sets level 1 (i.e. total) to NA so that var1 doesn't always have to be specified. If you change it you'll also need to modify the "into" arg for the seperate call below.
  x$label <- str_remove(x$label, "Estimate[:space:]*!![:space:]*Total[:space:]*(!!)*") %>% na_if("")

  args <- enquos(...)
  ex_args <- unname(purrr::imap(args, function(expr, name) quo(!!sym(name)%in%!!expr)))

  # then you need to split up the label so that it is essentially tidy format for filtering.
  x <- tidyr::separate(x, col = "label", into = paste0("var", 1:(max(x$level)-1)), sep = "!!")
  x %>% filter(!!!ex_args)
}



ipeds_join <- function(data, join_col, table, year, geography, use.parallel = T){
  jargs <- as.list(environment(), all.names = F)
  fips_vec <- data %>% pull(join_col) %>% unique()

  # get a vector of places that need to be queried. on 03/14/20 this has limited functionality since
  # it doesnt account for use case in which some one is joining data on a by county level but wants state data for the states
  # in which those counties reside. Maybe create seperate params for joining geography and data geography (eg: joining is on county geoids but the data is at the state level).
  if(geography == "state" && max(nchar(fips_vec)) == 2){
    jargs$state <- fips_vec
  } else if (geography == "county" && max(nchar(fips_vec == 5))) {
    # jargs$state <- substr(fips_vec, 1, 2) %>% unique() %>% as.integer()
    # jargs$county <- substr(fips_vec, 3,5) %>% unique() %>% as.integer()
    jargs$county <- fips_vec
  }

  jargs <- jargs[which(!(names(jargs) %in% c("join_col", "data")))]
  acs_data <- do.call(get_acs_combo, args = jargs)

  acs_data2 <- acs_data %>% tidyr::nest({{table}} := -c(geoid, year))

  joined_data <- left_join(data, acs_data2, by = c(setNames("geoid", join_col), "year"))
}
