
#' @importFrom glue glue
#' @import dplyr
#' @importFrom magrittr %>%
#' @import stringr

# get_acs(table = "B02001", geography = "county", state = c("UT"), output = "wide")


# xxx <- get_acs_combo(table = "B15001", year = 2017:2018, survey = "acs5", geography = "state", state = c("UT", "VT"))
#
#
# lablst <- xxx %>% select(level, levlab, label)

# gives list of levels of variables in table, tehe input might be better to come from acsvars and not the downloaded table.
var_levels <- function(x, col){
  #unnest data
  xun <- x %>% select(col) %>%
    tidyr::unnest(cols = col)

  xun <- xun %>% filter(str_detect(str_to_lower(str_squish(levlab)), "^total$", negate = T))

  lev_split <-split(xun, factor(xun$level))
  names(lev_split) <- paste0("var", 1:length(lev_split))

  u_vars <- purrr::map(lev_split, ~unique(.$levlab))

  message("Choose which values of each variable you'd like to include, for example:", "\n", glue("widen_vars(.data, var1 ='{u_vars$var1[1]}', var2 = c('{u_vars$var2[1]}', '{u_vars$var2[2]}'), etc.)"))
  print(u_vars)
  invisible(xun)
}



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

# debugonce(widen_vars)
# xxx %>%
# var_levels() %>%
# widen_vars(var2 = "18 to 24 years", var3 = "Less than 9th grade")
#
# widen_vars(xxx, var3 = "Less than 9th grade")
# widen_vars(xxx)

# tibble representing the data to join to (just fips codes)


xxx <- get_acs_combo(table = "B15001", year = 2017:2018, survey = "acs5", geography = "state", state = c("UT", "TX", "MA"), use.parallel = F)


tj <- tidycensus::fips_codes %>%
  filter(state %in% c("TX", "UT", "MA")) %>%
  tidyr::unite(geoid, state_code, county_code, sep = "" ) %>%
  select(geoid, state) %>% as_tibble()

ttj<- cross_df(list(geoid = tj$geoid, year = 2017:2018)) %>%
  full_join(tj) %>%
  group_by(state) %>%
  sample_frac(.2) %>%
  ungroup()

#ttj is used as a practice data set to join on.


ipeds_join <- function(data, join_col, table, year, geography, use.parallel = T){
  jargs <- as.list(environment(), all.names = F)
  fips_vec <- data %>% pull(join_col) %>% unique()

  # get a vector of places that need to be queried. on 03/14/20 this has limited functionality since
  # it doesnt account for use case in which some one is joining data on a by county level but wants state data for the states
  # in which those counties reside.
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


xx <- ipeds_join(ttj, join_col = "geoid", "B15001", year = 2017:2018, geography = "county")

xx %>%
var_levels("B15001") %>%
  widen_vars(var1 = )


# join_acs <- function(.data, table, year, ...,  join_col){
#   args <- list(...)
#   args$table <- table
#   .data %
#
# }
