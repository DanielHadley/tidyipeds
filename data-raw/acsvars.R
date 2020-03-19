## code to prepare `acsvars` dataset goes here
# To Do: 2009 acsvars has some problems:
# remove colons at the end of strings from levlab for 2009 variables in acsvars.
# Remove table numbers from concepts in acsvars for 2009
# for white alone tables in 2009 the levels are off as well

get_acs_vars <- function(years, survey) {
  # there aren't vars for acs1 for 2009, you get an error if you try
  if(survey == "acs1" & any(years %in% 2009)) stop("acs1 cannot be used with 2009")

  furrr::future_map_dfr(years, .progress = T,
                 .f = function(x, y = survey){
                   load_variables(year = x, dataset = y, cache = T) %>%
                     mutate(level = str_count(label, pattern = "!!")) %>%
                     rowwise() %>%
                     mutate(levlab = str_split(label, pattern = "!!") %>% unlist() %>% .[level + 1]) %>%
                     ungroup() %>%
                     mutate(concept = str_to_title(concept),
                            year = !! x) %>%
                     rename(variable = name)
                 })
}

# acsvars ends up containing all census variables. It's used to join
# with actual data and give you more informative table levels and descriptors for when you actually pull the data in using si_acs()
acsvars <- bind_rows("acs1" = get_acs_vars(2010:2018, "acs1"),
                     "acs5" = get_acs_vars(2009:2018, "acs5"),
                     .id = "survey")




# make a table that tells is_race_table -----------------------------------


# # make a table that give the base table name, and logical vectors to say whether it is a race table and or a pr table
# table_table <- acsvars %>% select(variable) %>%
#   # these tables give problems for some reason
#   filter(str_detect(variable,"B0000(1|2)", negate = T)) %>%
#   mutate(variable = str_sub(variable, 1, -5)) %>%
#   unique() %>%
#   mutate(is_race_table = str_detect(variable, ".{6}(A|B|C|D|E|F|G|H|I)"),
#          is_pr_table = str_detect(variable, ".*PR"),
#          table_name = str_sub(variable, 1,6)) %>%
#   group_by(table_name) %>%
#   mutate(is_race_table = any(is_race_table),
#          is_pr_table = any(is_pr_table)) %>%
#   distinct(is_race_table, is_pr_table, table_name)

usethis::use_data(acsvars)
