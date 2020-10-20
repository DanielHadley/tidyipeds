
download_ipeds <- function(survey, years = NULL) {

  pkg_path <- system.file(package = "tidyipeds")

  if(!fs::dir_exists(fs::path(pkg_path, "ipeds_data"))) fs::dir_create(fs::path(pkg_path, "ipeds_data"))

  ipeds_path <- fs::path(pkg_path, "ipeds_data")

  ipeds <- scrape_ipeds_datacenter_files()


  ipeds <- ipeds %>%
  dplyr::mutate(survgroup = stringr::str_remove(data_file, as.character(year)) %>%
    stringr::str_remove(year_to_fiscal(year))) %>%
  dplyr::mutate(survgroup = dplyr::case_when(
    stringr::str_detect(data_file, "GR200_") ~ "GR200",
    TRUE ~ survgroup
  )) %>%
    dplyr::mutate(survgroup = stringr::str_to_lower(survgroup))

  if(!is.null(years)) {
    ipeds <- ipeds %>%
      dplyr::filter(year %in% years)
  }

  ipeds %>%
    dplyr::filter(survgroup == !!survey) %>%
    split(.$data_file) %>%
    purrr::walk(function(ip) {

      temp <- fs::path(tempdir(), "ipeds", ip$data_file)

      fs::dir_create(temp)

      cat("\n", crayon::magenta(crayon::bgWhite(ip$data_file)), "\n")


      tempdata <- paste0(temp, "/data.zip")
      temphelp <- paste0(temp, "/help.zip")

      while(!fs::file_exists(tempdata)) {
        tryCatch(utils::download.file(ip$data_url, tempdata, mode = "wb"), error = function(e) {
          cat(crayon::white(crayon::bgRed("Error, retrying download...\n")))
          message(e)
          fs::file_delete(fs::dir_ls(temp))
        })
      }

      utils::unzip(tempdata, exdir = temp)

      is_provisional = !any(fs::dir_ls(temp) %>% stringr::str_detect("_rv|_RV")) #_rv means it's revised, ie final

      file <- if(!is_provisional) fs::dir_ls(temp, glob = "*_rv.csv|*_RV.csv") else fs::dir_ls(temp, glob = "*.csv")

      if(stringr::str_detect(stringr::str_to_lower(basename(file)), "^hd")) {
        csv <- suppressMessages(readr::read_delim(file, delim = ",", escape_double = F, trim_ws = T, guess_max = 10000, na = c("", "NA", ".")))
      } else {
        csv <- suppressMessages(readr::read_csv(file, guess_max = 10000, na = c("", "NA", ".")))
      }

      rds_name <- file %>%
        basename() %>%
        stringr::str_remove_all("_rv|_RV") %>%
        stringr::str_replace(".csv", ".rds")

      cat("\n", crayon::red("Writing compressed file:", rds_name, "\n"))

      csv %>% readr::write_rds(fs::path(ipeds_path, rds_name), compress = "gz")

      cat("\n", crayon::green("Adding help file...", "\n"))

      utils::download.file(ip$help_url, temphelp, mode = "wb")
      while(!fs::file_exists(temphelp)) {
        tryCatch(utils::download.file(ip$help_url, temphelp, mode = "wb"), error = function(e) {
          cat(crayon::white(crayon::bgRed("Error, retrying download...\n")))
          message(e)
          fs::file_delete(fs::dir_ls(temp))
        })
      }

      utils::unzip(temphelp, exdir = fs::path(ipeds_path, "helpfiles"))

      fs::dir_delete(temp)

    })
}



scrape_ipeds_datacenter_files <- function() {

  cli::cli_alert_info("Updating local list of IPEDS Datacenter files...")

  pb <- progress_bar$new(total = 100, show_after = 0)
  pb$tick(0)

  url1 <- "https://nces.ed.gov/ipeds/datacenter/login.aspx?gotoReportId=8"
  url2 <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx"

  UA <- "Mozilla/5.0 (Windows NT 6.1; rv:75.0) Gecko/20100101 Firefox/75.0"

  html <- httr::GET(url1, httr::user_agent(UA))
  pb$update(2/10)
  html <- httr::GET(url2, httr::user_agent(UA))
  pb$update(4/10)
  page <- html %>% xml2::read_html()
  pb$update(6/10)


  form <- list(
    `__VIEWSTATE` = page %>%
      rvest::html_node(xpath = "//input[@name='__VIEWSTATE']") %>%
      rvest::html_attr("value"),
    `__VIEWSTATEGENERATOR` = page %>%
      rvest::html_node(xpath = "//input[@name='__VIEWSTATEGENERATOR']") %>%
      rvest::html_attr("value"),
    `__EVENTVALIDATION` = page %>%
      rvest::html_node(xpath = "//input[@name='__EVENTVALIDATION']") %>%
      rvest::html_attr("value"),
    `ctl00$contentPlaceHolder$ddlYears` = "-1",
    `ddlSurveys` = "-1",
    `ctl00$contentPlaceHolder$ibtnContinue.x` = sample(50, 1),
    `ctl00$contentPlaceHolder$ibtnContinue.y` = sample(20, 1)
  )

  Headers <- httr::add_headers(
    `Accept-Encoding` = "gzip, deflate, br",
    `Accept-Language` = "en-GB,en;q=0.5",
    `Connection` = "keep-alive",
    `Host` = "nces.ed.gov",
    `Origin` = "https://nces.ed.gov",
    `Referer` = url2,
    `Upgrade-Insecure-Requests` = "1"
  )

  Cookies <- httr::set_cookies(stats::setNames(
    c(httr::cookies(html)$value, "true"),
    c(httr::cookies(html)$name, "fromIpeds")
  ))

  Result <- httr::POST(url2, body = form, httr::user_agent(UA), Headers, Cookies)

  pb$update(8/10)

  ipeds_scrape_raw <- Result %>%
    xml2::read_html() %>%
    rvest::html_node("#contentPlaceHolder_tblResult") %>%
    rvest::html_table() %>%
    dplyr::as_tibble()

  ipeds <- ipeds_scrape_raw %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      title = stringr::str_remove_all(title, "[^[:ascii:]]"),
      title = stringi::stri_encode(title, "", "ASCII")
    ) %>%
    dplyr::mutate(
      title = stringr::str_replace_all(title, "\\n", " "),
      title = stringr::str_remove_all(title, "\\r")
    ) %>%
    dplyr::mutate(rv_title = stringr::str_detect(title, "revised")) %>%
    dplyr::mutate(title = stringr::str_remove(title, "\\s?\\(?revised.*\\)?$")) %>%
    dplyr::mutate(title = stringr::str_squish(title)) %>%
    dplyr::filter(!stringr::str_detect(data_file, "FLAGS")) %>%
    dplyr::mutate(
      data_url = paste0("https://nces.ed.gov/ipeds/datacenter/data/", data_file, ".zip"),
      help_url = paste0("https://nces.ed.gov/ipeds/datacenter/data/", data_file, "_Dict.zip")
    ) %>%
    dplyr::select(-programs, -dictionary, -stata_data_file)

  pb$update(1)

  cli::cli_alert_success("Update complete.")

  return(ipeds)
}

update_available_ipeds <- function(force = FALSE) {

  pkg_path <- system.file(package = "tidyipeds")

  if(!fs::dir_exists(fs::path(pkg_path, "ipeds_data"))) fs::dir_create(fs::path(pkg_path, "ipeds_data"))

  ipeds_path <- fs::path(pkg_path, "ipeds_data")

  if(fs::file_exists(fs::path(ipeds_path, "datacenter_scrape.rds")) & !force) {

    existing_date <- fs::file_info(fs::path(ipeds_path, "datacenter_scrape.rds"))$modification_time
    if(difftime(lubridate::now(), existing_date, units = "weeks") > 4) {
      cli::cli_alert_warning("The locally stored list of available IPEDS data files is over 1 month old.")
      ipeds <- scrape_ipeds_datacenter_files()
      readr::write_rds(ipeds, fs::path(ipeds_path, "datacenter_scrape.rds"))
      cli::cli_alert_success("Update completed.")

    } else {

      cli::cli_alert_info("Using locally stored list of available IPEDS data files as it was updated within the last month. To force an update, re-run the function with `force = TRUE`")

    }
  } else {

    ipeds <- scrape_ipeds_datacenter_files()
    readr::write_rds(ipeds, fs::path(ipeds_path, "datacenter_scrape.rds"))

  }

}













