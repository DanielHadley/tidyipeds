scrape_ipeds_datacenter_files <- function() {
  url1 <- "https://nces.ed.gov/ipeds/datacenter/login.aspx?gotoReportId=8"
  url2 <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx"

  UA <- "Mozilla/5.0 (Windows NT 6.1; rv:75.0) Gecko/20100101 Firefox/75.0"

  html <- httr::GET(url1, httr::user_agent(UA))
  html <- httr::GET(url2, httr::user_agent(UA))
  page <- html %>% xml2::read_html()


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

  return(ipeds)
}
