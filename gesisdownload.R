rD <- RSelenium::rsDriver(
  browser = "firefox",
  port = netstat::free_port(),
  verbose = FALSE,
  chromever = NULL,
  phantomver = NULL
)

remDr <- rD[["client"]]
try(remDr$open(silent = TRUE), silent = TRUE)

# when finished:
remDr$close()
rD[["server"]]$stop()
rm(rD, remDr)
gc()



gesis_download_fixed <- function(file_id,
                                 email = getOption("gesis_email"),
                                 password = getOption("gesis_password"),
                                 use = getOption("gesis_use"),
                                 reset = FALSE,
                                 download_dir = "gesis_data",
                                 msg = TRUE,
                                 convert = TRUE,
                                 delay = 5) {

  if (reset) {
    use <- email <- password <- NULL
  }

  if (is.null(email)) {
    email <- readline("GESIS email: ")
  }
  if (is.null(password)) {
    password <- readline("GESIS password: ")
  }
  if (is.null(use)) {
    use <- readline("GESIS use: ")
  }

  use <- dplyr::case_when(
    use == 1 ~ "for final thesis of the study programme (e.g. Bachelor/Master thesis)",
    use == 2 ~ "for reserach with a commercial mission",
    use == 3 ~ "for non-scientific purposes",
    use == 4 ~ "for further education and qualification",
    use == 6 ~ "in the course of my studies",
    use == 7 ~ "in a course as lecturer",
    TRUE     ~ "for scientific research (incl. doctorate)"
  )

  if (Sys.info()[["sysname"]] == "Linux") {
    default_dir <- file.path("/home", Sys.info()[["user"]], "Downloads")
  } else {
    default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
  }

  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

  if (msg) message("Initializing RSelenium driver")

  rD <- RSelenium::rsDriver(
    browser = "firefox",
    port = netstat::free_port(),
    verbose = FALSE,
    chromever = NULL,
    phantomver = NULL
  )

  remDr <- rD[["client"]]
  try(remDr$open(silent = TRUE), silent = TRUE)

  signin <- "https://login.gesis.org"
  remDr$navigate(signin)
  Sys.sleep(delay)

  remDr$findElement(using = "id", "username")$sendKeysToElement(list(email))
  remDr$findElement(using = "id", "password")$sendKeysToElement(list(password))
  remDr$findElement(using = "id", "kc-login")$clickElement()
  Sys.sleep(delay)

  for (item in file_id) {
    if (msg) message("Downloading GESIS Data Archive file: ", item)

    dd_old <- list.files(default_dir)

    remDr$navigate(paste0("https://search.gesis.org/research_data/", item))
    Sys.sleep(delay)

    if (try(unlist(remDr$findElement(using = "partial link text", "Englis")$getElementAttribute("id")), silent = TRUE) == "") {
      remDr$findElement(using = "partial link text", "English")$clickElement()
    }
    Sys.sleep(delay)

    if (try(unlist(remDr$findElement(using = "link text", "Codebook")$getElementAttribute("id")), silent = TRUE) == "") {
      remDr$findElement(using = "link text", "Codebook")$clickElement()
    }
    Sys.sleep(delay)

    dd_old_plus_cdbk <- list.files(default_dir)

    remDr$findElement(using = "partial link text", "Datasets")$clickElement()
    remDr$findElement(using = "class", "data_purpose")$sendKeysToElement(list(use))
    remDr$findElement(using = "partial link text", "dta")$clickElement()
    Sys.sleep(delay)

    dd_new <- setdiff(list.files(default_dir), dd_old_plus_cdbk)

    while (any(stringr::str_detect(dd_new, "\\.crdownload$"))) {
      Sys.sleep(1)
      dd_new <- setdiff(list.files(default_dir), dd_old_plus_cdbk)
    }

    dd_new  <- setdiff(list.files(default_dir), dd_old)
    dd_cdbk <- setdiff(dd_old_plus_cdbk, dd_old)
    dd_data <- setdiff(dd_new, dd_cdbk)

    dir.create(file.path(download_dir, item), showWarnings = FALSE, recursive = TRUE)

    if (length(dd_data) > 0 && stringr::str_detect(dd_data, "\\.zip$")) {
      utils::unzip(file.path(default_dir, dd_data), exdir = file.path(download_dir, item))
      unlink(file.path(default_dir, dd_data))
      if (length(dd_cdbk) > 0) {
        file.rename(file.path(default_dir, dd_cdbk), file.path(download_dir, item, dd_cdbk))
      }
    } else {
      if (length(dd_new) > 0) {
        file.rename(file.path(default_dir, dd_new), file.path(download_dir, item, dd_new))
      }
    }
  }

  invisible(TRUE)
}
