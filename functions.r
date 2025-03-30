read_garmin_kml <- function(files) {
  data <- lapply(
    files,
    function(file) {
      path <- sf::st_read(file, "Track Points")
      geometry <- sf::st_cast(
        sf::st_combine(path$geometry),
        "LINESTRING"
      )

      details <- sf::st_read(file, "Laps")$Description[[1]] |>
        rvest::read_html() |>
        rvest::html_table(na.strings = "") |>
        getElement(1) |>
        na.omit()
      df <- as.data.frame(as.list(setNames(details$X2, details$X1)))
      colnames(df)[1] <- "date"
      df$geometry <- geometry
      df
    }
  )
  data_merged <- dplyr::bind_rows(data)
  data_merged$date <- gsub(
    "^.+([A-Z]{3}) ([0-9]{2}) ([0-9]{2}:[0-9]{2}:[0-9]{2}) ([a-z]{3}) ([0-9]{4}).+$",
    "\\5-\\1-\\2 \\3",
    data_merged$date,
    ignore.case = TRUE
  )
  data_merged$date <- as.POSIXct(strptime(data_merged$date, "%Y-%b-%d %H:%M:%S"))
  sf::st_as_sf(data_merged, crs = 4326)
}



read_manual_kml <- function(files) {
  # exported from: https://www.plotaroute.com/routeplanner
  data <- lapply(
    files,
    function(file) {
      date <- gsub("^.+([0-9]{4}-[0-9]{2}-[0-9]{2}).+$", "\\1", file)
      path <- sf::st_read(file)
      details <- data.frame(date = as.POSIXct(date))
      details$geometry <- path$geometry[1]
      details
    }
  )
  data_merged <- dplyr::bind_rows(data)
  sf::st_as_sf(data_merged, crs = 4326)
}