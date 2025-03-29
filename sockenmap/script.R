sf::sf_use_s2(FALSE)
options(stringsAsFactors = FALSE)
source("functions.R")
garmin <- list.files("socken/garmin", full.names = TRUE) |> read_garmin_kml()
manual <- list.files("socken/manual", full.names = TRUE) |> read_manual_kml()
activities <- dplyr::bind_rows(garmin, manual)

socken <- sf::st_read("sockenstad.gpkg")
deso <- sf::st_read("DeSO_2018_v2.gpkg") |> sf::st_transform(sf::st_crs(socken))
orter <- sf::st_read("Tatorter_1980_2020.gpkg") |> sf::st_transform(sf::st_crs(socken))
socken <- sf::st_transform(socken, sf::st_crs(4326))
deso <- sf::st_transform(deso, sf::st_crs(4326))
orter <- sf::st_transform(orter, sf::st_crs(4326))

# is last trip
activities <- activities |>
  dplyr::mutate(
    last = date == max(date),
    distance = sf::st_length(geometry) / 1000,
    label = sprintf(
      "date: %s<br/>distance: %1.0f km",
      as.Date(date), distance
    )
  )
activities$label <- lapply(activities$label, htmltools::HTML)

# filter socken and order just to Skåne
# orter <- dplyr::filter(orter, LANNAMN == "Skåne")
socken_centroids <- socken |>
  dplyr::filter(omradesnummer == 1) |>
  dplyr::mutate(SHAPE = sf::st_centroid(SHAPE))
# socken_centroids <- sf::st_transform(socken_centroids, sf::st_crs(4326))
socken_centroids$lannamn <- sapply(
  sf::st_within(socken_centroids, deso), function(x) {
    deso[x, ]$lannamn |>
      unique() |>
      toString()
  }
)
socken_centroids <- socken_centroids |> dplyr::filter(lannamn == "Skåne")
socken <- socken |> subset(sockenstadkod %in% socken_centroids$sockenstadkod)

# socken stats
activity_sockens <- do.call(
  "rbind",
  lapply(seq_len(nrow(activities)), function(idx) {
    activity <- activities[idx, ]
    activity_sockens <- socken[unlist(sf::st_intersects(activity, socken)), c("sockenstadkod", "sockenstadnamn")]
    activity_sockens$date <- activity$date
    as.data.frame(activity_sockens)
  })
)

socken_statistics <- activity_sockens |>
  dplyr::group_by(sockenstadkod, sockenstadnamn) |>
  dplyr::summarise(
    n = length(unique(as.Date(date))),
    last_time = max(as.Date(date))
  )

socken <- dplyr::left_join(socken, socken_statistics)
socken <- dplyr::mutate(
  socken,
  n = ifelse(is.na(n), 0, n),
  visited = n > 0,
  label = sprintf(
    "<strong>%s</strong><br/>visited count: %s<br/>last visited: %s",
    sockenstadnamn, n, last_time
  )
)
socken$label <- lapply(socken$label, htmltools::HTML)

# how many socken I've visited
socken <- socken |>
  dplyr::group_by(sockenstadkod) |>
  dplyr::mutate(visited = any(visited)) |>
  dplyr::ungroup()
is_socken_visited <- socken |>
  dplyr::group_by(sockenstadnamn) |>
  dplyr::summarise(visited = visited[1]) |>
  dplyr::group_by(visited) |>
  dplyr::summarise(n = dplyr::n())

# orter stats
activity_orter <- do.call(
  "rbind",
  lapply(
    seq_len(nrow(activities)),
    function(idx) {
      activity <- activities[idx, ]
      activity_orter <- orter[unlist(sf::st_intersects(activity, orter)), c("UUID", "TATORT", "BEF")]
      activity_orter$date <- activity$date
      as.data.frame(activity_orter)
    }
  )
)

orter_statistics <- activity_orter |>
  dplyr::group_by(UUID) |>
  dplyr::summarise(
    n = length(unique(as.Date(date))),
    last_time = max(as.Date(date))
  )

orter <- dplyr::left_join(orter, orter_statistics)
orter <- dplyr::mutate(
  orter,
  n = ifelse(is.na(n), 0, n),
  visited = n > 0,
  BEF_10K = BEF > 10000,
  BEF_5K = BEF > 5000,
  BEF_1K = BEF > 1000,
  BEF_500 = BEF > 500,
  label = sprintf(
    "<strong>%s</strong><br/>visited count: %s<br/>last visited: %s<br/>Population: %s",
    TATORT, n, last_time, BEF
  )
)
orter$label <- lapply(orter$label, htmltools::HTML)


library(leaflet)
m <- leaflet() |>
  setView(lng = 13.7439, lat = 55.9369, zoom = 8) |>
  addTiles() |>
  addPolygons(
    data = orter,
    fillColor = ~ ifelse(visited, "#2eb6ff", "#ffffff"),
    weight = 1,
    # add hover
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = FALSE
    ),
    label = ~label
  ) |>
  # add socken polygons
  addPolygons(
    data = socken,
    fillColor = ~ ifelse(visited, "grey", "white"),
    color = "black",
    weight = 1,
    # add hover
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = FALSE
    ),
    label = ~label
  ) |>
  # add old activities
  addPolylines(
    data = sf::st_zm(activities, 0),
    color = ~ ifelse(last, "red", "blue"),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#490080",
      bringToFront = TRUE
    ),
    label = ~label
  ) |>
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 200)) |>
  # add legend containing visited and not visited
  addLegend(
    title = "Is parish visited?",
    position = "bottomleft",
    colors = c("grey", "white"),
    labels = c(
      sprintf("Visited (%d)", is_socken_visited$n[is_socken_visited$visited]),
      sprintf("Not visited (%d)", is_socken_visited$n[!is_socken_visited$visited])
    )
  ) |>
  # add legend for old and last activity being over the socken legend
  addLegend(
    title = sprintf("Activities (%.2f km)", sum(sf::st_length(activities)) / 1000),
    position = "bottomright",
    colors = c("blue", "red"),
    labels = c(
      sprintf("Old (%d)", nrow(activities) - 1),
      sprintf("Last (%s)", as.Date(max(activities$date)))
    ),
    opacity = 1
  )
html_file <- "index.html"
htmlwidgets::saveWidget(m, html_file, selfcontained = FALSE)
html_content <- readLines(html_file, encoding = "UTF-8")
writeLines(html_content, html_file, useBytes = TRUE)
