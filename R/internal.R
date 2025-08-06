#' @keywords internal
teamName.teamId.triCode <- function(team, object = NA) {
  if (all(is.na(object))) {
    object <- content.from.endpoint("https://api.nhle.com/stats/rest/en/team")$data
    object <- bqutils::rename.column(object, "id", "teamId")
    object <- bqutils::rename.column(object, "fullName", "teamName")
  }

  if (length(stringr::str_split(team, " ")[[1]]) > 1) {
    row <- object[, "teamName"] == team
  } else if (!grepl("[^0-9]", as.character(team))) {
    row <- object[, "teamId"] == as.numeric(team)
  } else {
    row <- object[, "triCode"] == team
  }

  team <- object[row, c("teamName", "teamId", "triCode")]
  if (length(team) > 1) {
    team <- team[length(team)]
  }

  return(as.list(team))
}

#' @keywords internal
content.from.endpoint <- function(url, content.type = "application/octet-stream",
                                  accept = "application/json", fromJSON = TRUE) {
  response <- httr::VERB("GET", url, httr::content_type(content.type), httr::accept(accept))
  encoded <- httr::content(response, "text", encoding = "UTF-8")
  if (fromJSON) {
    encoded <- jsonlite::fromJSON(encoded)
  }
  return(encoded)
}
