#' @keywords internal
teamName.teamId.triCode <- function(team, object = NA) {
  if (all(is.na(object))) {
    csv.path <- system.file("extdata", "team.logos.csv", package="SLAPrink")
    object <- utils::read.csv(csv.path)
    # object <- read.csv("inst/extdata/team.logos.csv")
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

#' @keywords internal
NHL.teams <- function(){
  csv.path <- system.file("extdata", "team.logos.csv", package="SLAPrink")
  object <- utils::read.csv(csv.path)
  object <- object[, c("teamName", "teamId", "triCode")]
  return(object)
}


# object <- content.from.endpoint("https://api.nhle.com/stats/rest/en/team")$data
# object <- rename.column(object, "id", "teamId")
# object <- rename.column(object, "fullName", "teamName")
#
# for(element in 1:nrow(object)){
#   urls <- logo.endpoint(object[element, "triCode"])
#   if(length(uln(urls))==2){
#     object[element, "lightLogo"] <- urls$light
#     object[element, "darkLogo"] <- urls$light
#   }else{
#     object[element, "lightLogo"] <- urls$light[[2]]
#     object[element, "darkLogo"] <- urls$light[[2]]
#   }
#
# }
#
# for(team in 1:nrow(object)){
#   res <- try(rsvg_raw(object[team, "lightLogo"]), silent=TRUE)
#   if(inherits(res, "try-error")) {
#     object[team, "lightLogo"] <- NA
#   }
# }
# object <- object[, c("teamName", "teamId", "triCode", "lightLogo")]
# object <- remove.na(object, "lightLogo")
#
# write.csv(object, "inst/extdata/team.logos.csv", row.names=FALSE)
# read.csv("inst/extdata/team.logos.csv")
