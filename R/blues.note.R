#' Rink logo
#'
#' @param team teamName teamTriCode or teamId accepted (default is NA)
#'
#' @return team.logo
#' @export
#'
#' @examples
#' rink.logo("STL")
rink.logo <- function(team){
  csv.path <- system.file("extdata", "team.logos.csv", package="NHL.Rink")
  team.logos <- utils::read.csv(csv.path)
  team <- teamName.teamId.triCode(team, team.logos)$triCode
  logo <- team.logos[team.logos[,"triCode"]==team,]
  if(nrow(logo)>1){
    logo <- logo[nrow(logo), ]
  }
  logo <- rsvg::rsvg_raw(logo[,"lightLogo"])
  logo <- magick::image_read(logo)
  logo <- magick::image_fx(logo, expression = "a*0.7", channel="alpha")
  raster <- grDevices::as.raster(logo)
  grob <- grid::rasterGrob(raster, width=grid::unit(1, "snpc"), height=grid::unit(1, "snpc"), just="centre", interpolate=TRUE)
  logo.size <- 13
  team.logo <- ggplot2::annotation_custom(ymin=-logo.size, ymax=logo.size, xmin=-logo.size, xmax=logo.size, grob=grob)
  return(team.logo)
}

#' Blues note processing
#'
#' @description Generates a frame used to plot a Blues note from simple information.
#'
#' @param save boolean switch referencing a csv save of the intermediate blues.note.csv (default is FALSE)
#' @param ... optional arguments for internal processing (unused)
#'
#' @return blues.note
#' @export
#'
#' @examples
#' blues.note.processing()
blues.note.processing <- function(save=FALSE, ...){
  args <- list(...)
  if(!exists("blues.unprocessed")){
    csv.path <- system.file("extdata", "blues.unprocessed.csv", package="NHL.Rink")
    blues.unprocessed <- utils::read.csv(csv.path)
    # blues.unprocessed <- utils::read.csv("inst/extdata/blues.unprocessed.csv")
  }

  direction <- c("x", "y")
  for(b in seq_len(length(direction))){
    calculate <- which(is.na(blues.unprocessed[, direction[b]]))
    for(a in seq_len(length(calculate))){
      blues.unprocessed[calculate[a], direction[b]] <- blues.unprocessed[calculate[a]-1, direction[b]] + blues.unprocessed[calculate[a]-1, paste0(direction[b], ".distance")]
    }
  }

  curve.rows <- which(blues.unprocessed[,"geom"]=="curve")
  x1 <- blues.unprocessed[curve.rows[2], "x"]
  y1 <- blues.unprocessed[curve.rows[2], "y"]
  x2 <- blues.unprocessed[curve.rows[7], "x"]
  y2 <- blues.unprocessed[curve.rows[7], "y"]
  curve.equation.y <- bqutils::curve.equation("y", x1, y1, x2, y2, 0.07)[[1]]
  curve <- eval(parse(text=curve.equation.y))
  for(a in 2:7){
    blues.unprocessed[curve.rows[a], "x"] <- curve(blues.unprocessed[curve.rows[a], "y"])
  }

  # Calculate slope
  point.1 <- which(blues.unprocessed[,"point"]==1)
  point.2 <- which(blues.unprocessed[,"point"]==2)
  general.slope <- bqutils::slope(c(blues.unprocessed[point.1, "x"], blues.unprocessed[point.1, "y"]),
                                  c(blues.unprocessed[point.2, "x"], blues.unprocessed[point.2, "y"]))

  equations <- bqutils::uuln(blues.unprocessed[, "equation"])
  linear.equation.names <- bqutils::remove.na(equations[stringr::str_detect(equations, "linear")])
  linear.equations <- rep(list(NA), length(linear.equation.names))
  names(linear.equations) <- linear.equation.names
  for(a in seq_len(length(linear.equations))){
    point.row <- which(blues.unprocessed[, "point"]==as.numeric(stringr::str_remove(linear.equation.names[a], "linear ")))
    linear.equations[[a]] <- bqutils::linear.equation(x=blues.unprocessed[point.row, "x"], y=blues.unprocessed[point.row, "y"], slope=general.slope)
  }

  linear.rows <- which(stringr::str_detect(blues.unprocessed[, "equation"], "linear ")==TRUE)
  for(a in seq_len(length(linear.rows))){
    equation <- linear.equations[[blues.unprocessed[linear.rows[a], "equation"]]]
    x.y <- bqutils::linear.equation(blues.unprocessed[linear.rows[a], "x"], blues.unprocessed[linear.rows[a], "y"], general.slope, equation)
    blues.unprocessed[linear.rows[a], "x"] <- x.y[[1]]
    blues.unprocessed[linear.rows[a], "y"] <- x.y[[2]]
  }

  for(a in seq_len(nrow(blues.unprocessed))){
    if(a!=nrow(blues.unprocessed)){
      blues.unprocessed[a, "x.end"] <- blues.unprocessed[(a+1),"x"]
      blues.unprocessed[a, "y.end"] <- blues.unprocessed[(a+1),"y"]
    }else{
      blues.unprocessed[a, "x.end"] <- blues.unprocessed[1,"x"]
      blues.unprocessed[a, "y.end"] <- blues.unprocessed[1,"y"]
    }
  }

  curve.rows <- which(blues.unprocessed[, "geom"]=="curve")[c(2:4)]
  for(a in seq_len(length(curve.rows))){
    blues.unprocessed[curve.rows[a], "x.end"] <- blues.unprocessed[curve.rows[a]+1, "x.end"]-1.75
  }

  blues.note <- blues.unprocessed

  if(save){
    utils::write.csv(blues.note, "./blues.note.csv")
  }

  return(blues.note)
}

#' Blues note plot
#'
#' @description Generates a plot with a blues note or a rink with a blues note.
#'
#' @param rink boolean switch referencing the inclusion of the blues note in a rink plot (default is FALSE)
#' @param save boolean switch referencing an object save of blues.rink.plot.rds or blues.note.plot.rds (default is FALSE)
#'
#' @return blues.plot
#' @export
#'
#' @examples
#' blues.note.plot()
blues.note.plot <- function(rink=FALSE, save=FALSE){
  blues.note <- blues.note.processing()

  if(rink){
    direction <- c("x", "y")
    for(a in seq_len(length(direction))){
      max <- max(blues.note[, direction[a]])
      min <- min(blues.note[,direction[a]])
      shift <- (min + ((max-min)/2))
      if(direction[a]=="y"){
        shift <- shift+2
      }
      blues.note[, direction[a]] <- blues.note[, direction[a]] - shift
      blues.note[, paste0(direction[a], ".end")] <- blues.note[, paste0(direction[a], ".end")] - shift
    }

    transparancy <- 0.7
    blues.plot <- rink.plot()
  }else{
    transparancy <- 1
    blues.plot <- ggplot2::ggplot() + ggplot2::theme_void()
  }

  size=1

  # Add logo segments to the plot
  blues.plot <- blues.plot +
    ggplot2::geom_segment(data=bqutils::subset.object(blues.note, "segment", "geom"), alpha=transparancy,
                          ggplot2::aes(x=x, xend=x.end, y=y, yend=y.end), size=size, lineend="round")

  blues.note.curve <- bqutils::subset.object(blues.note, "curve", "geom")
  # Add logo curves to the plot
  for(a in seq_len(nrow(blues.note.curve))){
    blues.plot <- blues.plot +
      ggplot2::geom_curve(data=blues.note.curve[a,], alpha=transparancy, inherit.aes=FALSE, curvature=-blues.note.curve[a, "r"],
                          angle=blues.note.curve[a, "angle"], lineend="round", size=size,
                          ggplot2::aes(x=x, xend=x.end, y=y, yend=y.end))
  }

  if(save & rink){
    saveRDS(blues.plot, "./blues.rink.plot.rds")
  }else if(save & !rink){
    saveRDS(blues.plot, "./blues.note.plot.rds")
  }

  return(blues.plot)
}
