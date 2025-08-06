#' Rink
#'
#' @description Generates a plot containing an NHL regulation rink with a team logo.
#'
#' @param team teamName teamTriCode or teamId accepted (default is NA)
#'
#' @return rink.plot
#' @export
#'
#' @examples
#' rink("STL")
rink <- function(team=NA){
  rds.path <- system.file("extdata", "rink.plot.rds", package="NHL.Rink")
  rink.plot <- readRDS(rds.path)

  if(!is.na(team)){
    logo <- rink.logo(team)
    rink.plot$layers <- c(logo, rink.plot$layers)
  }

  return(rink.plot)
}


#' Rink processing
#'
#' @description Generates a frame used to plot an NHL regulation ice rink from simple information.
#'
#' @param save boolean switch referencing a csv save of the intermediate rink.frame.csv (default is FALSE)
#' @param ... optional arguments for internal processing (unused)
#'
#' @return rink.frame
#' @export
#'
#' @examples
#' rink.processing()
rink.processing <- function (save=FALSE, ...){
  args <- list(...)
  if(!exists("rink.unprocessed")){
    csv.path <- system.file("extdata", "rink.unprocessed.csv", package="NHL.Rink")
    rink.unprocessed <- utils::read.csv(csv.path)
    rink.unprocessed <- utils::read.csv("inst/extdata/rink.unprocessed.csv")
  }

  rink.y <- 90
  rink.x <- (200/85)*90
  rink.scale <- 10

  # rink.unprocessed[,"size"] <- (rink.unprocessed[,"size"] / rink.scale)*25.4
  rink.unprocessed[,"size"] <- (rink.unprocessed[,"size"] * 25.4)/rink.scale

  rink.unprocessed[which(rink.unprocessed[,"geom"]=="segment"), "size"] <- (rink.unprocessed[which(rink.unprocessed[,"geom"]=="segment"), "size"]/0.75)*1
  rink.unprocessed[which(rink.unprocessed[,"geom"]=="curve"), "size"] <- (rink.unprocessed[which(rink.unprocessed[,"geom"]=="curve"), "size"]/0.75)*1

  endzone.faceoff.segments <- bqutils::subset.object(bqutils::subset.object(rink.unprocessed, "endzone.faceoff", "element"), "segment", "geom")

  faceoff.circle <- bqutils::subset.object(bqutils::subset.object(rink.unprocessed, "endzone.faceoff", "element"), "circle", "geom")
  faceoff.top.line.x <- 2.875
  theta <- acos(((faceoff.circle[1,"r"]^2)+(faceoff.circle[1,"r"]^2)-(2.875^2))/(2*(faceoff.circle[1,"r"]^2)))
  faceoff.top.line.y <- faceoff.circle[1,"r"]*cos(theta)

  endzone.faceoff.segments[which(endzone.faceoff.segments[,"x"]==0), c("x", "xend", "y", "yend")] <- c(2.875, 2.875, faceoff.top.line.y, (faceoff.top.line.y+2))

  dir <- c("x", "y")
  for(a in 1:length(dir)){
    endzone.faceoff.segments.inverse.dir <- endzone.faceoff.segments
    endzone.faceoff.segments.inverse.dir[, c(dir[a], paste0(dir[a], "end"))] <- endzone.faceoff.segments.inverse.dir[, c(dir[a], paste0(dir[a], "end"))]*-1
    endzone.faceoff.segments <- rbind(endzone.faceoff.segments, endzone.faceoff.segments.inverse.dir)
  }
  dir <- c("x", "y")
  for(a in 1:length(dir)){
    endzone.faceoff.segments[, c(dir[a], paste0(dir[a], "end"))] <- endzone.faceoff.segments[, c(dir[a], paste0(dir[a], "end"))] + faceoff.circle[1, dir[a]]
  }

  rink.unprocessed <- rbind(bqutils::subset.object(rink.unprocessed, "endzone.faceoff", "element", remove=TRUE), faceoff.circle, endzone.faceoff.segments)

  dir <- c("x", "y")
  for(a in 1:length(dir)){
    rink.unprocessed.inverse.dir <- rink.unprocessed[rink.unprocessed[, paste0("inverse.", dir[a])],]
    rink.unprocessed.inverse.dir[, c(dir[a], paste0(dir[a], "end"), "curvature")] <- rink.unprocessed.inverse.dir[, c(dir[a], paste0(dir[a], "end"), "curvature")]*(-1)
    rink.unprocessed <- rbind(rink.unprocessed, rink.unprocessed.inverse.dir)
  }

  rink.frame <- rink.unprocessed

  if(save){
    utils::write.csv(rink.frame, "./rink.csv")
  }
  return(rink.frame)
}


#' Rink plot
#' @importFrom magrittr %>%
#' @importFrom scales alpha
#' @importFrom ggplot2 stage
#' @importFrom ggforce geom_circle
#'
#' @description Generates a plot containing an NHL regulation ice rink.
#'
#' @param save boolean switch referencing an object save of rink.plot.rds (default is FALSE)
#'
#' @return rink.plot
#' @export
#'
#' @examples
#' rink.plot()
rink.plot <- function(save=FALSE){
  rink.frame <- rink.processing()

  transparancy <- 1
  rink.y <- 90
  rink.x <- (200/85)*90
  rink.scale <- 300
  rink.frame[,"size"] <- as.numeric(rink.frame[,"size"])

  rink.plot <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits=c(-(rink.x/2), (rink.x/2)), breaks=seq(-100, 100, by=25), expand=ggplot2::expansion(mult=c(0.01, 0.01))) +
    ggplot2::scale_y_continuous(limits=c(-(rink.y/2), (rink.y/2)), breaks=seq(-42.5, 42.5, by=25), expand=ggplot2::expansion(mult=c(0.01, 0.01)))

  layers <- bqutils::uuln(rink.frame[,"layer"]) %>%
    .[order(.)]
  for(b in 1:length(layers)){
    if(layers[b]==4){
      transparancy <- 1
    }
    rink.frame.layer <- bqutils::subset.object(rink.frame, layers[b], "layer")
    rink.frame.layer.curve <- bqutils::subset.object(rink.frame.layer, "curve", "geom")
    if(nrow(rink.frame.layer.curve)>0){
      for(a in 1:nrow(rink.frame.layer.curve)){
        rink.plot <- rink.plot +
          ggplot2::geom_curve(data=rink.frame.layer.curve[a,], alpha=transparancy, inherit.aes=FALSE, curvature=rink.frame.layer.curve[a, "curvature"],
                              ggplot2::aes(x=x, xend=xend, y=y, yend=yend, linewidth=size, color=color))
      }
    }
    # alpha("#1F77B4", 0.5)
    rink.plot <- rink.plot +
      ggforce::geom_circle(data=bqutils::subset.object(rink.frame.layer, "circle", "geom"), inherit.aes=FALSE,
                           ggplot2::aes(x0=x, y0=y, r=r, linewidth=size, color=color, fill=fill))
    rink.plot <- rink.plot +
      ggplot2::geom_segment(data=bqutils::subset.object(rink.frame.layer, "segment", "geom"), alpha=transparancy, inherit.aes=FALSE,
                            ggplot2::aes(x=x, xend=xend, y=y, yend=yend, linewidth=size, color=color))
  }

  rink.plot <- rink.plot + ggplot2::theme(plot.background=ggplot2::element_rect(color="white", fill="white"))

  if(save){
    saveRDS(rink.plot, "./rink.plot.rds")
  }
  return(rink.plot)
}
