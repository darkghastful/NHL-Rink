

#' Rink Scale
#'
#' @description Determine the scale to generate plot and dimensions to save.
#'
#'
#' @param unit desired unit to save plot c("in","mm","cm","ft","px")
#' @param ... must include one of the following (height=#), (width=#), (scale=#)
#'
#' @return c(scale, height, width, unit) scale used to generate plot and the desired dimensions and unit to save the plot
#' @export
#'
#' @examples
#' rink.scale("px", height=400, dpi=300)
#' rink.scale("mm", width=100)
#' rink.scale("in", scale=2)
rink.scale <- function(unit=c("in","mm","cm","ft","px"), ...) {

  args <- list(...)

  `%||%` <- function(a, b){
    if(!is.null(a)){
      as.numeric(a)
    }else{
      b
    }
  }

  height <- args$height %||% NA
  width <- args$width %||% NA
  scale <- args$scale %||% NA
  dpi <- args$dpi %||% NA

  default.height.mm <- 8.5
  default.width.mm <- 20

  unit <- match.arg(unit)

  if(unit=="px" & is.na(dpi)){
    dpi=300
    # stop("You must provide dpi with px.")
  }

  to.mm <- switch(
    unit,
    "mm"=1,
    "cm"=10,
    "in"=25.4,
    "ft"=25.4*12,
    "px"=25.4/dpi
  )

  if(!is.na(scale)){
    final.scale <- scale
  }else if(!is.na(width)){
    width.mm <- width*to.mm
    final.scale <- width.mm/default.width.mm
  }else if(!is.na(height)){
    height.mm <- height*to.mm
    final.scale <- height.mm/default.height.mm
  }else{
    stop("You must provide either height, width, or scale.")
  }

  final.height.mm <- default.height.mm*final.scale
  final.width.mm <- default.width.mm*final.scale

  from.mm <- switch(
    unit,
    "mm"=1,
    "cm"=1/10,
    "in"=1/25.4,
    "ft"=1/(25.4*12),
    "px"=dpi/25.4
  )

  final.height <- final.height.mm*from.mm
  final.width <- final.width.mm*from.mm

  return(list(scale=final.scale, final.height=final.height, final.width=final.width, unit))
}

#' Rink
#'
#' @description Generates a plot containing an NHL regulation rink with a team logo.
#'
#' @param team teamName teamTriCode or teamId accepted (default is NA)
#' @param scale for plot generation
#'
#' @return rink.plot
#' @export
#'
#' @examples
#' rink("STL", 1)
rink <- function(team=NA, scale=1){
  # rds.path <- system.file("extdata", "rink.plot.rds", package="NHL.Rink")
  # rink.plot <- readRDS(rds.path)
  rink.plotted <- rink.plot(scale)

  if(!is.na(team)){
    logo <- rink.logo(team)
    rink.plotted$layers <- c(logo, rink.plotted$layers)
  }

  return(rink.plotted)
}


#' Rink processing
#'
#' @description Generates a frame used to plot an NHL regulation ice rink from simple information.
#'
#' @param scale for plot generation
#' @param save boolean switch referencing a csv save of the intermediate rink.frame.csv (default is FALSE)
#' @param ... optional arguments for internal processing (unused)
#'
#' @return rink.frame
#' @export
#'
#' @examples
#' rink.processing(1)
rink.processing <- function (scale=1, save=FALSE, ...){
  if(length(scale)>1){
    scale <- scale$scale
  }

  args <- list(...)
  if(!exists("rink.unprocessed")){
    csv.path <- system.file("extdata", "rink.unprocessed.csv", package="NHL.Rink")
    rink.unprocessed <- utils::read.csv(csv.path)
    # rink.unprocessed <- utils::read.csv("inst/extdata/rink.unprocessed.csv")
  }

  rink.y <- 85
  rink.x <- 200

  print(scale)


  rink.unprocessed[,"size"] <- rink.unprocessed[,"size"]*2.54

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
#' @param scale for plot generation
#' @param save boolean switch referencing an object save of rink.plot.rds (default is FALSE)
#'
#' @return rink.plot
#' @export
#'
#' @examples
#' rink.plot()
rink.plot <- function(scale=1, save=FALSE){
  if(length(scale)>1){
    scale <- scale$scale
  }

  rink.frame <- rink.processing(scale)

  transparancy <- 1
  rink.y <- 85
  rink.x <- 200

  rink.plot <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits=c(-(rink.x/2), (rink.x/2)), breaks=seq(-100, 100, by=25), expand=ggplot2::expansion(mult=c(0, 0))) +
    ggplot2::scale_y_continuous(limits=c(-(rink.y/2), (rink.y/2)), breaks=seq(-42.5, 42.5, by=1), expand=ggplot2::expansion(mult=c(0, 0))) +
    ggplot2::coord_fixed()

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
          ggplot2::geom_curve(data=rink.frame.layer.curve[a,], lineend="round", alpha=transparancy, inherit.aes=FALSE, curvature=rink.frame.layer.curve[a, "curvature"],
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
