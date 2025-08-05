
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
  # To run this code you need ggplot2 and ggforce packages installed and loaded
  # My function subset is needed to run

  # if(team %in% c("St. Louis Blues", "STL", "19", 19)){
  #   blues.rink.plot <- readRDS("Load/blues.rink.plot.rds")
  #   return(blues.rink.plot)
  # }

  # Direct to and read the .csv files
  rink.plot <- readRDS("Load/rink.plot.rds")

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
#' @arg rink.unprocessed table of required data that provides the structure for the ice rink plot
#'
#' @return rink.frame
#' @export
#'
#' @examples
#' rink.processing()
rink.processing <- function(save=FALSE, rink.unprocessed=NA){
  # Used to process rink.unprocessed.csv
  # Direct to and read the .csv files
  if(!is(rink.unprocessed, "dataframe")){
    rink.unprocessed <- read.csv("Load/rink.unprocessed.csv")
  }

  rink.y <- 90
  rink.x <- (200/85)*90
  rink.scale <- 10
  # rink.unprocessed[,"size"] <- ((rink.unprocessed[,"size"]*1.333333) * 25.4)/rink.scale

  rink.unprocessed[,"size"] <- (rink.unprocessed[,"size"] * 25.4)/rink.scale

  rink.unprocessed[which(rink.unprocessed[,"geom"]=="segment"), "size"] <- (rink.unprocessed[which(rink.unprocessed[,"geom"]=="segment"), "size"]/0.75)*1

  rink.unprocessed[which(rink.unprocessed[,"geom"]=="curve"), "size"] <- (rink.unprocessed[which(rink.unprocessed[,"geom"]=="curve"), "size"]/0.75)*1

  endzone.faceoff.segments <- subset.object(subset.object(rink.unprocessed, "endzone.faceoff", "element"), "segment", "geom")

  faceoff.circle <- subset.object(subset.object(rink.unprocessed, "endzone.faceoff", "element"), "circle", "geom")
  faceoff.top.line.x <- 2.875
  # use half the distance between top two lines as one side and radius of face off circle for the other two sides
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

  rink.unprocessed <- rbind(subset.object(rink.unprocessed, "endzone.faceoff", "element", remove=TRUE), faceoff.circle, endzone.faceoff.segments)

  # Mirror the elements over the respective axis
  dir <- c("x", "y")
  for(a in 1:length(dir)){
    rink.unprocessed.inverse.dir <- rink.unprocessed[rink.unprocessed[, paste0("inverse.", dir[a])],]
    rink.unprocessed.inverse.dir[, c(dir[a], paste0(dir[a], "end"), "curvature")] <- rink.unprocessed.inverse.dir[, c(dir[a], paste0(dir[a], "end"), "curvature")]*(-1)
    rink.unprocessed <- rbind(rink.unprocessed, rink.unprocessed.inverse.dir)
  }

  rink.frame <- rink.unprocessed

  if(save){
    write.csv(rink.frame, "./rink.csv")
  }
  return(rink.frame)
}


#' Rink plot
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
  # if(!is(rink.frame, "dataframe")){
    rink.frame <- rink.processing()
  # }

  transparancy <- 1
  rink.y <- 90
  rink.x <- (200/85)*90
  rink.scale <- 10

  # Generate blank plot bound by the rink dimensions (acknowledge the size, color, and fill arguments)
  rink.plot <- ggplot() +
    theme_void() +
    scale_size_identity() +
    scale_color_identity() +
    scale_fill_identity() +
    scale_x_continuous(limits=c(-(rink.x/2), (rink.x/2)), breaks=seq(-100, 100, by=25), expand=expansion(mult=c(0.01, 0.01))) +
    scale_y_continuous(limits=c(-(rink.y/2), (rink.y/2)), breaks=seq(-42.5, 42.5, by=25), expand=expansion(mult=c(0.01, 0.01)))

  layers <- uuln(rink.frame[,"layer"]) %>%
    .[order(.)]
  for(b in 1:length(layers)){
    if(layers[b]==4){
      transparancy <- 1
    }
    rink.frame.layer <- subset.object(rink.frame, layers[b], "layer")
    # Add curves to the plot
    rink.frame.layer.curve <- subset.object(rink.frame.layer, "curve", "geom")
    if(nrow(rink.frame.layer.curve)>0){
      for(a in 1:nrow(rink.frame.layer.curve)){
        rink.plot <- rink.plot +
          geom_curve(data=rink.frame.layer.curve[a,], alpha=transparancy, inherit.aes=FALSE, curvature=rink.frame.layer.curve[a, "curvature"],
                     aes(x=x, xend=xend, y=y, yend=yend, size=size, color=color))
      }
    }

    # Add circles to the plot
    rink.plot <- rink.plot +
      geom_circle(data=subset.object(rink.frame.layer, "circle", "geom"), alpha=transparancy, inherit.aes=FALSE,
                  aes(x0=x, y0=y, r=r, size=size, color=stage(color, after_scale=alpha(color, transparancy)), fill=fill))
    # Add segments to the plot
    rink.plot <- rink.plot +
      geom_segment(data=subset.object(rink.frame.layer, "segment", "geom"), alpha=transparancy, inherit.aes=FALSE,
                   aes(x=x, xend=xend, y=y, yend=yend, size=size, color=color))
    # print(rink.frame.layer)
  }

  rink.plot <- rink.plot + theme(plot.background=element_rect(color="white", fill="white"))

  if(save){
    # saveRDS(rink.plot, "Load/rink.plot.rds")
    saveRDS(rink.plot, "./rink.plot.rds")
  }
  return(rink.plot)
}



