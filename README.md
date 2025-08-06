
<!-- <div align="center"> -->

# NHL.Rink

## Installation

``` r
<!-- # From CRAN (when published) -->
<!-- install.packages("NHL.Rink") -->

# Or, install the development version from GitHub:
# install.packages("devtools")      
install.packages("darkghastful/NHL.Rink")
```

------------------------------------------------------------------------

## Quick Start

#### Load Package

``` r
library(NHL.Rink)
#> Loading required package: ggplot2
```

<!-- #### Choose a team logo for the rink -->
<!-- ```{r} -->
<!-- NHL.teams()[c(26, 22, 31),] #  Use NHL.teams() for full list -->
<!-- ``` -->
<!-- ```{r rink, fig.width=6, fig.asp=85/200} -->
<!-- # Generate an NHL regulation rink -->
<!-- rink.plot() -->
<!-- ``` -->

#### Generate a rink with a team logo in the center

``` r
rink("UTA") # use rink() for empty rink
```

<img src="man/figures/README-logo.rink-1.png" width="100%" />

#### NHL rink with a blues logo generated using calculated equations

``` r
blues.note.plot(rink=TRUE)
```

<img src="man/figures/README-blues.rink-1.png" width="100%" />

<!-- ```{r} -->
<!-- blues.note.plot() -->
<!-- ``` -->

------------------------------------------------------------------------

## Functions

### `rink(team=NA)`

Generates a plot containing an NHL regulation rink with a team logo.

- **Arguments**
  - `team` — team name, tri-code, or ID (e.g. `"STL"`).  
- **Returns**
  - A **ggplot** object with the rink (and logo if specified).

### `blues.note.plot(rink=FALSE, save=FALSE)`

Calculates the equations needed to generate a blues note on a coordinate
plot then generates a rink with the logo or the logo independently.

- **Arguments**
  - `rink` — logical; if `TRUE`, overlays the note on a rink.  
  - `save` — logical; if `TRUE`, writes `blues.note.plot.rds` or
    `blues.rink.plot.rds`.  
- **Returns**
  - A **ggplot** object of the note (and rink if `rink=TRUE`).

### `rink.logo(team)`

Fetches and prepares a team’s logo as a **grob** for annotation.

- **Arguments**
  - `team` — team name, tri-code, or ID (e.g. `"STL"`).  
- **Returns**
  - A **grob** that can be layered onto any ggplot.

------------------------------------------------------------------------

### Dependencies:

- **bqutils**
- **ggplot2** ≥ 3.5.2  
- **ggforce**  
- **rsvg**
- **magick**
- **grid**
- **stringr**
- **magrittr**
- **scales**

------------------------------------------------------------------------

## License

GPL-3.0 license © Bailey Quinn
