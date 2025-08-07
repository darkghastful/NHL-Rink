
# **NHL.Rink**

------------------------------------------------------------------------

## **Installation**

<!-- From CRAN (when published) -->
<!-- install.packages("NHL.Rink") -->

``` r
# install.packages("devtools")      
install.packages("darkghastful/NHL.Rink")
```

------------------------------------------------------------------------

## **Quick Start**

### **Load the package**

``` r
library(NHL.Rink)
```

------------------------------------------------------------------------

### **Calculate rink scale**

``` r
# The rink scale needs to be for a plot 12 inches wide as that is the size of the markdown plot
scale <- rink.scale("in", width=12)  
```

------------------------------------------------------------------------

<!-- ### **Choose a team** -->
<!-- ```{r} -->
<!-- NHL.teams()[c(26, 22, 31),]  # Use NHL.teams() for full list -->
<!-- ``` -->
<!-- --- -->

### **Generate a rink with a team logo.**

``` r
rink("UTA", scale=scale) # Use rink() for empty rink
```

<img src="man/figures/README-logo.rink-1.png" width="100%" />

------------------------------------------------------------------------

### **NHL rink with a blues logo generated using calculated equations.**

``` r
blues.note.plot(rink = TRUE, scale=12)
```

<img src="man/figures/README-blues.rink-1.png" width="100%" />

------------------------------------------------------------------------

## **Functions**

### `rink.scale(unit=c("in","mm","cm","ft","px"), ...)`

Determine the scale to generate plot and dimensions to save.

- **Arguments**
  - `unit` — Desired unit to save plot (e.g ‘“in”’).
  - `...` - Must include either height, weight, or scale.
- **Returns**
  - A plot scale and the save dimensions with the desired unit.

------------------------------------------------------------------------

### `rink(team = NA, scale)`

Generates an NHL regulation rink plot with an optional team logo.

- **Arguments**
  - `team` — Team name, tri-code, or ID (e.g. `"STL"`).
  - `scale` - Scale of the desired plot.
- **Returns**
  - A **ggplot** object with the rink (and logo if specified).

------------------------------------------------------------------------

### `blues.note.plot(rink = FALSE, save = FALSE)`

Calculates the equations for a Blues note logo and optionally overlays
it on a rink.

- **Arguments**
  - `rink` — Logical; if `TRUE`, overlays the note on a rink.  
  - `scale` - Scale of the desired plot.
  - `save` — Logical; if `TRUE`, saves as `blues.note.plot.rds` or
    `blues.rink.plot.rds`.  
- **Returns**
  - A **ggplot** object of the note (and rink if `rink = TRUE`).

------------------------------------------------------------------------

### `rink.logo(team)`

Queries and prepares a **grob** of a team logo.

- **Arguments**
  - `team` — Team name, tri-code, or ID (e.g. `"STL"`).  
- **Returns**
  - A **grob** that can be layered onto any ggplot.

------------------------------------------------------------------------

## **Dependencies**

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

## **License**

GPL-3.0 license © Bailey Quinn
