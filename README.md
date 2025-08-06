
# **NHL.Rink**

------------------------------------------------------------------------

## **Installation**

<!-- From CRAN (when published) -->
<!-- install.packages("NHL.Rink") -->

``` r
# Development version from GitHub:
# install.packages("devtools")      
install.packages("darkghastful/NHL.Rink")
#> Installing package into 'C:/Users/Bailey Quinn/AppData/Local/Temp/Rtmp4mTLRG/temp_libpath46dc157b5f6b'
#> (as 'lib' is unspecified)
#> Warning: package 'darkghastful/NHL.Rink' is not available for this version of R
#> 
#> A version of this package for your version of R might be available elsewhere,
#> see the ideas at
#> https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

------------------------------------------------------------------------

## **Quick Start**

### **Load the package**

``` r
library(NHL.Rink)
#> Loading required package: ggplot2
```

------------------------------------------------------------------------

<!-- ### **Choose a team** -->
<!-- ```{r} -->
<!-- NHL.teams()[c(26, 22, 31),]  # Use NHL.teams() for full list -->
<!-- ``` -->
<!-- --- -->

### **Generate a rink with a team logo.**

``` r
rink("UTA") # Use rink() for empty rink
```

<img src="man/figures/README-logo.rink-1.png" width="100%" />

------------------------------------------------------------------------

### **NHL rink with a blues logo generated using calculated equations.**

``` r
blues.note.plot(rink = TRUE)
```

<img src="man/figures/README-blues.rink-1.png" width="100%" />

------------------------------------------------------------------------

## **Functions**

### `rink(team = NA)`

Generates an NHL regulation rink plot with an optional team logo.

- **Arguments**
  - `team` — Team name, tri-code, or ID (e.g. `"STL"`).  
- **Returns**
  - A **ggplot** object with the rink (and logo if specified).

------------------------------------------------------------------------

### `blues.note.plot(rink = FALSE, save = FALSE)`

Calculates the equations for a Blues note logo and optionally overlays
it on a rink.

- **Arguments**
  - `rink` — Logical; if `TRUE`, overlays the note on a rink.  
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
