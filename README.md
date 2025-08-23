
## **NHL.Rink**

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

### **NHL rink**

``` r
scale <- rink.scale("in", height=3) 
blank.rink <- rink(scale=scale)
rink.save(blank.rink, scale=scale, file.name="nhl.rink.png")
```

### **Mammoth rink**

``` r
rink.save(rink("UTA", scale=scale), scale=1, file.name="mammoth.rink.png")
```

### **Calculated blues rink**

``` r
rink.save(blues.note.plot(rink=TRUE), scale=rink.scale("mm", height=85), file.name="calculated.blues.rink.png")
```

<!-- ### **Choose a team** -->
<!-- ```{r} -->
<!-- NHL.teams()[c(26, 22, 31),]  # Use NHL.teams() for full list -->
<!-- ``` -->
<!-- --- -->

------------------------------------------------------------------------

## **Functions**

### `rink.scale(unit = c("in","mm","cm","ft","px"), ...)`

Determine the scale to generate plot and dimensions to save. To ensure
the lines of the rink are the correct thickness the scale of the rink
and the save dimensions must be consistent.

- **Arguments**
  - `unit` - Desired unit to save plot (e.g ‘“in”’).
  - `...` - Must include either height, weight, or scale.
- **Returns**
  - A plot scale and the save dimensions with the desired unit.

### **Example**

#### `scale <- rink.scale(unit = "in", width = 12)`

The width of the output plots on the markdown file is 12in.

### `rink(team = NA, scale)`

Generates an NHL regulation rink plot with an optional team logo.

- **Arguments**
  - `team` - Team name, tri-code, or ID (e.g. `"UTA"`).
  - `scale` - Scale of the desired plot.
- **Returns**
  - A **ggplot** object with the rink (and logo if specified).

### **Example**

#### `rink(team = "UTA", scale = scale)`

<img src="man/figures/README-logo.rink-1.png" width="100%" />

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

### **Example**

#### `blues.note.plot(rink = TRUE, scale = scale)`

<img src="man/figures/README-blues.note.plot-1.png" width="100%" />

### `rink.save(rink, scale, file.name = "rink.png")`

Wrapper to save a rink as a png

- **Arguments**
  - `rink` — Either a rink function or plot.
  - `scale` — Scale of the desired plot.
  - `file.name` — Name of the output rink png.

### **Example**

#### `rink.save(rink(team = "SEA"), scale = 1, file.name = "kraken.logo.png")`

### `rink.logo(team)`

Queries and prepares a **grob** or **ggplot** of/with a team logo.

- **Arguments**
  - `team` — Team name, tri-code, or ID (e.g. `"SEA"`).  
- **Returns**
  - A **grob** or **ggplot**.

### **Example**

#### `rink.logo(team = "SEA")`

![](inst/extdata/kraken.png)

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
