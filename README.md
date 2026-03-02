# Chemical Space Plot Generator

An interactive R Shiny application for generating Chemical Space Plots (CSPs) — graphical tools that visualize the partitioning properties of chemicals across air, water, and organic matter.

## Overview

CSPs illustrate how chemicals distribute between environmental media based on three fundamental partition ratios: K<sub>AW</sub> (air–water), K<sub>OW</sub> (octanol–water), and K<sub>OA</sub> (octanol–air). This app eliminates the manual effort of recalculating threshold lines when compartment volumes change.

## Features

- **Multiphase Plot** — log K<sub>AW</sub> vs log K<sub>OW</sub> with customizable compartment volumes and mole fraction thresholds
- **Atmospheric Plot** — log K<sub>AW</sub> vs log K<sub>OA</sub> for atmospheric chemistry applications
- **Custom Data Overlay** — upload a CSV of compounds (only 2 of 3 partition ratios required; the third is auto-calculated)
- **Threshold Indicators** — centroid arrows, triangular, and curved indicator options
- **Flexible Aesthetics** — adjustable colours, line weights, labels, axis ranges, and shading
- **Export** — download publication-ready plots as PNG or SVG

## Getting Started

### Requirements

R packages: `shiny`, `dplyr`, `ggplot2`, `ggrepel`, `ragg`, `shinyjs`, `colourpicker`, `shinycssloaders`

### Run

```r
# Install dependencies
install.packages(c("shiny", "dplyr", "ggplot2", "ggrepel", "ragg", "shinyjs", "colourpicker", "shinycssloaders"))

# Launch the app
shiny::runApp()
```

## CSV Template

Upload a CSV with the following columns:

| Compound | logKow | logKoa | logKaw | Group |
|----------|--------|--------|--------|-------|

Only two of the three partition ratio columns are required per compound. The `Group` column is optional and enables categorical colouring.

## References

1. S.C. Lee, D. Mackay, *Environ. Sci. Technol.* 14 (1995) 1839–1846.
2. T. Gouin, D. Mackay, E. Webster, F. Wania, *Environ. Sci. Technol.* 34 (2000) 881–884.

## Author

Eric Wootton
