# 🌆 NYC Air Quality Dashboard

> Interactive Shiny application for exploring PM2.5 concentration trends, spatial patterns, and congestion-zone proximity across New York City and surrounding counties.  
> **Course:** SYSEN 5460 | **Authors:** Chenchong Xia, Luobing Wang

---

## ✨ Features
- **Multi-tab Navigation**: Overview, Temporal Analysis, and Spatial Analysis
- **Dynamic Filtering**: Select years (2018, 2019, 2024, 2025) and counties interactively
- **Key Metrics**: Mean PM2.5, Standard Error, and observation count with responsive UI cards
- **Temporal Insights**: Hourly time series & monthly trend with linear regression + 95% CI
- **Spatial Analytics**: 
  - Proximity analysis (congestion zone vs. remote areas)
  - Interpolation diagnostics (spatial heterogeneity & correlation)
  - Hotspot identification (mean + 1 SD threshold)
  - Regression scatter plot (Distance vs. PM2.5)
  - Interactive Leaflet map with CartoDB basemap

---

## 📦 Prerequisites
### Software
- R (≥ 4.1.0 recommended)
- RStudio (optional, but recommended for development)

### R Packages
All dependencies are standard CRAN packages. Install them with:
```r
install.packages(c(
  "shiny", "dplyr", "readr", "ggplot2", "plotly", "DT", 
  "broom", "lubridate", "bslib", "bsicons", "sf", "leaflet"
), dependencies = TRUE)
