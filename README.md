
<!-- badges: start -->
<!--![GitHub release (latest by date)](https://img.shields.io/github/v/release/timonelmer/dena)
![GitHub Release Date](https://img.shields.io/github/release-date/timonelmer/dena) -->
![GitHub issues](https://img.shields.io/github/issues-raw/timonelmer/dena)
![GitHub All Releases](https://img.shields.io/github/downloads/timonelmer/dena/total)
<!-- [![Codecov test coverage](https://codecov.io/gh/timonelmer/dena/branch/master/graph/badge.svg)](https://codecov.io/gh/timonelmer/dena?branch=master) -->
<!-- badges: end -->

# dena
`dena` is R package that aids in preprocessing, modeling, and visualization of categorical timeseries data and egocentric social network dynamics.

## Installation

The latest version of `dena` can be installed from source using `devtools`:

```r
devtools::install_github("timonelmer/dena")
```

## Setup 

`EnergyAware` is a very simple package that computes the energy consumption of a particular R task based on the CPU's power consumption (i.e., Thermal Design Power; TDP) over its active timeperiod. For this, you need to let R know what CPU model you are running the R task on or how much TDP your CPU model uses. This can be achieved in two ways:

#### Use

```r
library(dena)
```

