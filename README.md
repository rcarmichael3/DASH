# DASH
Drone Assisted Stream Habitat

`DASH` is an R package to summarize habitat metrics from data generated using the Drone Assisted Stream Habitat (DASH) protocol (Carmichael et al. 2019). Initially, the DASH R package is being created to summarize habitat metrics collected by on-the-ground personnel with the eventual goal of also generating metrics from drone collected orthomosaics. Habitat metrics include data describing characteristics such as large woody debris, undercut banks, channel unit size, undercut banks, etc. Below we provide the Executive Summary from the DASH protocol (Carmichael et al. 2019).

## Getting Started

To install `DASH` you can use Hadley Wickham's `devtools` package. To install and load the `devtools` packag use:
```
install.packages("devtools")
library(devtools)
```
NOTE: To use `devtools`, you may also have to download and install RTools (although you shouldn't). The latest version of RTools can be found at https://cran.r-project.org/bin/windows/Rtools/

Once `devtools` is successfully installed, use the following to install `DASH`:
```
devtools::install_github("mackerman44/DASH")
```
If you are interested in contributing to `DASH`, consider getting a GitHub account, fork this repository, clone to a local directory, modify, and send a pull request. Changes can then be reviewed and merged.

For further information on the `DASH` protocol, see:

Carmichael, R.A., M.W. Ackerman, K. See, B. Lott, T. Mackay, and C. Beasley. 2019. Drone Assisted Stream Habitat (DASH) Protocol, DRAFT. 

Enjoy!

## Executive Summary from DASH Protocol

This Drone Assisted Stream Habitat (DASH) protocol outlines procedures to collect accurate habitat data in an efficient and cost-effective manner that can be implemented across large spatial scales. Habitat attributes are collected primarily at the channel-unit (i.e., pool, riffle, run, rapid +, side channel) scale and secondarily at the reach (e.g., 100m - 1km) scale. Channel-unit scale habitat data can then later be summarized at larger scales if desired. By integrating high-resolution drone imagery, and when available, bathymetric light detection and ranging (LiDAR) data with minimal ground crew data collection, this protocol provides robust and accurate habitat data to inform habitat status and trends as well as fish-habitat modeling efforts. Ground crews delineate channel units, collect habitat attributes that cannot be obtained from remote sensing data, and collect high-resolution GPS information so that on-the-ground data is spatially explicit and easily compatible with remote sensing (e.g., drone, LiDAR) data. Data collected by ground crews can also be used to cross-validate remotely sensed data, when desired.

This protocol builds on previously developed methods for habitat sampling, and improves upon them by leveraging: 1) sub-meter global navigation satellite system (GNSS) receivers; 2) cost-effective drone imagery collection, image stitching, and photogrammetry; and 3) semi-automated data post-processing. Many of the ground crew methods used here have been adapted and simplified from the Columbia Habitat Monitoring Program (CHaMP) in an effort to increase survey repeatability and to remove potential human error. All data collection efforts are georeferenced and topologically compatible to increase repeatability of methods and data collection locations; a primary criticism of previous CHaMP survey efforts.

Another concern from previous habitat monitoring programs was the inability to extrapolate site-level data to larger (e.g., tributary, watershed) scales. With the DASH protocol, the intent is to circumvent the need to extrapolate data by collecting data for individual channel units in a rapid manner and using remote sensing technologies. During initial efforts, channel unit data will be collected at the reach scale (e.g., 3 km reaches); however, this protocol can easily be applied to larger (e.g., tributary, watershed) scales because of the speed and cost of drone imagery data collection and minimized minimal use of ground crew data collection. Habitat data acquired using this protocol can be paired with channel unit scale or larger scale fish abundance and density estimates to better elicit fish-habitat relationships. For example, estimates of capacity could be generated at any desired scale using available models (e.g., quantile regression forest [QRF] capacity models). The DASH protocol can be used for status and trends estimates of watershed health because of the ability to repeat measurements efficiently and effectively across large spatial scales. In addition, by enabling the use of drone and remote-sensing data, this protocol reduces labor; providing a cost-effective tool for habitat data collection supporting status and trend evaluation and model products to better inform habitat restoration prioritization and planning. 

