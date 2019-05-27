# README for eBird targeting

## Goal: Produce sites to visit to maximize adding species to list

## Process
0. Download current list from: [https://ebird.org/MyEBird?cmd=list&rtype=custom&r=north_america&time=year&fmt=csv](https://ebird.org/MyEBird?cmd=list&rtype=custom&r=north_america&time=year&fmt=csv)
1. Update `data/centers.csv` to indicate coordinates and name of center(s) for 
queries.
2. Update Target-report.Rmd to point to file location of list from 0.
3. Run report via `rmarkdown::render(input = "Target-report.Rmd")`

## Resources
+ [eBird API 2.0](https://documenter.getpostman.com/view/664302/ebird-api-20/2HTbHW)


