# README for eBird targeting

## Goal: Produce sites to visit to maximize adding species to list

## Process
0. Download current list from: [https://ebird.org/MyEBird?cmd=list&rtype=custom&r=north_america&time=year&fmt=csv](https://ebird.org/MyEBird?cmd=list&rtype=custom&r=north_america&time=year&fmt=csv)
1. Use API to get recent observations within radius of specified latitude & longitude
  + https://ebird.org/ws2.0/data/obs/geo/recent?lat={{lat}}&lng={{lng}}
  + dist=80 # kilometers
  + back=4 # days
  + hotspot=true
2. Find difference between lists 0 & 1 to identify targets
3. For each target, use API to get recent nearby observations of species
  + https://ebird.org/ws2.0/data/obs/geo/recent?lat={{lat}}&lng={{lng}}
  + https://ebird.org/ws2.0/data/obs/geo/recent/rethaw?lat=31.7251514&lng=-110.8801067&key={{key}}
  + dist=80 # kilometers
  + back=4 # days
  + hotspot=true
4. Combine results from each query in 3. Produce a list with an element for each site. Each list element would have:
  + Site name
  + Number of targets
  + Coordinates (two-element numeric vector)
    + Longitude
    + Latitude
  + Data frame
    + Species name
    + Average number observed
    + Number of times reported per day

## Resources
+ [eBird API 2.0](https://documenter.getpostman.com/view/664302/ebird-api-20/2HTbHW)


