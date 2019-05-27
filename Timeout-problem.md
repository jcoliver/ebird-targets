# Timeout problems
# 2019-04-21

**UPDATE**
2019-05-11

+ Figured out how to fix eBird requests, which are handled by code I wrote (use 
`curl::curl` and set CONNECTTIMEOUT to be higher than 10 seconds).
+ But retrieving maps (via `ggmap::get_map`) required globally setting the value
for connection timeout via `httr::set_config(httr::config(connecttimeout = 30))`

The request:
https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp

Works just fine in a web browser and with bash command line tools (see below), but fails with 

```{r}
request <- "https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp"
jsonlite::fromJSON(txt = request)
# Error in open.connection(con, "rb") : 
#  Timeout was reached: Resolving timed out after 10000 milliseconds
```

And

```{r}
httr::GET(request, timeout(20))
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#  Timeout was reached: Resolving timed out after 10000 milliseconds
```

This _also_ fails with same error:
```{r}
httr::GET("http://cran.r-project.org/Rlogo.jpg")
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#  Timeout was reached: Resolving timed out after 10000 milliseconds
```

Changing the timeout does not seem to matter; passing values less than 10 
(seconds) will change timeout value; **however**, values _above_ 10, e.g. 20, 
like example below, still time out in 10 seconds.
```{r}
httr::GET(url = "http://cran.r-project.org/Rlogo.jpg", 
          httr::timeout(20))
# Error in curl::curl_fetch_memory(url, handle = handle) : 
#  Timeout was reached: Resolving timed out after 10000 milliseconds
```

But, here we use curl_download, and setting the timeout this way actually gets 
the appropriate reponse, and it takes longer (does not time out). Following 
recommendations from https://github.com/jeroen/curl/issues/72
```{r}
curl::curl_download("http://cran.r-project.org/Rlogo.jpg",
                    destfile = "~/Desktop/Rlogo.jpg",
                    handle = curl::new_handle(CONNECTTIMEOUT = 20))
```

Setting timeout this way also fixes the eBird query.
```{r}
curl::curl_download("https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp",
                    destfile = "~/Desktop/ebird.json",
                    handle = curl::new_handle(CONNECTTIMEOUT = 20))
```

Tough to see how to set timeout in `jsonlite::fromJSON`, but we can split the 
download and parse it into JSON

```{r}
request <- "https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp"
ebird.connection <- curl::curl(request,
                        handle = curl::new_handle(CONNECTTIMEOUT = 20))
ebirdJSON <- readLines(ebird.connection)
obs.request <- try(expr = jsonlite::fromJSON(txt = ebirdJSON), silent = TRUE)
```

However in Linux terminal, this works:

```{bash}
curl "https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp" > ebird.json
```

As does wget:
```{bash}
wget "https://ebird.org/ws2.0/data/obs/geo/recent?&lat=32.241&lng=-110.938&dist=50&back=4&hotspot=true&key=q41r9o19hdsp"
```

And for the rlogo:
```{bash}
curl "http://cran.r-project.org/Rlogo.jpg" > ~/Desktop/Rlogo.jpg
```