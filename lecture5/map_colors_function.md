---
title: "map_colors_function"
output: 
  html_document: 
    keep_md: yes
---



### Let's look at the poor old function.

This is the function code from Barry's student.


```r
map.colors <- function (value,high.low,palette) {
  proportion <- ((value-high.low[1])/(high.low[2]-high.low[1]))
  index <- round ((length(palette)-1)*proportion)+1
  return (palette[index])
}
```

Now let's try to understand this function


```r
map.colors2 <- function(x, 
                        high.low=range(x), 
                        palette=cm.colors(100)) {
  
  # Determine where in the 'high.low' range our values of 'x' lie.
  percent <- ((x-high.low[1])/(high.low[2] - high.low[1]))
  
  #Where in the vector of colors 'palette' is this 'percent'
  #note catch for 0 percent values to 1
  index <- round ((length(palette)-1) * percent )+1
  
  return (palette[index])
}
```


## Working with our own functions
My first function

```r
add <- function(x, y=1) {
 # Sum the input x and y
 x + y
}
```

Let's test our first function


```r
add(6,4)
```

```
## [1] 10
```


```r
add( c(1:5),20 )
```

```
## [1] 21 22 23 24 25
```


Write my second function

```r
rescale <- function(x) {
 rng <-range(x)
 (x - rng[1]) / (rng[2] - rng[1])
}
```

Test my second function

```r
rescale( 1:10 )
```

```
##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
##  [8] 0.7777778 0.8888889 1.0000000
```

Rescale with NA

```r
rescale( c(1,2,NA,3,10) )
```

```
## [1] NA NA NA NA NA
```

Making NA not a problem

```r
rescale2 <- function(x) {
 rng <-range(x, na.rm=TRUE)
 (x - rng[1]) / (rng[2] - rng[1])
}
```


```r
rescale2( c(1,2,NA,3,10) )
```

```
## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000
```



```r
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {
 if(na.rm) {
 rng <-range(x, na.rm=na.rm)
 } else {
 rng <-range(x)
 }
 print("Hello")
 answer <- (x - rng[1]) / (rng[2] - rng[1])

 print("is it me you are looking for?")
 if(plot) {
 plot(answer, typ="b", lwd=4)
 }
 print("I can see it in ...")
 
  return(answer)
}
```


```r
rescale3( c(1,2,NA,3,10), plot=TRUE )
```

```
## [1] "Hello"
## [1] "is it me you are looking for?"
```

![](map_colors_function_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```
## [1] "I can see it in ..."
```

```
## [1] 0.0000000 0.1111111        NA 0.2222222 1.0000000
```












