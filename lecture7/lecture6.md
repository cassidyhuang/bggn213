---
title: "Lecture 7"
output: 
  html_document: 
    keep_md: yes
---




# R functions
We can source any file of R code with the `source`() function


```r
source("http://tinyurl.com/rescale-R")
```


Let's make sure things are here

```r
ls()
```

```
##  [1] "both_na"         "both_na2"        "both_na3"       
##  [4] "df1"             "df2"             "df3"            
##  [7] "gene_intersect"  "gene_intersect2" "gene_intersect3"
## [10] "gene_intersect4" "rescale"         "rescale2"
```

Check our `rescale()` is working

```r
rescale(1:10)
```

```
##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
##  [8] 0.7777778 0.8888889 1.0000000
```


```r
rescale( c(1:10, "string"))
```


Let's check if `rescale2()` does any better

```r
rescale2( c(1:10, "string"))
```

## FUnction for finding missing values in two datasets

Write a `both_na()` function to do this


```r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

is.na(x)
```

```
## [1] FALSE FALSE  TRUE FALSE  TRUE
```

```r
which(is.na(x))
```

```
## [1] 3 5
```

```r
which(!is.na(x))
```

```
## [1] 1 2 4
```


```r
# how many time it is true in x
sum( is.na(x))
```

```
## [1] 2
```

```r
# how many times it is true in both x and y
sum( is.na(x) + is.na(y))
```

```
## [1] 4
```

```r
# where is it `na` in both x and y
is.na(x) & is.na(y)
```

```
## [1] FALSE FALSE  TRUE FALSE FALSE
```

```r
# how many times is it `na` in both x and y
sum( is.na(x) & is.na(y) )
```

```
## [1] 1
```


Writing a working function from this snippet

```r
both_na <- function(x, y) {
 sum( is.na(x) & is.na(y) )
}
```

Test it


```r
both_na(x, y)
```

```
## [1] 1
```


```r
x <- c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA)

both_na(x, y2)
```

Improving function with error message

```r
both_na2 <- function(x, y) {
  
   if(length(x) != length(y)) {
    stop("Input x and y should be the same length")
   }
  
 sum( is.na(x) & is.na(y) )
}
```

Test new function

```r
both_na2(x, y2)
```


Refine function by adding more info `both_na3()`

```r
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

both_na3(x, y)
```

```
## Found 1 NA's at position(s):3
```

```
## $number
## [1] 1
## 
## $which
## [1] 3
```





## And a last function

```r
x <- df1$IDs
y <- df2$IDs

x
```

```
## [1] "gene1" "gene2" "gene3"
```

```r
y
```

```
## [1] "gene2" "gene4" "gene3" "gene5"
```


```r
# find similarities between x and y
intersect(x, y)
```

```
## [1] "gene2" "gene3"
```


We can try the `intersect()` function and the `%in%` functions

```r
intersect(x, y)
```

```
## [1] "gene2" "gene3"
```

```r
x %in% y
```

```
## [1] FALSE  TRUE  TRUE
```



```r
x[x %in% y]
```

```
## [1] "gene2" "gene3"
```

```r
y[ y %in% x ]
```

```
## [1] "gene2" "gene3"
```

```r
#put these together as columns of a matrix
cbind( x[ x %in% y ], y[ y %in% x ] )
```

```
##      [,1]    [,2]   
## [1,] "gene2" "gene2"
## [2,] "gene3" "gene3"
```


```r
#cbind
cbind( c("Hello", "Help"), c("Please", "Help"))
```

```
##      [,1]    [,2]    
## [1,] "Hello" "Please"
## [2,] "Help"  "Help"
```

```r
#rbind
rbind( c("Hello", "Help"), c("Please", "Help"))
```

```
##      [,1]     [,2]  
## [1,] "Hello"  "Help"
## [2,] "Please" "Help"
```

Writing it into a function

```r
gene_intersect <- function(x, y) {
 cbind( x[ x %in% y ], y[ y %in% x ] )
}
```



```r
gene_intersect(x, y)
```

```
##      [,1]    [,2]   
## [1,] "gene2" "gene2"
## [2,] "gene3" "gene3"
```


Gene intersect for data frames

```r
gene_intersect2(df1, df2)
```

```
##     IDs exp df2[df2$IDs %in% df1$IDs, "exp"]
## 2 gene2   1                               -2
## 3 gene3   1                                1
```

Make the output look better, into `gene_intersect3`

```r
gene.colname="IDs"
df1[,gene.colname]
```

```
## [1] "gene1" "gene2" "gene3"
```

```r
gene_intersect3(df1, df2)
```

```
##     IDs exp exp2
## 2 gene2   1   -2
## 3 gene3   1    1
```



Make the function look better

```r
gene_intersect4(df1, df3)
```

```
## Warning in data.frame(..., check.names = FALSE): row names were found from
## a short variable and have been discarded
```

```
##     IDs exp exp2
## 1 gene2   1   -2
## 2 gene2   1   NA
```


Let's use the `merge()` function

```r
merge(df1, df2, by="IDs")
```

```
##     IDs exp.x exp.y
## 1 gene2     1    -2
## 2 gene3     1     1
```















