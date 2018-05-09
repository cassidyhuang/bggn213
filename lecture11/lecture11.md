---
title: "Untitled"
output: 
  html_document: 
    keep_md: yes
---




## PDB Statistics

```r
#import data
p <- read.table("Data Export Summary.csv", sep = ",", header = T, row.names = 1)
#set rownames to be experimental method

#calculate percent of each type of molecule
percent <- (p$Total / sum(p$Total))*100
names(percent) <- row.names(p)
percent
```

```
##               X-Ray                 NMR Electron Microscopy 
##         89.51673340          8.71321614          1.51239392 
##               Other        Multi Method 
##          0.16986775          0.08778879
```




