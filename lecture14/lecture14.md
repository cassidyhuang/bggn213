---
title: "lecture14"
output: 
  html_document: 
    keep_md: yes
---



Import all data

```r
counts <- read.csv("airway_scaledcounts.csv", stringsAsFactors = FALSE)
metadata <-  read.csv("airway_metadata.csv", stringsAsFactors = FALSE)
```

Look at the data

```r
head(counts)
```

```
##           ensgene SRR1039508 SRR1039509 SRR1039512 SRR1039513 SRR1039516
## 1 ENSG00000000003        723        486        904        445       1170
## 2 ENSG00000000005          0          0          0          0          0
## 3 ENSG00000000419        467        523        616        371        582
## 4 ENSG00000000457        347        258        364        237        318
## 5 ENSG00000000460         96         81         73         66        118
## 6 ENSG00000000938          0          0          1          0          2
##   SRR1039517 SRR1039520 SRR1039521
## 1       1097        806        604
## 2          0          0          0
## 3        781        417        509
## 4        447        330        324
## 5         94        102         74
## 6          0          0          0
```

```r
head(metadata)
```

```
##           id     dex celltype     geo_id
## 1 SRR1039508 control   N61311 GSM1275862
## 2 SRR1039509 treated   N61311 GSM1275863
## 3 SRR1039512 control  N052611 GSM1275866
## 4 SRR1039513 treated  N052611 GSM1275867
## 5 SRR1039516 control  N080611 GSM1275870
## 6 SRR1039517 treated  N080611 GSM1275871
```


```r
colnames(counts)[-1] == metadata$id
```

```
## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
#does the column names equal the id's of meta data?
```


```r
control <- metadata[metadata[,"dex"]=="control",]
control
```

```
##           id     dex celltype     geo_id
## 1 SRR1039508 control   N61311 GSM1275862
## 3 SRR1039512 control  N052611 GSM1275866
## 5 SRR1039516 control  N080611 GSM1275870
## 7 SRR1039520 control  N061011 GSM1275874
```


```r
control.mean <- rowSums(counts[,control$id])/4
#takes sums of rows and divide by number of rows to get means
```


```r
names(control.mean) <- counts$ensgene
#adds ensemble gene names back to better keep track of gene data
head(control.mean)
```

```
## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 
##          900.75            0.00          520.50          339.75 
## ENSG00000000460 ENSG00000000938 
##           97.25            0.75
```

Follow the same procedure for treated samples

```r
#get only treated samples
treated <- metadata[metadata[,"dex"]=="treated",]
treated
```

```
##           id     dex celltype     geo_id
## 2 SRR1039509 treated   N61311 GSM1275863
## 4 SRR1039513 treated  N052611 GSM1275867
## 6 SRR1039517 treated  N080611 GSM1275871
## 8 SRR1039521 treated  N061011 GSM1275875
```

```r
#take sums of treated rows and divide by number of rows
treated.mean <- rowSums(counts[,treated$id])/nrow(treated)
#divide by nrow(treated) to make the code more robust

names(treated.mean) <- counts$ensgene

head(treated.mean)
```

```
## ENSG00000000003 ENSG00000000005 ENSG00000000419 ENSG00000000457 
##          658.00            0.00          546.00          316.50 
## ENSG00000000460 ENSG00000000938 
##           78.75            0.00
```

Combine into new dataframe

```r
meancounts <- data.frame(control.mean, treated.mean)
head(meancounts)
```

```
##                 control.mean treated.mean
## ENSG00000000003       900.75       658.00
## ENSG00000000005         0.00         0.00
## ENSG00000000419       520.50       546.00
## ENSG00000000457       339.75       316.50
## ENSG00000000460        97.25        78.75
## ENSG00000000938         0.75         0.00
```

```r
colSums(meancounts)
```

```
## control.mean treated.mean 
##     23005324     22196524
```

Plot the data

```r
plot(meancounts, xlab = "log Control", ylab = "log Treated", 
     log = "xy")
```

```
## Warning in xy.coords(x, y, xlabel, ylabel, log): 15032 x values <= 0
## omitted from logarithmic plot
```

```
## Warning in xy.coords(x, y, xlabel, ylabel, log): 15281 y values <= 0
## omitted from logarithmic plot
```

![](lecture14_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#plot as log to see all points
```

Calculate log2foldchange

```r
meancounts$log2fc <- log2(meancounts[,"treated.mean"]/meancounts[,"control.mean"])
head(meancounts)
```

```
##                 control.mean treated.mean      log2fc
## ENSG00000000003       900.75       658.00 -0.45303916
## ENSG00000000005         0.00         0.00         NaN
## ENSG00000000419       520.50       546.00  0.06900279
## ENSG00000000457       339.75       316.50 -0.10226805
## ENSG00000000460        97.25        78.75 -0.30441833
## ENSG00000000938         0.75         0.00        -Inf
```
See some weird results. "NaN" is not a number and -Inf results
This is caused by genes with 0 expression


```r
x <- matrix(1:10, ncol=2, byrow=T)
x
```

```
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## [3,]    5    6
## [4,]    7    8
## [5,]    9   10
```

```r
#see example matrix

#make the last number 0
x[5,2] <- 0
x
```

```
##      [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## [3,]    5    6
## [4,]    7    8
## [5,]    9    0
```

```r
#see which number in the matrix is 0
which(x==0, arr.ind = T)
```

```
##      row col
## [1,]   5   2
```

```r
#arr.ind = T tells which row and column is 0
```


Remove the genes with 0 expression

```r
zero.vals <- which(meancounts[,1:2]==0, arr.ind=TRUE)

to.rm <- unique(zero.vals[,1])
mycounts <- meancounts[-to.rm,]
head(mycounts)
```

```
##                 control.mean treated.mean      log2fc
## ENSG00000000003       900.75       658.00 -0.45303916
## ENSG00000000419       520.50       546.00  0.06900279
## ENSG00000000457       339.75       316.50 -0.10226805
## ENSG00000000460        97.25        78.75 -0.30441833
## ENSG00000000971      5219.00      6687.50  0.35769358
## ENSG00000001036      2327.00      1785.75 -0.38194109
```

A common threshold used for calling something differentially expressed is a log2(FoldChange) of greater than 2 or less than -2. Let’s filter the dataset both ways to see how many genes are up or down-regulated.

```r
up.ind <- mycounts$log2fc > 2
down.ind <- mycounts$log2fc < (-2)
```

Determine how many genes are up or down regulated

```r
paste("Up:", sum(up.ind))
```

```
## [1] "Up: 250"
```

```r
paste("Down:", sum(down.ind))
```

```
## [1] "Down: 367"
```

## Adding annotation data

```r
anno <- read.csv("annotables_grch38.csv")
head(anno)
```

```
##           ensgene entrez   symbol chr     start       end strand
## 1 ENSG00000000003   7105   TSPAN6   X 100627109 100639991     -1
## 2 ENSG00000000005  64102     TNMD   X 100584802 100599885      1
## 3 ENSG00000000419   8813     DPM1  20  50934867  50958555     -1
## 4 ENSG00000000457  57147    SCYL3   1 169849631 169894267     -1
## 5 ENSG00000000460  55732 C1orf112   1 169662007 169854080      1
## 6 ENSG00000000938   2268      FGR   1  27612064  27635277     -1
##          biotype
## 1 protein_coding
## 2 protein_coding
## 3 protein_coding
## 4 protein_coding
## 5 protein_coding
## 6 protein_coding
##                                                                                                  description
## 1                                                          tetraspanin 6 [Source:HGNC Symbol;Acc:HGNC:11858]
## 2                                                            tenomodulin [Source:HGNC Symbol;Acc:HGNC:17757]
## 3 dolichyl-phosphate mannosyltransferase polypeptide 1, catalytic subunit [Source:HGNC Symbol;Acc:HGNC:3005]
## 4                                               SCY1-like, kinase-like 3 [Source:HGNC Symbol;Acc:HGNC:19285]
## 5                                    chromosome 1 open reading frame 112 [Source:HGNC Symbol;Acc:HGNC:25565]
## 6                          FGR proto-oncogene, Src family tyrosine kinase [Source:HGNC Symbol;Acc:HGNC:3697]
```

Use the merge function
Merge the annotation data with our mycounts data

```r
results <- merge(mycounts, anno, by.x="row.names", by.y="ensgene")
head(results)
```

```
##         Row.names control.mean treated.mean      log2fc entrez   symbol
## 1 ENSG00000000003       900.75       658.00 -0.45303916   7105   TSPAN6
## 2 ENSG00000000419       520.50       546.00  0.06900279   8813     DPM1
## 3 ENSG00000000457       339.75       316.50 -0.10226805  57147    SCYL3
## 4 ENSG00000000460        97.25        78.75 -0.30441833  55732 C1orf112
## 5 ENSG00000000971      5219.00      6687.50  0.35769358   3075      CFH
## 6 ENSG00000001036      2327.00      1785.75 -0.38194109   2519    FUCA2
##   chr     start       end strand        biotype
## 1   X 100627109 100639991     -1 protein_coding
## 2  20  50934867  50958555     -1 protein_coding
## 3   1 169849631 169894267     -1 protein_coding
## 4   1 169662007 169854080      1 protein_coding
## 5   1 196651878 196747504      1 protein_coding
## 6   6 143494811 143511690     -1 protein_coding
##                                                                                                  description
## 1                                                          tetraspanin 6 [Source:HGNC Symbol;Acc:HGNC:11858]
## 2 dolichyl-phosphate mannosyltransferase polypeptide 1, catalytic subunit [Source:HGNC Symbol;Acc:HGNC:3005]
## 3                                               SCY1-like, kinase-like 3 [Source:HGNC Symbol;Acc:HGNC:19285]
## 4                                    chromosome 1 open reading frame 112 [Source:HGNC Symbol;Acc:HGNC:25565]
## 5                                                     complement factor H [Source:HGNC Symbol;Acc:HGNC:4883]
## 6                                          fucosidase, alpha-L- 2, plasma [Source:HGNC Symbol;Acc:HGNC:4008]
```

Using Bioconductor's annotation packagse to help with mapping

```r
library("AnnotationDbi")
```

```
## Loading required package: stats4
```

```
## Loading required package: BiocGenerics
```

```
## Loading required package: parallel
```

```
## 
## Attaching package: 'BiocGenerics'
```

```
## The following objects are masked from 'package:parallel':
## 
##     clusterApply, clusterApplyLB, clusterCall, clusterEvalQ,
##     clusterExport, clusterMap, parApply, parCapply, parLapply,
##     parLapplyLB, parRapply, parSapply, parSapplyLB
```

```
## The following objects are masked from 'package:stats':
## 
##     IQR, mad, sd, var, xtabs
```

```
## The following objects are masked from 'package:base':
## 
##     anyDuplicated, append, as.data.frame, cbind, colMeans,
##     colnames, colSums, do.call, duplicated, eval, evalq, Filter,
##     Find, get, grep, grepl, intersect, is.unsorted, lapply,
##     lengths, Map, mapply, match, mget, order, paste, pmax,
##     pmax.int, pmin, pmin.int, Position, rank, rbind, Reduce,
##     rowMeans, rownames, rowSums, sapply, setdiff, sort, table,
##     tapply, union, unique, unsplit, which, which.max, which.min
```

```
## Loading required package: Biobase
```

```
## Welcome to Bioconductor
## 
##     Vignettes contain introductory material; view with
##     'browseVignettes()'. To cite Bioconductor, see
##     'citation("Biobase")', and for packages 'citation("pkgname")'.
```

```
## Loading required package: IRanges
```

```
## Loading required package: S4Vectors
```

```
## 
## Attaching package: 'S4Vectors'
```

```
## The following object is masked from 'package:base':
## 
##     expand.grid
```

```r
library("org.Hs.eg.db")
```

```
## 
```

```r
#download organism annotation package org for homo sapiens "Hs"
#organized as AnnotationDbi database package "db"
#using Entrez Gene IDs "eg" as primary key

columns(org.Hs.eg.db)
```

```
##  [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT" 
##  [5] "ENSEMBLTRANS" "ENTREZID"     "ENZYME"       "EVIDENCE"    
##  [9] "EVIDENCEALL"  "GENENAME"     "GO"           "GOALL"       
## [13] "IPI"          "MAP"          "OMIM"         "ONTOLOGY"    
## [17] "ONTOLOGYALL"  "PATH"         "PFAM"         "PMID"        
## [21] "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG"      
## [25] "UNIGENE"      "UNIPROT"
```

```r
mycounts$symbol <- mapIds(org.Hs.eg.db,
                     keys=row.names(mycounts),
                     column="SYMBOL",
                     keytype="ENSEMBL",
                     multiVals="first")
```

```
## 'select()' returned 1:many mapping between keys and columns
```

```r
head(mycounts)
```

```
##                 control.mean treated.mean      log2fc   symbol
## ENSG00000000003       900.75       658.00 -0.45303916   TSPAN6
## ENSG00000000419       520.50       546.00  0.06900279     DPM1
## ENSG00000000457       339.75       316.50 -0.10226805    SCYL3
## ENSG00000000460        97.25        78.75 -0.30441833 C1orf112
## ENSG00000000971      5219.00      6687.50  0.35769358      CFH
## ENSG00000001036      2327.00      1785.75 -0.38194109    FUCA2
```

## DESeq2 analysis
DESeq2 is an R package for analyzing count-based NGS data

```r
library(DESeq2)
```

```
## Loading required package: GenomicRanges
```

```
## Loading required package: GenomeInfoDb
```

```
## Loading required package: SummarizedExperiment
```

```
## Loading required package: DelayedArray
```

```
## Loading required package: matrixStats
```

```
## 
## Attaching package: 'matrixStats'
```

```
## The following objects are masked from 'package:Biobase':
## 
##     anyMissing, rowMedians
```

```
## 
## Attaching package: 'DelayedArray'
```

```
## The following objects are masked from 'package:matrixStats':
## 
##     colMaxs, colMins, colRanges, rowMaxs, rowMins, rowRanges
```

```
## The following object is masked from 'package:base':
## 
##     apply
```

```r
citation("DESeq2")
```

```
## 
##   Love, M.I., Huber, W., Anders, S. Moderated estimation of fold
##   change and dispersion for RNA-seq data with DESeq2 Genome
##   Biology 15(12):550 (2014)
## 
## A BibTeX entry for LaTeX users is
## 
##   @Article{,
##     title = {Moderated estimation of fold change and dispersion for RNA-seq data with DESeq2},
##     author = {Michael I. Love and Wolfgang Huber and Simon Anders},
##     year = {2014},
##     journal = {Genome Biology},
##     doi = {10.1186/s13059-014-0550-8},
##     volume = {15},
##     issue = {12},
##     pages = {550},
##   }
```

Create design formula

```r
dds <- DESeqDataSetFromMatrix(countData=counts, 
                              colData=metadata, 
                              design=~dex, 
                              tidy=TRUE)
```

```
## converting counts to integer mode
```

```
## Warning in DESeqDataSet(se, design = design, ignoreRank): some variables in
## design formula are characters, converting to factors
```

```r
dds
```

```
## class: DESeqDataSet 
## dim: 38694 8 
## metadata(1): version
## assays(1): counts
## rownames(38694): ENSG00000000003 ENSG00000000005 ...
##   ENSG00000283120 ENSG00000283123
## rowData names(0):
## colnames(8): SRR1039508 SRR1039509 ... SRR1039520 SRR1039521
## colData names(4): id dex celltype geo_id
```

Run DESeq

```r
dds <- DESeq(dds)
```

```
## estimating size factors
```

```
## estimating dispersions
```

```
## gene-wise dispersion estimates
```

```
## mean-dispersion relationship
```

```
## final dispersion estimates
```

```
## fitting model and testing
```

See results

```r
res <- results(dds)
res
```

```
## log2 fold change (MLE): dex treated vs control 
## Wald test p-value: dex treated vs control 
## DataFrame with 38694 rows and 6 columns
##                  baseMean log2FoldChange     lfcSE       stat     pvalue
##                 <numeric>      <numeric> <numeric>  <numeric>  <numeric>
## ENSG00000000003 747.19420    -0.35070283 0.1682342 -2.0846111 0.03710462
## ENSG00000000005   0.00000             NA        NA         NA         NA
## ENSG00000000419 520.13416     0.20610652 0.1010134  2.0403876 0.04131173
## ENSG00000000457 322.66484     0.02452714 0.1451103  0.1690242 0.86577762
## ENSG00000000460  87.68263    -0.14714409 0.2569657 -0.5726216 0.56690095
## ...                   ...            ...       ...        ...        ...
## ENSG00000283115  0.000000             NA        NA         NA         NA
## ENSG00000283116  0.000000             NA        NA         NA         NA
## ENSG00000283119  0.000000             NA        NA         NA         NA
## ENSG00000283120  0.974916     -0.6682308  1.694063 -0.3944544  0.6932456
## ENSG00000283123  0.000000             NA        NA         NA         NA
##                      padj
##                 <numeric>
## ENSG00000000003 0.1630257
## ENSG00000000005        NA
## ENSG00000000419 0.1757326
## ENSG00000000457 0.9616577
## ENSG00000000460 0.8157061
## ...                   ...
## ENSG00000283115        NA
## ENSG00000283116        NA
## ENSG00000283119        NA
## ENSG00000283120        NA
## ENSG00000283123        NA
```

Get summary of results

```r
summary(res)
```

```
## 
## out of 25258 with nonzero total read count
## adjusted p-value < 0.1
## LFC > 0 (up)     : 1564, 6.2% 
## LFC < 0 (down)   : 1188, 4.7% 
## outliers [1]     : 142, 0.56% 
## low counts [2]   : 9971, 39% 
## (mean count < 10)
## [1] see 'cooksCutoff' argument of ?results
## [2] see 'independentFiltering' argument of ?results
```

Order results table by smallest p value

```r
resOrdered <- res[order(res$pvalue),]
```

Adjust p value cut off

```r
res05 <- results(dds, alpha=0.05)
summary(res05)
```

```
## 
## out of 25258 with nonzero total read count
## adjusted p-value < 0.05
## LFC > 0 (up)     : 1237, 4.9% 
## LFC < 0 (down)   : 933, 3.7% 
## outliers [1]     : 142, 0.56% 
## low counts [2]   : 9033, 36% 
## (mean count < 6)
## [1] see 'cooksCutoff' argument of ?results
## [2] see 'independentFiltering' argument of ?results
```

The more generic way to access the actual subset of the data.frame passing a threshold like this is with the subset() function

```r
resSig05 <- subset(as.data.frame(res), padj < 0.05)
nrow(resSig05)
```

```
## [1] 2182
```

Find how many are significant with an adjusted p-val < 0.01

```r
resSig01 <- subset(as.data.frame(res), padj < 0.01)
nrow(resSig01)
```

```
## [1] 1437
```

```r
head(resSig01)
```

```
##                  baseMean log2FoldChange      lfcSE      stat       pvalue
## ENSG00000002834 8609.1828      0.4168750 0.10827683  3.850085 1.180767e-04
## ENSG00000003096  414.0753     -0.9645789 0.19172945 -5.030937 4.880878e-07
## ENSG00000003402 3368.7234      1.1624996 0.12612244  9.217230 3.048738e-20
## ENSG00000004059 1684.3218      0.3796901 0.11417088  3.325630 8.821899e-04
## ENSG00000004487 1255.8003     -0.3341069 0.09600563 -3.480076 5.012723e-04
## ENSG00000004700 1510.2085      0.4095532 0.11914030  3.437570 5.869579e-04
##                         padj
## ENSG00000002834 1.824767e-03
## ENSG00000003096 1.490341e-05
## ENSG00000003402 7.859356e-18
## ENSG00000004059 9.389154e-03
## ENSG00000004487 6.006146e-03
## ENSG00000004700 6.760059e-03
```

Using either the previously generated anno object (annotations from the file annotables_grch38.csv file) or the mapIds() function (from the AnnotationDbi package) add annotation to your res01 results data.frame.

```r
columns(org.Hs.eg.db)
```

```
##  [1] "ACCNUM"       "ALIAS"        "ENSEMBL"      "ENSEMBLPROT" 
##  [5] "ENSEMBLTRANS" "ENTREZID"     "ENZYME"       "EVIDENCE"    
##  [9] "EVIDENCEALL"  "GENENAME"     "GO"           "GOALL"       
## [13] "IPI"          "MAP"          "OMIM"         "ONTOLOGY"    
## [17] "ONTOLOGYALL"  "PATH"         "PFAM"         "PMID"        
## [21] "PROSITE"      "REFSEQ"       "SYMBOL"       "UCSCKG"      
## [25] "UNIGENE"      "UNIPROT"
```

```r
resSig01$symbol <- mapIds(org.Hs.eg.db,
                keys=row.names(resSig01),
                column="SYMBOL",
                keytype="ENSEMBL",
                multiVals="first")
```

```
## 'select()' returned 1:many mapping between keys and columns
```

```r
resSig01$entrez <- mapIds(org.Hs.eg.db,
                keys=row.names(resSig01),
                column="ENTREZID",
                keytype="ENSEMBL",
                multiVals="first")
```

```
## 'select()' returned 1:many mapping between keys and columns
```

```r
resSig01$uniprot <- mapIds(org.Hs.eg.db,
                keys=row.names(resSig01),
                column="UNIPROT",
                keytype="ENSEMBL",
                multiVals="first")
```

```
## 'select()' returned 1:many mapping between keys and columns
```

Arrange and view results by adjusted p-value

```r
ord <- order( resSig01$padj )
head(resSig01[ord,])
```

```
##                   baseMean log2FoldChange      lfcSE      stat
## ENSG00000152583   954.7709       4.368359 0.23713648  18.42129
## ENSG00000179094   743.2527       2.863888 0.17555825  16.31304
## ENSG00000116584  2277.9135      -1.034700 0.06505273 -15.90556
## ENSG00000189221  2383.7537       3.341544 0.21241508  15.73120
## ENSG00000120129  3440.7038       2.965211 0.20370277  14.55656
## ENSG00000148175 13493.9204       1.427168 0.10036663  14.21955
##                       pvalue         padj  symbol entrez    uniprot
## ENSG00000152583 8.867079e-76 1.342919e-71 SPARCL1   8404 A0A024RDE1
## ENSG00000179094 7.972621e-60 6.037268e-56    PER1   5187     O15534
## ENSG00000116584 5.798513e-57 2.927283e-53 ARHGEF2   9181     Q92974
## ENSG00000189221 9.244206e-56 3.500088e-52    MAOA   4128     P21397
## ENSG00000120129 5.306416e-48 1.607313e-44   DUSP1   1843     B4DU40
## ENSG00000148175 6.929711e-46 1.749175e-42    STOM   2040     F8VSL7
```

Write to csv file

```r
write.csv(resSig01[ord,], "signif01_results.csv")
```


## Data Visualization

```r
#see gene ID for CRISPLD2
i <- grep("CRISPLD2", resSig01$symbol)
resSig01[i,]
```

```
##                 baseMean log2FoldChange     lfcSE     stat       pvalue
## ENSG00000103196 3096.159       2.626034 0.2674705 9.818031 9.416441e-23
##                         padj   symbol entrez    uniprot
## ENSG00000103196 3.395524e-20 CRISPLD2  83716 A0A140VK80
```


```r
rownames(resSig01[i,])
```

```
## [1] "ENSG00000103196"
```

Plot the counts, intgroup means interesting group, variable is the "dex" column

```r
plotCounts(dds, gene="ENSG00000103196", intgroup="dex")
```

![](lecture14_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

Return the data

```r
d <- plotCounts(dds, gene="ENSG00000103196", intgroup="dex", returnData=TRUE)
head(d)
```

```
##                count     dex
## SRR1039508  774.5002 control
## SRR1039509 6258.7915 treated
## SRR1039512 1100.2741 control
## SRR1039513 6093.0324 treated
## SRR1039516  736.9483 control
## SRR1039517 2742.1908 treated
```

```r
d
```

```
##                count     dex
## SRR1039508  774.5002 control
## SRR1039509 6258.7915 treated
## SRR1039512 1100.2741 control
## SRR1039513 6093.0324 treated
## SRR1039516  736.9483 control
## SRR1039517 2742.1908 treated
## SRR1039520  842.5452 control
## SRR1039521 6224.9923 treated
```

Plot with boxplot

```r
boxplot(count~dex, data=d)
```

![](lecture14_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

Plot with ggplot2

```r
library(ggplot2)
ggplot(d, aes(dex, count)) + 
  geom_boxplot(aes(fill=dex)) + 
  scale_y_log10() + 
  ggtitle("CRISPLD2")
```

![](lecture14_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

To see more plot examples, see 
https://bioboot.github.io/bggn213_S18/class-material/lecture14-BGGN213_W18-lab/




















