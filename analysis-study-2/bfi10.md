BFI-10
================

**Preparatory settings**

Initialise Framework

``` r
knitr::read_chunk("start3.R")
```

``` r
library(puttytat4R)
source("init/database.R")
# outputString("* Connecting to database ...")
# dbConnect_operator()
# outputDone(step = T)
source("init/data-manipulation.R")
```

Connect to database

``` r
dbConnect_operator("Study2")
```

    ## * Connected to database: URBAN-MV-VIE_UniBw_Study2 
    ## ** See object: dbconn_study2

Analysis
========

Load libaries
-------------

``` r
library(psych)
library(ggplot2)
```

    ## 
    ## Attaching package: 'ggplot2'

    ## The following objects are masked from 'package:psych':
    ## 
    ##     %+%, alpha

Load data
---------

``` r
dat_bfi <- dbGetSrc(dbconn_study2, "t_q_bfi10")
```

    ## * Query data from source: t_q_bfi10

``` r
head(dat_bfi)
```

    ##   subid bfi01 bfi02 bfi03 bfi04 bfi05 bfi06 bfi07 bfi08 bfi09 bfi10
    ## 1     1     2     5     2     4     3     4     2     4     2     5
    ## 2     2     2     2     2     2     3     4     2     4     2     4
    ## 3     3     3     4     4     3     4     4     4     3     2     4
    ## 4     4     4     3     2     2     4     4     4     4     3     4
    ## 5     5     4     2     4     5     5     3     3     4     2     1
    ## 6     6     2     3     2     5     4     4     3     4     2     4

Recode items
------------

``` r
items2recode <- names(dat_bfi)[c(1, 3, 4, 5, 7) + 1]
dat_bfi <- recodeItems(dat_bfi, items2recode, 5)
head(dat_bfi)
```

    ##   subid bfi01 bfi02 bfi03 bfi04 bfi05 bfi06 bfi07 bfi08 bfi09 bfi10
    ## 1     1     4     5     4     2     3     4     4     4     2     5
    ## 2     2     4     2     4     4     3     4     4     4     2     4
    ## 3     3     3     4     2     3     2     4     2     3     2     4
    ## 4     4     2     3     4     4     2     4     2     4     3     4
    ## 5     5     2     2     2     1     1     3     3     4     2     1
    ## 6     6     4     3     4     1     2     4     3     4     2     4

Compute scores
--------------

``` r
scales2compute <- 
  c("bfi_e", 
    "bfi_n", 
    "bfi_o", 
    "bfi_c", 
    "bfi_a")

items4scales <- 
  list(paste0("bfi", sprintf("%02d", c(1, 6))),
       paste0("bfi", sprintf("%02d", c(4, 9))),
       paste0("bfi", sprintf("%02d", c(5, 10))),
       paste0("bfi", sprintf("%02d", c(3, 8))),
       paste0("bfi", sprintf("%02d", c(2, 7))))

dat_bfi <- 
  computeScores(dat_bfi, 
                scales2compute, 
                items4scales, 
                "mean",
                compZ = T)

head(dat_bfi[, c(scales2compute)])
```

    ##   bfi_e bfi_n bfi_o bfi_c bfi_a
    ## 1   4.0   2.0   4.0   4.0   4.5
    ## 2   4.0   3.0   3.5   4.0   3.0
    ## 3   3.5   2.5   3.0   2.5   3.0
    ## 4   3.0   3.5   3.0   4.0   2.5
    ## 5   2.5   1.5   1.0   3.0   2.5
    ## 6   4.0   1.5   3.0   4.0   3.0

``` r
head(dat_bfi[, c(paste0(scales2compute, ".z"))])
```

    ##      bfi_e.z    bfi_n.z    bfi_o.z    bfi_c.z     bfi_a.z
    ## 1  0.5435573 -0.3832787  0.8325775  0.3027937  1.94786225
    ## 2  0.5435573  0.9498647  0.2042171  0.3027937 -0.01636859
    ## 3  0.0000000  0.2832930 -0.4241432 -1.6095876 -0.01636859
    ## 4 -0.5435573  1.6164364 -0.4241432  0.3027937 -0.67111220
    ## 5 -1.0871146 -1.0498505 -2.9375847 -0.9721272 -0.67111220
    ## 6  0.5435573 -1.0498505 -0.4241432  0.3027937 -0.01636859

Boxplot
-------

### Gather data

``` r
dat_bfi.long <- 
  dat_bfi %>% 
  select(subid, bfi_e:bfi_a) %>% 
  gather(key = subid) %>% 
  setNames(., c("subid", "scale", "score")) %>% 
  mutate(scale = factor(scale, 
                        levels = scales2compute,
                        labels = c("Extraversion",
                                   "Neurotiscm",
                                   "Openness",
                                   "Consiousness",
                                   "Agreeableness")))
```

### Adjust values for plotting points and lines

``` r
dat_bfi.long$scale.jittered <- 
  jitter(as.numeric(dat_bfi.long$scale), factor = 0.4)
dat_bfi.long$score.jittered <- 
  jitter(dat_bfi.long$score, factor = 1.5)

head(dat_bfi.long)
```

    ##   subid        scale score scale.jittered score.jittered
    ## 1     1 Extraversion   4.0      1.0501789       4.108946
    ## 2     2 Extraversion   4.0      0.9234541       4.080272
    ## 3     3 Extraversion   3.5      1.0688735       3.583723
    ## 4     4 Extraversion   3.0      1.0661835       2.976357
    ## 5     5 Extraversion   2.5      1.0055063       2.409349
    ## 6     6 Extraversion   4.0      0.9744470       3.973813

### Plot boxplot

``` r
ggplot() + 
  geom_boxplot(data = dat_bfi.long,
               aes(x = scale,
                   y = score,
                   fill = scale),
               alpha = 0.25,
               notch = T, notchwidth = 0.8) +
  geom_point(data = dat_bfi.long,
             aes(x = scale.jittered,
                 y = dat_bfi.long$score.jittered,
                 colour = scale)) + 
  geom_line(data = dat_bfi.long,
            aes(x = scale.jittered,
                y = dat_bfi.long$score.jittered,
                group = subid),
            alpha = 0.25) + 
  theme_bw() + 
  ggtitle("BFI-10") + 
  labs(x = "Scale",
       y = "Score",
       fill = "Scale", colour = "Scale")
```

    ## notch went outside hinges. Try setting notch=FALSE.

![](bfi10_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-7-1.png)

Correlation
-----------

``` r
pairs.panels(dat_bfi[, scales2compute], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
```

![](bfi10_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-8-1.png)
