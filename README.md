# TimePlotR
a naive library of time-related graphs

## Install:
```{r}
devtools::install_github("TimeWz667/TimePlotR")
```


## Import:
```{r}
library(TimePlotR)
```

## Data frame
```{r}
head(FourLevels)
```

## Usage:
```{r}
dat.ss = as.stage.series(FourLevels, c('Lv1', 'Lv2', 'Lv3', 'Lv4'))
print(dat.ss)
```



## Figures


```{r}
hierarchical.sankey(dat.ss, bar.dist.min = 20)
```
