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

## Sankey diagram for panel data. d

Data frame

```{r}
head(FourLevels)
```

## Usage:
```{r}
dat.ss = as.stage.series(FourLevels, c('Lv1', 'Lv2', 'Lv3', 'Lv4'))
print(dat.ss)
```

## State transition diagram

## Monte-Carlo trends


## Stage distribution diagram

## Figures


```{r}
hierarchical.sankey(dat.ss, bar.dist.min = 20)
```
