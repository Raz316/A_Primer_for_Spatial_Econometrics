---
title: "θ.R Table"
author: "Takuya Shimamura"
date: "`r Sys.time()`"
output: html_document
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### kable

```{r kable}
knitr::kable(head(mtcars))
```

### DT package

```{r DT_package}
library(DT)
datatable(mtcars)
```
