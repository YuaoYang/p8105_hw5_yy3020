hw5
================
YuaoYang
2019/11/10

# Problem 1

``` r
library(tidyverse)
```

    ## -- Attaching packages ----------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.2.1     √ purrr   0.3.2
    ## √ tibble  2.1.3     √ dplyr   0.8.3
    ## √ tidyr   1.0.0     √ stringr 1.4.0
    ## √ readr   1.3.1     √ forcats 0.4.0

    ## -- Conflicts -------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
set.seed(10)

iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

``` r
fill_miss = function(x) {
  if(is.numeric(x)){
    x = replace_na(x, round(mean(x, na.rm = TRUE),2))
  }else if(is.character(x)){
    x = replace_na(x, "virginica")
  }
}

new_iris = map(iris_with_missing, fill_miss) %>%
  data.frame()
```

\#Problem 2
