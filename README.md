ffbase2
=======

Next version of `ffbase`
- dplyr on ff

Work in progress!

[![Build Status](https://travis-ci.org/edwindj/ffbase2.svg?branch=master)](https://travis-ci.org/edwindj/ffbase2)

## Usage

```S

iris_f <- tbl_ffdf(iris)
# or equivalently iris_f <- as.ffdf(iris)

species <- 
   iris_f %>%
   group_by(Species) %>%
   summarise(petal_width = sum(Petal.Width))
```

