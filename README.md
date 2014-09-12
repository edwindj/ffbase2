ffbase2
=======

Next version of `ffbase`
- dplyr on ff

Working, but documentation is not yet CRAN ready...

[![Build Status](https://travis-ci.org/edwindj/ffbase2.svg?branch=master)](https://travis-ci.org/edwindj/ffbase2)

## Usage

```S
iris_f <- tbl_ffdf(iris)

species <- 
   iris_f %>%
   group_by(Species) %>%
   summarise(petal_width = sum(Petal.Width))
```

Using directories for storing ffdf
```S
# store a ffdf data.frame in "db_ff"" directory
mtcars <- tbl_ffdf(mtcars, src="db_ff")

src <- src_ffdf("db_ff")

#retrieve table from directory 
my_tbl <- tbl(src, from="mtcars")
```
