ffbase2
=======

Next version of `ffbase`
- dplyr on ff

Working, but documentation is not yet CRAN ready...

[![Build Status](https://travis-ci.org/edwindj/ffbase2.svg?branch=master)](https://travis-ci.org/edwindj/ffbase2)

## Install

`ffbase2` is currently only available from github. To install it, run the following
script.
```S
# install.packages("devtools")
devtools::install_github("edwindj/ffbase2")
```

### CRAN-readiness

- Documentation needs to be completed
- Add more tests
- copy some `dplyr` internal functions into ffbase2
- Phase out dependency on `ffbase` (version 1)

## Usage

### Creation
Creating a tbl_ffdf: this will create/use a temporary ffdf data.frame in 
`options("fftempdir")`.

```S
iris_f <- tbl_ffdf(iris)

species <- 
   iris_f %>%
   group_by(Species) %>%
   summarise(petal_width = sum(Petal.Width))
```

A `tbl_ffdf` is also a `ffdf`

```r
iris_f <- tbl_ffdf(iris)
is.ffdf(iris_f)
```

```
## [1] TRUE
```

Use `src_ffdf` for storing your data in a directory 

```r
library(ffbase2)
# store a ffdf data.frame in "./db_ff"" directory
cars <- tbl_ffdf(mtcars, src="./db_ff", name="cars")
print(cars, n=2)
```

```
## Source:     ffdf ('./db_ff/cars') [32 x 11]
## 
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1   21   6  160 110  3.9 2.620 16.46  0  1    4    4
## 2   21   6  160 110  3.9 2.875 17.02  0  1    4    4
## .. ... ...  ... ...  ...   ...   ... .. ..  ...  ...
```

To retrieve tables from a ffdf source, use `src_ffdf`

```r
src <- src_ffdf("./db_ff")
print(src) 
```

```
## src: ffdf ['./db_ff']
## tbls: cars
```

```r
# what tables are available?
src_tbls(src)
```

```
## [1] "cars"
```

```r
#retrieve table from src 
cars <- tbl(src, from="cars") # or equivalently tbl_ffdf(src=src, name="cars")
print(cars, n=2)
```

```
## Source:     ffdf ('./db_ff/cars') [32 x 11]
## 
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1   21   6  160 110  3.9 2.620 16.46  0  1    4    4
## 2   21   6  160 110  3.9 2.875 17.02  0  1    4    4
## .. ... ...  ... ...  ...   ...   ... .. ..  ...  ...
```

Use `copy_to` to add data to a `src_ffdf`

```r
src <- src_ffdf("./db_ff")
copy_to(src, iris) # or equivalenty tbl_ffdf(iris, src)
```

```
## Source:     ffdf ('./db_ff/iris') [150 x 5]
## 
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
## 1           5.1         3.5          1.4         0.2  setosa
## 2           4.9         3.0          1.4         0.2  setosa
## 3           4.7         3.2          1.3         0.2  setosa
## 4           4.6         3.1          1.5         0.2  setosa
## 5           5.0         3.6          1.4         0.2  setosa
## 6           5.4         3.9          1.7         0.4  setosa
## 7           4.6         3.4          1.4         0.3  setosa
## 8           5.0         3.4          1.5         0.2  setosa
## 9           4.4         2.9          1.4         0.2  setosa
## 10          4.9         3.1          1.5         0.1  setosa
## ..          ...         ...          ...         ...     ...
```

```r
src_tbls(src)
```

```
## [1] "cars" "iris"
```

