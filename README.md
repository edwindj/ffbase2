ffbase2
=======

Next version of `ffbase`
- dplyr on ff

Work in progress!


## Usage

```S

species <- 
   tbl_ffdf(iris) %>%
   group_by(Species) %>%
   summarise(petal_width = sum(Petal.Width))
```
