
# Dict

[![](https://www.r-pkg.org/badges/version/Dict?color=green)](https://cran.r-project.org/package=Dict)
[\![](<https://www.r-pkg.org/badges/version/Dict?color=green>)](<https://cran.r-project.org/package=Dict>)

## Overview

`Dict` is a R package which implements a key-value dictionary data structure based on [R6](https://github.com/r-lib/R6) class. It is designed to be similar usages with other languages' dictionary implementations (e.g. Python).

R's `vector` and `list`, of course can have names, so you can get and set value by a name (key) like a dictionary. Using regular data structure must be a recommended way in the most of cases. But, if you are interested in the following characteristics, this package is for you!

-   **Reference semantics**: Useful to keep project wide parameters or states referred from multiple models.
-   **Inheritance**: Easily expand features according to your needs.

## Installation

```R
install.packages("Dict")

# Or the the development version from GitHub:
# install.packages("devtools")
devtools::install_github("five-dots/Dict")
```

## Usage

### Instantiate a dictionary

To instantiate a dict, you can pass any length of key-value pairs to the initialize method.

```R
library(Dict)
ages <- Dict$new(
  Charlie = 40L,
  Alice = 30L,
  Bob = 25L,
  .class = "integer",
  .overwrite = TRUE
)
ages
```

    
    # A tibble: 3 x 2
     key      value    
     <chr>    <list>   
    1 Charlie <int [1]>
    2 Alice   <int [1]>
    3 Bob     <int [1]>

#### Some notes

-   `dict()` can be used instead of `Dict$new()` as some IDEs cloud not show R6's function arguments hint.
-   `.class` specifies what kind of objects the dictionary can contains. Default "any" means the dict cloud have any type of value.
-   `.overwrite` controls the behavior when the same key is added.
-   Dict keep key-value items in `tbl_df` from tibble package whose key is a character column and value is a list column. You can use various existing tooling for `data.frame` or `tibble` to manipulate dict items.

### Get a value

A value can be access by both `Dict$get()` or `` `[` `` with a character key or integer index of items rows.

```R
ages["Bob"]
ages$get("Bob")
ages$get(3L)
```

    [1] 25
    
    [1] 25
    
    [1] 25



If no key found, value of `default` is returned.

```R
ages["Michael", default = 30]
```

    [1] 30

### Add a new item

Adding a item also can be done by R6 methods `Dict$add()` or `` `[<-` ``.

```R
ages["John"] <- 18L # or ages$add(John = 18L)
ages["John"]
```

    
    [1] 18



Can be overridden if `.overwrite = TRUE` (default).

```R
ages["Bob"] <- 26L
ages$get("Bob")
```

    
    [1] 26

### Other methods and fields

Remove item:

```R
ages$remove("Bob")
```



Check if items contains a key:

```R
ages$has("Bob")
```

    [1] FALSE



Sort by keys:

```R
ages$sort()
ages
```

    
    # A tibble: 3 x 2
     key      value    
     <chr>    <list>   
    1 Alice   <int [1]>
    2 Charlie <int [1]>
    3 John    <int [1]>



Clear items:

```R
ages$clear()
ages
```

    
    # A tibble: 0 x 2
    # … with 2 variables: key <chr>, value <list>



Fields:

```R
ages$keys
ages$values
ages$items
ages$length
```
