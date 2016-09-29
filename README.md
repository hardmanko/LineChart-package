LineChart-package
=================

This is a simple package that plots line charts in R. The introductory vignettes are in the `/vignettes` folder in pdf form. It's just a few pages and describes pretty much the whole package.

Installation
------------

This package can be installed directly from this repository with the following commands in R:
```{r}
#install.packages("devtools") #Uncomment if you don't have devtools installed
library("devtools")
devtools::install_github("hardmanko/LineChart-package")
```

It is slightly better to install the package from a package file, which you can get by downloading this repository and getting the file from the "packaged" subdirectory. Once you have that file on you computer somewhere, you can install the package using:
```{r}
install.packages(type = "source", repos = NULL, pkgs = "path_to_package_file/LineChart_0.2.tar.gz")
```

To just test the package without installing it, you can simply source `./R/LineChart-functions.R`, which also contains all of the functions and the documentation as comments.
