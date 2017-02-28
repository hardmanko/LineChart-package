LineChart-package
=================

This is a simple package that plots line charts in R. The introductory vignettes are in the `/vignettes` folder in pdf form. It's just a few pages and describes most of the whole package.

Installation
------------

This package can be installed directly from this repository with the following commands in R:
```{r}
#You don't need to do this if you already have devtools installed.
install.packages("devtools")

#Install the package
devtools::install_github("hardmanko/LineChart-package")
```

You can also install the package from one of the source packages in the `packages` subdirectory. Once you have a package file on you computer somewhere, you can install the package using:
```{r}
install.packages(type = "source", repos = NULL, pkgs = "path_to_package_file/LineChart_0.2.tar.gz")
```

To just test the package without installing it, you can simply source `./R/LineChart-functions.R`, which also contains all of the functions and the documentation as comments.
