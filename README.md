LineChart-package
=================

This is a simple package that plots line charts in R. The introductory vignettes are in the `/vignettes` directory of this repository in pdf form. It's just a few pages and describes most of the whole package. Or, once the package is installed, you can run
```{r}
vignette("introduction", "LineChart")
```
to open the vignette.

Installation
------------

This package can be installed directly from this repository with the following commands in R:
```{r}
#You can skip this if you already have these packages installed and up-to-date
install.packages(c("devtools", "R.rsp"))

# Install the package
devtools::install_github("hardmanko/LineChart-package", build_vignettes = TRUE)
```

You can alternately install the package from one of the source packages in the `packages` subdirectory. Once you have a package file on you computer somewhere, you can install the package using:
```{r}
install.packages(type = "source", repos = NULL, pkgs = "path_to_package_file/LineChart_0.3.tar.gz")
```
