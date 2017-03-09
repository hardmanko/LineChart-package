LineChart-package
=================

This is a simple package that plots line charts in R. The introductory manual is in the `/vignettes` directory of this repository in pdf form. It's just a few pages and describes most of the whole package. Or, once the package is installed, you can run
```{r}
vignette("introduction", "LineChart")
```
to open the introductory manual.

Installation
------------

This package can be installed directly from this repository with the following commands in R:
```{r}
#You can skip this if you already have these packages installed and up-to-date
install.packages(c("devtools", "R.rsp"))

# Install the package
devtools::install_github("hardmanko/LineChart-package", build_vignettes = TRUE)
```

