
<!-- README.md is generated from README.Rmd. Please edit that file -->

# metapip

<!-- badges: start -->
<!-- badges: end -->

The goal of `{metapip}` is to provide the user with a set of functions
that allow him/her to work efficiently with all the PIP R packages. The
technical world of PIP consist of several packages that interact to each
to load, format, modify, and estimate data for the PIP system. Given
that the number of packages has increased over time, it was necessary to
create a meta package whose only objective is the proper management of
all the other PIP packages.

`{metapip}` is highly inspired by the
[{tidyverse}](https://github.com/tidyverse/tidyverse), so that some of
its functions are just a simple refactoring of the `{tidyverse}`
functions. All the credit, then, goes to the Tidyerse team. Thank you!

In addition to the basic functionality of attaching and installing
PIP-related R packages, `{metapip}` also works as the manager of those
packages in your working environment such that you are always working
with the right version of any of the PIP packages.

## Core packages

The core PIP R packages are

- pipapi
- pipaux
- pipload
- wbpip
- pipfun
- pipdata
- pipr

## Installation

You can install the development version of metapip from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("PIP-Technical-Team/metapip")
```

## Functions and it’s usage in metapip

Since `metapip` works with Github API a lot it is required to have a
github token in your environment. You can generate a token for yourself
from [here](https://github.com/settings/tokens) and set it as an
environment variable as shown below.

``` r
library(metapip)
Sys.setenv(GITHUB_PAT = "code")
```

Let us now go through the functions in metapip.

1.  `get_branches()`

    This function as the name suggests gives you the available branches
    for a particular pip package on GitHub. It will display in the
    console the available branches.

``` r
get_branches("pipr")   

#── These are available branches for pipr package:  
#• 29-website-formatting
#• 61_global_end
#• DEV
#• add_retry
#• default_options
#• fix_popshare
#• gh-pages
#• httr2
#• main
#• test
```

At the same time it also returns the branch name as character vector
invisibly.

``` r
out <- get_branches("pipr")

#── These are available branches for pipr package:  
#• 29-website-formatting
#• 61_global_end
#• DEV
#• add_retry
#• default_options
#• fix_popshare
#• gh-pages
#• httr2
#• main
#• test

out
#[1] "29-website-formatting" "61_global_end"         "DEV"                   "add_retry"            
#[5] "default_options"       "fix_popshare"          "gh-pages"              "httr2"                
#[9] "main"                  "test"                 
```

There is an argument called `display` which is set as `TRUE` by default.
This argument controls if you want to show the branch names in the
console or not.

``` r
out <- get_branches("pipr", display = FALSE)  
out
#[1] "29-website-formatting" "61_global_end"         "DEV"                   "add_retry"            
#[5] "default_options"       "fix_popshare"          "gh-pages"              "httr2"                
#[9] "main"                  "test"                 
```

2.  `install_branch()`

    This installs a specific branch for a particular package.

``` r
install_branch(package = "wbpip", branch = "DEV")
```

The above command will install `DEV` branch of `wbpip` package.

3.  `get_branch_info()`

This function will give you information of a particular branch in a
package. For example, when was last commit done on this branch and by
whom.

``` r
get_branch_info(package = "pipr", branch = "DEV")

#  package branch_name last_commit_author_name     last_update_time
#1    pipr         DEV               Tony Fujs 2023-09-04T13:06:47Z
```

You may pass more than one branch names.

``` r
get_branch_info(package = "wbpip", branch = c("PROD", "QA"))

#  package branch_name last_commit_author_name     last_update_time
#1   wbpip        PROD               Tony Fujs 2023-09-20T08:02:24Z
#2   wbpip          QA                tonyfujs 2023-07-14T08:50:33Z
```

4.  `install_latest_branch()`

This function installs the packages from the branch which was last
updated i.e the latest branch.

If you pass no arguments to it, it will install latest branch for all
pip core R packages. However, you can also restrict it to only a few
packages as well.

``` r
install_latest_branch()
install_latest_branch(c("pipfun", "pipapi"))
```

5.  `core_metadata()`

Get metadata information about pip packages.

``` r
out <- core_metadata()
out <- core_metadata(c("pipapi", "wbpip"))
```

6.  `install_all_packages()`

Install one or more core packages from a specific branch. If you have
fresh installation of R then running `install_all_packages()` would set
you up with all pip packages.

``` r
install_all_packages(branch = "test")
install_all_packages(c("pipapi", "wbpip"), "test")
```
