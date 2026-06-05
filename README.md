
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PacemakeR

<!-- badges: start -->

<!-- badges: end -->

PacmakeR is an R package for analysing pacing strategies in major
marathon races, such as London or Berlin. The package provides simple
tools to analyse marathon data, compute paces, summarise progression as
well as forcasting pacing strategies based on the fed data.

## Installation

You can install the development version of PacemakeR from
[GitHub](https://github.com/Runandecon/PacemakeR) with:

``` r
# install.packages("devtools")
devtools::install_github("Runandecon/PacemakeR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# install.packages("devtools")
devtools::install_github("Runandecon/PacemakeR")
#> Warning: `install_github()` was deprecated in devtools 2.5.0.
#> ℹ Please use pak::pak("user/repo") instead.
#> This warning is displayed once per session.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Using GitHub PAT from the git credential store.
#> Downloading GitHub repo Runandecon/PacemakeR@HEAD
#> cpp11 (0.5.3 -> 0.5.5) [CRAN]
#> S7    (0.2.1 -> 0.2.2) [CRAN]
#> dplyr (1.2.0 -> 1.2.1) [CRAN]
#> Installing 3 packages: cpp11, S7, dplyr
#> Installing packages into 'C:/Users/Julia/AppData/Local/R/win-library/4.5'
#> (as 'lib' is unspecified)
#> package 'cpp11' successfully unpacked and MD5 sums checked
#> package 'S7' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'S7'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\Julia\AppData\Local\R\win-library\4.5\00LOCK\S7\libs\x64\S7.dll to
#> C:\Users\Julia\AppData\Local\R\win-library\4.5\S7\libs\x64\S7.dll: Permission
#> denied
#> Warning: restored 'S7'
#> package 'dplyr' successfully unpacked and MD5 sums checked
#> Warning: cannot remove prior installation of package 'dplyr'
#> Warning in file.copy(savedcopy, lib, recursive = TRUE): problem copying
#> C:\Users\Julia\AppData\Local\R\win-library\4.5\00LOCK\dplyr\libs\x64\dplyr.dll
#> to C:\Users\Julia\AppData\Local\R\win-library\4.5\dplyr\libs\x64\dplyr.dll:
#> Permission denied
#> Warning: restored 'dplyr'
#> 
#> The downloaded binary packages are in
#>  C:\Users\Julia\AppData\Local\Temp\RtmpMlmYxf\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>       ✔  checking for file 'C:\Users\Julia\AppData\Local\Temp\RtmpMlmYxf\remotes4600f091e21\Runandecon-PacemakeR-bf83261/DESCRIPTION' (720ms)
#>   ─  preparing 'PacemakeR':
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts (462ms)
#> ─  checking for empty or unneeded directories
#>        NB: this package now depends on R (>= 3.5.0)
#>      WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>      serialize/load version 3 cannot be read in older versions of R.
#>      File(s) containing such objects:
#>        'PacemakeR/data/London_Marathon_2026.rda'
#>      NB: this package now depends on R (>=        NB: this package now depends on R (>= 4.1.0)
#>      WARNING: Added dependency on R >= 4.1.0 because package code uses the
#>      pipe |> or function shorthand \(...) syntax added in R 4.1.0.
#>      File(s) using such syntax:
#>        'processing.R'
#> ─  building 'PacemakeR_0.1.0.tar.gz'
#>      
#> 
#> Installing package into 'C:/Users/Julia/AppData/Local/R/win-library/4.5'
#> (as 'lib' is unspecified)
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#(London_Marathon_2026, distance = 5)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" alt="" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
