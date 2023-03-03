
<!-- README.md is generated from README.Rmd. Please edit that file -->

# plutor

<!-- badges: start -->

[![R-CMD-check](https://github.com/william-swl/plutor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/william-swl/plutor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Features

- A template of R package
- Imported `%>%`
- `lintr` config
- `.gitignore` template, `.Rbuildignore`

## Use

- Download and unzip this repository
- Rename the directory as `newpkg/`
- In `newpkg/`, use `bash preprocess.sh newpkg` to:
  - remove `.git/` directory
  - Remove `.gitignore`, and rename `.gitignore_template` to
    `.gitignore`
  - Replace package name from `rpkg.template` to `newpkg`
- Remove `preprocess.sh`
- Init a new local git repository, and link to a remote github
  repository
- Edit `DESCRIPTION`
- Assign open source license, for MIT use `usethis::use_mit_license()`
- Add code of conduct by `usethis::use_code_of_conduct('your@email')`
- Add github actions

``` r
usethis::use_github_action("lint")
usethis::use_tidy_github_actions()
unlink('.github/workflows/pkgdown.yaml')
```

- Assign package logo by

``` r
usethis::use_logo("man/figures/logo.png", geometry='480x576')
```

- Edit `README.Rmd`, and rebuild `README.md` by

``` r
devtools::build_rmd('README.Rmd')
```

- Build website for this package by `pkgdown`, and add github actions

``` r
usethis::use_github_action("pkgdown")
```
