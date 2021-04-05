
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wreportr <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/cramirezs/wreportr/workflows/R-CMD-check/badge.svg)](https://github.com/cramirezs/wreportr/actions)
<!-- badges: end -->

The goal of [wreportr](https://cramirezs.github.io/wreportr/index.html)
is to make a summary of your pipeline easier.

The central concept is having a set of “steps” your analysis follows,
but it is not limited to a sequential design.

## Installation

You can install the developing version of wreportr from
[GitHub](https://github.com/cramirezs/wreportr) with:

``` r
devtools::install_github("cramirezs/wreportr", ref = "main")
```

## Examples

There are several [ways](vignette/) of generating your HTML report, but
the input essentially looks like this:

``` yaml
---
title: "My side project"
output: html_document
steps:
  metadata: /Users/ciro/Documents/liai/covid19/part2/metadata_library.csv
  step_1: /Users/ciro/Documents/liai/covid19/literature
  step_2: /Users/ciro/Documents/liai/covid19/part1
  step_3: /Users/ciro/Documents/liai/covid19/part2
  step_4: /Users/ciro/Documents/liai/published/covid19
...
```
