# Get Camtrap DP version

Extracts the version number used by a Camera Trap Data Package object.
This version number indicates what version of the [Camtrap DP
standard](https://camtrap-dp.tdwg.org) was used.

## Usage

``` r
version(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).
  Also works on a Frictionless Data Package, as returned by
  [`frictionless::read_package()`](https://docs.ropensci.org/frictionless/reference/read_package.html).

## Value

Camtrap DP version number (e.g. `1.0`).

## Details

The version number is derived as follows:

1.  The `version` attribute, if defined.

2.  A version number contained in `x$profile`, which is expected to
    contain the URL to the used Camtrap DP standard.

3.  `x$profile` in its entirety (can be `NULL`).

## Examples

``` r
x <- example_dataset()
version(x)
#> [1] "1.0.2"
```
