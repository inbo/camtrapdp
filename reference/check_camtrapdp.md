# Check a Camera Trap Data Package object

Checks if an object is a Camera Trap Data Package object with the
required properties.

## Usage

``` r
check_camtrapdp(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

## Value

`x` invisibly or an error.

## Examples

``` r
x <- example_dataset()
check_camtrapdp(x) # Invisible return of x if valid
```
