# Get taxa

Gets the (unique) scientific names and associated taxonomic information
from the observations of a Camera Trap Data Package object. Duplicate
taxa (i.e. with the same `scientificName`) are removed, retaining the
taxon with (first) a `taxonID` and (second) the most taxonomic
information.

## Usage

``` r
taxa(x)
```

## Arguments

- x:

  Camera Trap Data Package object, as returned by
  [`read_camtrapdp()`](https://inbo.github.io/camtrapdp/reference/read_camtrapdp.md).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
data frame with the taxonomic information, containing at least a
`scientificName` column.

## See also

Other accessor functions:
[`contributors()`](https://inbo.github.io/camtrapdp/reference/contributors.md),
[`deployments()`](https://inbo.github.io/camtrapdp/reference/deployments.md),
[`events()`](https://inbo.github.io/camtrapdp/reference/events.md),
[`individuals()`](https://inbo.github.io/camtrapdp/reference/individuals.md),
[`locations()`](https://inbo.github.io/camtrapdp/reference/locations.md),
[`media()`](https://inbo.github.io/camtrapdp/reference/media.md),
[`observations()`](https://inbo.github.io/camtrapdp/reference/observations.md)

## Examples

``` r
x <- example_dataset()
taxa(x)
#> # A tibble: 10 × 5
#>    scientificName     taxonID  taxonRank vernacularNames.eng vernacularNames.nld
#>    <chr>              <chr>    <chr>     <chr>               <chr>              
#>  1 Anas platyrhynchos https:/… species   mallard             wilde eend         
#>  2 Anas strepera      https:/… species   gadwall             krakeend           
#>  3 Ardea              https:/… genus     great herons        reigers            
#>  4 Ardea cinerea      https:/… species   grey heron          blauwe reiger      
#>  5 Aves               https:/… class     bird sp.            vogel              
#>  6 Homo sapiens       https:/… species   human               mens               
#>  7 Martes foina       https:/… species   beech marten        steenmarter        
#>  8 Mustela putorius   https:/… species   European polecat    bunzing            
#>  9 Rattus norvegicus  https:/… species   brown rat           bruine rat         
#> 10 Vulpes vulpes      https:/… species   red fox             vos                
```
