## Resubmission

The previous submission raised:

Check: Rd cross-references, Result: NOTE
   Found the following Rd file(s) with Rd \link{} targets missing package
   anchors:
     deployments.Rd: tibble
     events.Rd: tibble
     locations.Rd: tibble
     media.Rd: tibble
     observations.Rd: tibble
     taxa.Rd: tibble
   Please provide package anchors for all Rd \link{} targets not in the package itself and the base packages.
   
Those links have now been fixed and the referenced package tibble is added to `Suggests`.

## R CMD check results

0 errors | 0 warnings | 0 notes
