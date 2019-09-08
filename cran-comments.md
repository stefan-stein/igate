## Test environments
* local OS X install, R 3.6.0
* winbuilder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. 


There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Stefan Stein <s.stein@warwick.ac.uk>’
New submission

This is my first R package to be submitted to CRAN, hence the note.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

## Resubmission

This is a resubmission. In this version I addressed the issues raised by 
Martina Schmirl on behalf of the CRAN team in her email from the 05.09.2019. Thank you for your feedback.
The changes are as follows:

* Added a reference describing the implemented methods to the description field of the DESCRIPTION file.
* Removed the \\dontrun{} in the example of the `report` function. The output is now written into
`tempdir()` as requested.

