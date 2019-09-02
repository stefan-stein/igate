## Test environments
* local OS X install, R 3.6.0
* local Windows 7 install R 3.6.0

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
Martina Schmirl in the name of the CRAN team in her email from the 01.09.2019:

* Changed and shortend the title as requested; changed description as requested
* Added \value field for the report function
* Changed \dontrun to \donttest in the examples section of the report function
* Information is now written to the console via the message() function rather than print(). This affects functions
  - categorical.igate
  - igate
  - igate.regressions
  - robust.categorical.igate




