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
Martina Schmirl in the name of the CRAN team in her email from the 01.09.2019. Thank you for your feedback.
The changes are as follows:

* Changed and shortend the title as requested; changed description as requested.
* Added \\value field for the report function.
* Information is now written to the console via the `message()` function rather than `print()` or `cat()`.
* I no longer change the user's settings.
* An issue about writing to the user's filespace was raised. I solved that issue as follows:
    - The functions `igate`, `igate.regressions` and `categorical.freqplot` by default only
    plot graphs without saving them to the file space. Only if the user uses the option `savePlots = TRUE`
    (default is `FALSE`),
    the plots will be saved to disk, by default into `tempdir()`. The plots will only be saved to the user's
    file space if they explicitly ask for it by using `savePlots = TRUE` and
    also specifying a directory they want to save the plots into (via the `image_directory` argument).
    - The purpose of the function `report` is to create an html report of the conducted analysis. This function now 
    will only execute if the user explicitly specifies a directory they want to save the report into (via the
    `output_directory` option).
    
  The above measures are similar to how functions such as `save` from the base package or `write_csv` from the readr
  package write to the user's file space and therefore I hope this is a legitimate way of obtaining the user's
  permission to write to their file space.
* Use of \\dontrun{} in the examples of `report`: The new version of `report` needs an output directory as argument which depends on the file space structure of the user's machine. Hence \\dontrun{} is necessary.



