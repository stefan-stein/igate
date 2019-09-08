## Test environments
* local OS X install, R 3.6.0
* ubuntu 16.04.6 (on travis-c), R 3.6.1
* winbuilder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. 


There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Stefan Stein <s.stein@warwick.ac.uk>’

Days since last update: 1

In this update I address the issues that were raised by Brian Ripley after my package was initially uploaded to CRAN. These issues came to light after uploading the package to CRAN and checking it on additional platforms (details below), hence the short time window between updates. Thank you for bringing these issues to my attention.

## Changes

When testing igate on CRAN, two errors occured:

* On r-patched-solaris-x86: My package requires pandoc to use the function `report` which seems not to be installed on this system and thus an error occured when running the example for `report`. I added 'SystemRequirements: pandoc (>= 1.12.3) - http://pandoc.org' to the DESCRIPTION file. Sorry about not having done this before.
* On r-devel-linux-x86_64-debian-gcc: When running the example of `report` an error appeared: 'Warning in file(con, "w") : cannot open file 'iGATE_Report.knit.md': Read-only file system Error in file(con, "w") : cannot open the connection'. This error occurs when `report` is calling `rmarkdown::render`, because when knitting files with rmarkdown some temporary files are written to the user's filespace and then removed again once the knitting finishes. I addressed this issue by passing `intermediates_dir = tempdir()` and `knit_root_dir = tempdir()` to the call of `rmarkdown::render`. Now all temporary files are written to `tempdir()` and the knitting process takes place in there as well. At no point are files written permanently to the user's filespace unless with their explicit permission.




