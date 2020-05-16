## Resubmission
*Added a reference in the Description.
*Created runnable examples based on files in inst/extdata. For boxer() and boxer2() I have left these as \dontrun{} because those functions call grDevices::x11() and result in an error "screen devices should not be used in examples..."
*replaced instances where I used cat() with message().


## Test environments
* local OS 10.15.4 installation, R 4.0.0
* ubuntu 16.04.6 (on travis-ci), R 4.0.0
* fedora linux (via rhub)
* win-builder (devel and release)

win devel resulted in a loadNamespace error, "there is no package called 'tiff'." There appear to be no win binaries for r-devel for this package. https://cran.r-project.org/web/packages/tiff/index.html


## R CMD check results

0 errors | 0 warnings | 1 notes

* checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
    'out_SW1.jpg'
    
    This is the output file from the outbox() example. outbox() requires an       output directory in the call and I have set the default to getwd() to make     the example runnable.

* This is a new release. 
