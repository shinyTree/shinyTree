## Submission

This is an update of shinyTree from 0.3.1 (published 2023-08-07) to 1.0.0.
The maintainer and the licence are unchanged.

The major version marks new features rather than removed ones: no exported
function or argument has been removed, and the 5 reverse dependencies pass
(see below). The minimum versions are now R >= 3.3.0 and shiny >= 1.6.0,
which the code already needed. See NEWS.md for the changes.

Bundled third-party code (jsTree 3.3.7, its "proton" theme, and that theme's
Titillium Web font) is credited in Authors@R and listed, with its licence and
source, in inst/COPYRIGHTS. The unminified jstree.js is included alongside
jstree.min.js.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Red Hat Enterprise Linux 9.8, R 4.5.2
* GitHub Actions: ubuntu-latest (R-devel, R-release, R-oldrel-1),
  macos-latest (R-release), windows-latest (R-release)
* win-builder: R-devel, R-release, R-oldrelease

## Reverse dependencies

We checked the 5 reverse dependencies on CRAN (Certara.DarwinReporter,
Certara.ModelResults, OmopViewer, sdcHierarchies, teal.modules.general),
comparing R CMD check results with the CRAN version and with this version.

* We saw 0 new problems.
* We failed to check 0 packages.
