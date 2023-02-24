# Resubmission of simaerep

Thank you for taking the time to review my submission. I have addressed all of your
comments.

## DESCRIPTION

Please reduce the length of the title to less than 65 characters.

*done*

Please do not start the description with "This package", package name,
title or similar.
I would start with: "Detect clinical trial sites..."

*done*

Please always add all authors, contributors and copyright holders in the
Authors@R field with the appropriate roles.

*I have added Hoffmann La Roche to the Authors@R field and assigned cph*
*role to the institution and myself. This reflects our internal policy.*
*as stated in the licence file.*
*COPYRIGHT HOLDER: F. Hoffmann-La Roche Ltd and simaerep authors*
*This covers copyright interest of potential future external collaborators.*

## Examples

Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.
*done, with one exception in the sim_studies() example show casing parallel*
*processing which makes changes to the user's environment.*

Please replace if(interactive()){} with \donttest or unwrap the
examples.
*done, with one exception, with_progress_cnd() the progressbar is only visible*
*in interactive mode*

## Console Messages

R/lint.R

*lint_package() has been declared an internal function, print() calls have been*
*exchanged for message() calls*

R/simaerep_plot.R
*print() calls have been exchanged for message() calls that inform user that*
*certain element of the plot cannot be generated*

## Seed
R/simaerep_plot.R
*seed can now be passed as parameter, fixed seed is desired so that the generated*
*plot always looks the same*




# First Submission of simaerep

## Test Results

1 Note

* checking CRAN incoming feasibility ... [11s] NOTE
Maintainer: 'Bjoern Koneswarakantha <bjoern.koneswarakantha@roche.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  AE (11:92, 12:52, 12:82)
  Koneswarakantha (13:65)
  
## Test Environments

- Rhub, Windows Server 2022, R-devel, 64 bit
- GitHub CI/CD macOS-latest
- winbuilder release
- winbuilder devel


## Comments
I confirm that AE and Koneswarakantha in DESCRIPTION are not misspelled.
