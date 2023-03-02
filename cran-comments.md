# resubmission 2 simaerep 0.4.3
- deleted unit tests which compared function performance when called with different parameters as requested.

Thanks, we see you added

   skip_on_cran() # performance on ci/cd systems is not predictable

but it should not be doing the test anywhere, as WRE explains.

Best,
Uwe Ligges

# resubmission 1 simaerep 0.4.3

- fix CRAN url in README

- Fix to address issue of failing unit test on CRAN which compared function performance when called with different parameters. This test is now skipped on CRAN. 

Dear maintainer,

Please see the problems shown on
<https://cran.r-project.org/web/checks/check_results_simaerep.html>.

Please correct before 2023-03-11 to safely retain your package on CRAN.

Do remember to look at the 'Additional issues'.

The CRAN Team

## Test Results

No notes, warnings or errors


## Test Environments

- Rhub, debian
- GitHub CI/CD macOS-latest




