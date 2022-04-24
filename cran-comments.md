## Version 1.0.1

Updated method to resolve length > 1 in coercion to logical error.

## Resubmission #3

Package cleared automated checks. Issues within review by Gregor Seyer addressed as follows:

> Please add a few more details about the package functionality and implemented methods in your Description text.

__Done.__

> If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

__This package doesn't contain any references.__

> Please add some small executable examples in your Rd-files to illustrate the use of the exported function but also enable automatic testing.

__Done.__

## Resubmisson #2

* Package did not pass automated checks due to warning from [`devtools::check_win`]: no examples, no tests, no vignettes. 
* Added automated tests to resolve the warning.

## Resubmission #1

Confirmation email never received. Previously used my company email in Description, which may be blocking email; updated to personal email.

## Initial Submission

### Test environments

* local windows install, R 4.1.2
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R-release
* Microsoft Windows Server 2019 10.0.17763 (on github actions) R 3.6.3
* macOS 11.6.2 (on github actions) R-release
* ubuntu 18.04 (on github actions), R-devel, R-release, R-previous 4 versions

### R CMD check results

0 errors | 0 warnings | 0 notes

R CMD check succeeded
