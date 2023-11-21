## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

* ❯ checking CRAN incoming feasibility ... [4s/12s] NOTE
  Maintainer: ‘Barry Lawson <blawson@bates.edu>’

  New submission

  Package was archived on CRAN

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2023-10-17 as email to the maintainer is
      undeliverable.

  This is a re-upload of an archived CRAN package. The corresponding author
  changed academic institutions, remembering to update his email address only
  after recognizing the package had been archived.  Apologies...
 
  The package has been updated to address previous lack of using plotmath, and
  changing @return to @returns for all exported functions.


## devtools::check_win_devel() results

* Possibly misspelled words in DESCRIPTION:
    Lehmer (17:73)
    queueing (12:56, 20:18)
    variates (14:28)

* Lehmer is the last name of D. H. Lehmer, for whom the "Lehmer"
  psuedo-random number generator is named.  An article citation
  is provided in the help for the lehmer() function.
* Both the spellings "queueing" and "queuing" are correct.
* The word "variates" is plural for (random) variate, an 
  algorithmically generated observation of a random variable.


## revdepcheck results

* There are currently no downstream dependencies for this package.

── CHECK ─────────────────────────────────────────────────────────────── 0 packages ──
OK: 0
BROKEN: 0
Total time: <1 min
