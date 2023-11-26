## Resubmission

* Corrected missing "doi:" from DOI in DESCRIPTION.  (Sorry for my oversight!)

## Resubmission

This is a resubmission to address CRAN feedback (V. Wimmer). All check results
below are for this resubmission-to-feedback.  In this resubmitted version I
have:

* Added three references with DOIs to the DESCRIPTION.

* Changed all instances of \dontrun{} to \donttest{} in thinning.R.  However,
  kept \dontrun{} for accrej.R, all i*.R functions, lehmer.R, and msq.R (only
  one instance in each function) since it wraps an example demonstrating
  interactive use, requiring user input to allow the user to step through (as
  described in our references).  Using \donttest{} in these particular cases
  would not execute correctly when using devtools::check() as no user input
  is possible.

* Changed use of print() to warning() in (internal-use) PlotContinuous.R and
  PlotDiscrete.R.  Changed uses of print() to message() in ssqvis.R.  Also
  changed use of cat() to message() in (internal-use) compPlot.R, as well as
  in msq.R, ssq.R, and ssqvis.R.

* Added on.exit() logic at top of function to reset user's par() preferences
  inside our user-facing visualization functions and helper functions: 
  PlotContinuous.R, PlotDiscrete.R, accrej.R, compPlot.R, all i*.R functions,
  lehmer.R, msq.R, ssq.R, ssqvis.R, thinning.R.  (Changing the par during the
  function execution is necessary for the layout of the animations and
  visualizations that we provide.)

* Added "oldpar <- par(no.readonly = TRUE) ...[example]... par(oldpar)" logic
  in examples where par was modified -- all i*.R functions -- whenever the
  function is plotting and therefore setting par.  Also added an additional
  restorePar logical parameter to all i*.R functions to allow the user to
  override whether the original par values are restored, permitting multiple
  visualizations (e.g., 3x3) per plot and/or drawing overlays on existing
  visualizations (e.g., segments for K-S test representation) --- these use
  cases were already in examples and are discussed in our references.

* Removed (via comment-out) options(warn = -1) and associated previous-capturing
  and subsequent-resetting code from PlotContinuous.R and PlotDiscrete.R, all
  i*.R functions, ssqvis.R, testRuns.R, and utils.R.  I changed calls to
  warning() that occur in visualization routines to use warning()'s 
  immediate. = TRUE argument.

* The offending uses of <<- littering the global scope with a "pauseData" object
  were in each of the functions below, and corrections confirmed by testing
  each function followed by ls() in the global scope, which returned
  "character(0)" for each:
    * PlotContinuous.R: Now corrected, so that pauseData is defined to only
        exist in the scope of the PlotContinuous functions (as with other
        variables there), consistent with stateful function use
        (https://adv-r.hadley.nz/function-factories.html#stateful-funs)
    * PlotDiscrete.R: Same as previous item.
    * accrej.R: Only used <<- on pauseData, now corrected as in PlotContinuous.
    * msq.R: Same as previous item.
    * ssq.R: Same as previoius item.
    * thinning.R: Same as previous item.

* Moved the functions meanTPS, sdTPS, and quantileTPS from utils.R to their own
  separate source file: tps.R.

* Moved the function sample from utils.R into generators.R, as it makes more 
  sense for it to live with set.seed et al.

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

❯ checking CRAN incoming feasibility ... [3s/11s] NOTE
  Maintainer: ‘Barry Lawson <blawson@bates.edu>’

  New submission

  Package was archived on CRAN

  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2023-10-17 as email to the maintainer is
      undeliverable.

  The Description field contains
    Lawson and Leemis (2021) <10.1109/WSC52266.2021.9715299>.
  Please write DOIs as <doi:10.prefix/suffix>.

* This is a re-upload of an archived CRAN package. The corresponding author
  changed academic institutions, remembering to update his email address only
  after recognizing the package had been archived.  Apologies...
 
* The package has been updated to address previous lack of using plotmath, and
  changing @return to @returns for all exported functions.

* After adding DOIs, the DOI note above appeared (and also in the 
  check_win_devel notes), but it is the correct DOI according to IEEE
  (the publisher): https://ieeexplore.ieee.org/document/9715299


## devtools::check_win_devel() results

* Results: https://win-builder.r-project.org/yX33k5UWcP9g/00check.log

* Possibly misspelled words in DESCRIPTION:
    Kudlay (23:5)
    Leemis (22:28, 23:25, 24:19)
    Lehmer (17:73)
    queueing (12:56, 20:18)
    variates (14:28)

* Kudlay and Leemis are the last names of the package co-authors.
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
