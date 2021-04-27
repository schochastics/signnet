# Minor update from 0.7.0 to 0.7.1

fixed some bugs in various functions

## Test environments
* ubuntu 20.04, R 4.0.2
* win-builder (devel and release)

# Update from 0.6.0 to 0.7.0

added some new functions


# Update from 0.5.3 to 0.6.0

added a new dataset and sped up some algorithms. No user facing changes.

# Update from 0.5.2 to 0.5.3

Some bugfixes without user facing changes

# Update from 0.5.1 to 0.5.2

This update fixes an issue with stringsAsFactors in `as_signed_proj()` that occurs in rdevel (as requested by K. Hornik).


# Update from 0.5.0 to 0.5.1

This update fixes an issue on solaris (as requested by B. Ripley) :  
`circArc.cpp:7:22: error: call of overloaded ‘acos(int)’ is ambiguous`

# Resubmission 3 of initial submission

> \dontrun{} should only be used if the example really cannot be executed

I explained why I wrapped the call in dontrun. But I switched to an example now which doesn't require dontrun.

> Please add references describing the methods in your package also to the 
description field of your DESCRIPTION

I added a paper for each topic area.

# Resubmission 2 of initial submission 

> Shouldn't the year in your license file be updated?

fixed

> Some code lines in examples are commented out.

Example in `eigen_centrality_signed()` was wrapped in `\dontrun{}` (should not be called by users)

# Resubmission 1 of initial submission

>The Description field should not start with the package name, 'This package' or similar.

fixed

>Is there some reference about the method you can add in the Description
>field in the form Authors (year) <doi:.....>?

The package brings together several methods from many different authors. I would not want to single one out in the DESCRIPTION


# Initial submission

## Test environments
* ubuntu 18.04, R 3.6.2
* ubuntu 14.04 (on travis-ci), R 3.6.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
