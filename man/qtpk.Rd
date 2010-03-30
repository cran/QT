\name{qtpk}
\docType{data}
\alias{qtpk}
\title{Generic data set for the QT Tool}
\description{Example data set for use with the QT library.}
\usage{qtpk}
\format{
       \tabular{rll}{
         [,1] \tab subjid    \tab Unique subject identifier (numeric or character) (required) \cr
         [,2] \tab treat     \tab Treatment group (numeric or character) (required) \cr
         [,3] \tab period    \tab Treatment period for crossover studies (numeric or character) (optional)  \cr
         [,4] \tab day       \tab Day relative to first dose (within period for crossove studies) (numeric) (required)\cr
         [,5] \tab time      \tab Time (hr) relative to dose (numeric) (required)\cr
         [,6] \tab qt        \tab QT interval (ms) (numeric) (required)\cr
         [,7] \tab qtcb      \tab Bazett's corrected QT interval (ms) (numeric) (required)\cr
         [,8] \tab qtcf      \tab Fridericia's corrected QT interval (ms) (numeric) (required)\cr
         [,9] \tab hr        \tab Heart rate (bpm) (numeric) (required)\cr
         [,10] \tab rr       \tab RR interval (ms) (numeric) (required)\cr
         [,11] \tab qt.bs    \tab Baseline QT interval (ms) (numeric) (required)\cr
         [,12] \tab qtcb.bs  \tab Baseline Bazett's corrected QT interval (ms) (numeric) (required)\cr
         [,13] \tab qtcf.bs  \tab Baseline Fridericia's corrected QT interval (ms) (numeric) (required)\cr
         [,14] \tab hr.bs    \tab Baseline heart rate (bpm) (numeric) (required)\cr
         [,15] \tab rr.bs    \tab Baseline RR interval (ms) (numeric) (required)\cr
         [,16] \tab qt.cfb   \tab QT interval (change from baseline) (ms) (numeric) (required)\cr
         [,17] \tab qtcb.cfb \tab Bazett's corrected QT interval (change from baseline) (ms) (numeric) (required)\cr
         [,18] \tab qtcf.cfb \tab Fridericia's corrected QT interval (change from baseline) (ms) (numeric) (required)\cr
         [,19] \tab hr.cfb   \tab Heart rate (change from baseline) (ms) (numeric) (required)\cr
         [,20] \tab rr.cfb   \tab RR interval (change from baseline) (ms) (numeric) (required)\cr
         [,21] \tab conc     \tab Parent drug concentration (numeric) (optional) \cr
         [,22] \tab moxi     \tab Moxifloxacin concentration (numeric) (optional) \cr
         [,23] \tab sex      \tab Gender (controlled term, M=male, F=female) (required)\cr
       }
}
\note{
  For use with \link{QT}.  Additional optional columns (meta, weight, height, age, Race, etc.) are described under help(QT).
}

\author{Christoffer W. Tornoe \email{pharmacometrics@fda.hhs.gov}}
\note{DISCLAIMER: This R package does not reflect the official policy of the FDA. No official endorsement by the FDA is intended or should be inferred.}
\keyword{models}
