\name{gr_ath_aid}
\alias{gr_ath_aid}

\title{Graduation Rates: GR_ATH_AID}

\description{
This file contains the number of full-time undergraduate students receiving athletically-related student aid for specific sports in current academic year.
}

\section{Sports}{
 Sports include football, basketball, baseball, cross country/track and all other sports combined.
}


\section{Data Layout}{
This file has multiple records per institution.  Records are uniquely defined by the variables IPEDS ID (UNITID), and sport (GRTYPE) .  Each record will contain the above data by gender and race/ethnicity.
}

\section{Glossary}{
\describe{
  \item{Sport}{
  This variable identifies the sport of the number of full-time degree/certificate-seeking undergraduate students who received athletically-related student aid at any time during the current academic year.  If a student received athletically-related aid for more than ONE SPORT, the student is counted only ONCE. These numbers should account for all full-time, degree/ certificate-seeking undergraduate students receiving athletically-related student aid.}
  \item{All Other Sports Combined }{Any sport not specified separately (in the Graduation Rate data collection). Includes sports such as golf, tennis, lacross or field hockey where teams participate in intercollegiate athletics competition. Does not include cheerleading or dance teams even though the institution might award aid to students participating in these activities under the auspices of the athletic department.}
  }
}
