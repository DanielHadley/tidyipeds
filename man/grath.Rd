\name{grath}
\alias{grath}

\title{Graduation Rates: GRATH}

\description{
This file contains the graduation rate status as of August 31 of the current year for the subcohort of full-time, first-time degree/certificate-seeking undergraduates receiving athletically-related student aid for specified sports in both four year and two year institutions.

Data include the number in the  cohort, the number completed a degree/certificate by type of award, and number of transfer-outs. Sports include football, basketball, baseball, cross country/track and all other sports combined.
}

\section{Data Layout}{
This file has multiple records per institution.  Records are uniquely defined by the variables IPEDS ID (UNITID), sport subcohort (SPORT) and graduation status within the cohort (GRTYPE).  Each record will contain the above data by gender and race/ethnicity.
}

\section{Sport}{
Graduation rate data are reported separately for the following sports:
\itemize{
  \item Football;
  \item Basketball;
  \item Baseball;
  \item Cross-country and track combined;
  \item All other sports combined.
  }

If a student receives athletically-related student aid for MORE THAN ONE SPORT, the student is counted only ONCE, using the following sports hierarchy: football; basketball; baseball; cross-country and track combined; and all other sports combined. For example, if a student receives athletically-related aid for playing football and baseball, information for this student was provided only under the sport of football.
}

\section{Note}{
Perturbation procedures were applied to these data to protect against disclosure of individual information.
}


\section{Glossary}{
\describe{
\item{Athletic Subcohort}{Those students in the revised cohort who received athletically-related student aid to play a specific sport}

\item{Bachelor's degree}{An award (baccalaureate or equivalent degree, as determined by the Secretary, U.S. Department of Education) that normally requires at least 4 but NOT more than 5 years of full-time equivalent college-level work. This includes ALL bachelor's degrees conferred in a 5-year.}

\item{Cohort data [GRTYPE]}{Cohort status defines the graduation status students of the cohort.}

\item{Completers within 150 percent (of normal time)}{Students (within a cohort or subcohort) who completed their program within 150 percent of the normal time to completion.}

\item{Cooperative (Work-Study plan) program}{A cooperative plan provides for alternate class attendance and employment in business, industry, or government; thus, it allows students to combine actual work experience with their college studies. Also, includes bachelor's degrees in which the normal 4 years of work are completed in 3 years.}

\item{Exclusions}{ Students from the cohort or subcohort who left the institution for any of the following reasons: died or were totally and permanently disabled; to serve in the armed forces; to serve with a foreign aid service of the Federal Government, such as the Peace Corps; or to serve on official church missions.}

\item{Normal time to completion}{ The amount of time necessary for a student to complete all requirements for a degree or certificate according to the institution's catalog. This is typically 4 years (8 semesters or trimesters, or 12 quarters, excluding summer terms) for a bachelor's degree in a standard term-based institution; 2 years (4 semesters or trimesters, or 6 quarters, excluding summer terms) for an associate's degree in a standard term-based institution; and the various scheduled times for certificate programs. }

\item{Sport [SPORT]}{Graduation rate data are reported separately for the following sports: (1) Football; (2) Basketball; (3) Baseball; (4) Cross-country and track combined; and (5) All other sports combined. }

\item{Total transfer-our students (Non-Completers)}{Total number of students from the cohort or subcohort who transferred out at any time within 150 percent of normal time to completion. A transfer-out student is a student who has not completed or graduated from the program in which he or she was enrolled, but who has subsequently enrolled at another eligible institution in any program for which the reporting institution provided substantial preparation.}
  }
}
