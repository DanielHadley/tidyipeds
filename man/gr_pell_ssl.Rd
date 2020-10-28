\name{gr_pell_ssl}
\alias{gr_pell_ssl}

\title{Graduation Rates: GR_PELL_SSL}

\description{
This file contains the graduation rate status as of August 31, 2018 for three subcohorts of full-time, first-time degree/certificate-seeking undergraduates.
}

\section{Subcohorts}{
 The three subcohorts are:
 \itemize{
  \item Pell grant recipients,
  \item Subsidized Stafford Loan recipients not receiving Pell Grants,
  \item students not receiving Pell grants or subsidized Stafford loans.
  }
The number who completed in 150 percent of normal time are included for the subcohorts.
}

\section{Four-Year Institutions}{
In four year institutions each of the subcohorts will include:
\itemize{
  \item the number of bachelor degree-seeking students who were enrolled 6 years prior to the current year
  \item the number of bachelor degree seeking students who completed any degree/certificate within 150 percent of normal time
  \item the number who completed a bachelor's degree within 150 percent on normal time}

Data for students seeking a degree/certificate other than a bachelor's degree are also included for four year institutions.
}

\section{Two-Year and less-than 2-year Institutions}{
Data for two year and less-than 2-year institutions include:
\itemize{
  \item the number of full-time, first-time students who were enrolled 3 years prior to the current year
  \item the number of students who completed any degree/certificate 150 percent of normal time
  }
}

\section{Data Layout}{
This file has multiple records per institution.  Records are uniquely defined by the variables IPEDS ID (UNITID), and the cohort  (PSGRTYPE, see frequencies for details).  Each record will contain  data for each of the financial aid subcohorts.
}


\section{Glossary}{
\describe{
  \item{150 percent of normal time to complete }{
  Students who completed their program within 150 percent of the normal (or expected) time for completion.}

  \item{Bachelor's or equivalent degree-seeking subcohort}{
  A cohort of students who were seeking a bachelor's or equivalent degree upon entry.}

  \item{Cohort type [PSGRTYPE] }{
  This variable describes the four cohorts contained in this data table.
  \itemize{
  \item  Full-time, first-time, degree/certificate-seeking cohort that entered in 2011. This is the sum of the bachelor's and other than bachelor's degree seeking cohort.  (4-year nstitutions)
  \item  Full-time first-time bachelor's or equivalent degree-seeking subcohort that entered in 2012. (4-year institutions)
  \item  Full-time first-time other degree/certificate-seeking subcohort that entered in 2012.  (4-year institutions)
  \item  Full-time, first-time, degree/certificate-seeking cohort that entered in 2015. (2-year and less-than 2-year institutions)}
  }

  \item{Did not receive Pell grant or subsidized Stafford loan - adjusted  cohort (revised minus exclusions) [NRADJCT]}{The number of students not receiving a Pell grant or subsidized Stafford loan in the revised cohort minus the number of exclusions.}

\item{Did not receive Pell grant or subsidized Stafford loan - exclusions [NREXCLU]}{Exclusions from students not receiving a Pell grant or subsidized Stafford loan cohort within 150 percent of normal time

NOTE: Students who leave the institution for one of the reasons described in "Exclusions", but return prior to the 150 percent status date of August 31, may still be reported as exclusions.}

\item{Did not receive Pell grant or subsidized Stafford loan - number that completed a bachelor's or equivalent degree within 150 percent of normal time [NRCMBAC]}{The number of students not receiving a Pell grant or subsidized Stafford loan of the cohorts specified:  total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree in 150 percent of normal time 6-years (4-year institutions only) }

\item{Did not receive Pell grant or subsidized Stafford loan - number that completed an award of less than 4 academic years within 150 of normal time [NRCMOBA]}{This is the number of students not receiving a Pell grant or subsidized Stafford loan of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed  other than a bachelor's degree including associate degrees or certtificates of less-than 4-years in 150 percent of normal time (4-year institutions); or  students not receiving a Pell grant or subsidized Stafford loan in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Did not receive Pell grant or subsidized Stafford loan - revised cohort [NRREVCT]}{Number of students in the cohort/subcohort identified by the cohort type (PSGRTYPE), who did not receive a Pell grant or a subsidized Stafford loan in the cohort year.}

\item{Did not receive Pell grant or subsidized Stafford loan - total number that completed an award  within 150 percent of normal time [NRCMTOT]}{This is the number of  students not receiving a Pell grant or subsidized Stafford loan of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort(PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree or other than bachelor's degree in 150 percent of normal time (4-year institutions); or the number of students not receiving a Pell grant or subsidized Stafford loan in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

  \item{Full-time first-time student (undergraduate)}{
  A student who has no prior postsecondary experience (except as noted below) attending any institution for the first time at the undergraduate level. This includes students enrolled in academic or occupational programs. It also includes students enrolled in the fall term who attended college for the first time in the prior summer term, and students who entered with advanced standing (college credits earned before graduation from high school).

  Full-time undergraduate students are enrolled for 12 or more semester credits, or 12 or more quarter credits, or 24 or more contact hours a week each term.}

  \item{Normal time }{
  The amount of time necessary for a student to complete all requirements for a degree or certificate according to the institution's catalog. This is typically 4 years (8 semesters or trimesters, or 12 quarters, excluding summer terms) for a bachelor's degree in a standard term-based institution; 2 years (4 semesters or trimesters, or 6 quarters, excluding summer terms) for an associate's degree in a standard term-based institution; and the various scheduled times for certificate programs.}

  \item{Other degree/certificate-seeking subcohort }{
  A subset of students belonging to a GR cohort who were seeking a degree or certificate other than bachelor's degree upon entry. }

  \item{Pell Grant recipients - adjusted  cohort [PGADJCT]}{
  The number of Pell grant recipients in the revised cohort minus the number of exclusions. }

  \item{Pell Grant recipients - exclusions [PGEXCLU] }{
  Exclusions from the Pell grant cohort within 150 percent of normal time.

  NOTE: Students who leave the institution for one of the reasons described in "Exclusions", but return prior to the 150 percent status date of August 31, may still be reported as exclusions. }

  \item{Pell Grant  recipients - number that completed a bachelor's or equivalent degree within 150 percent of normal time [PGCMBAC]}{
  This is the number of Pell grant recipients of the cohorts specified:  total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree in 150 percent of normal time 6-years (4-year institutions only)}

  \item{Pell Grant  recipients - number that completed an award of less than 4 academic years or equivalent within 150 percent of normal time [PGCMOBA]}{
  This is the number of Pell grant recipients of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed  other than a bachelor's degree including associate degrees or certificates of less-than 4-years in 150 percent of normal time (4-year institutions); or the number of Pell grant recipients in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete.}

  \item{Pell Grant recipients - revised cohort [PGREVCT]}{
Number of students in the cohort/subcohort identified by the cohort type (PSGRTYPE), who were recipients of a Pell Grant in the cohort year.

Recipients are defined as those students receiving and using their Pell Grant.  These students must receive the award upon entry into the institution.  Students that are recipients of an award at a later point after entry, are not included.}

  \item{Pell Grant  recipients - total number that completed an award  within 150 percent of normal time [PGCMTOT]}{
  This is the number of Pell grant recipients of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort(PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree or other than bachelor's degree in 150 percent of normal time (4-year institutions); or the number of Pell grant recipients in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - adjusted cohort (revised minus exclusions) [SSADJCT]}{The number of subsidized Staff loan recipients who did not receive a Pell Grant in the revised cohort minus the number of exclusions.}

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - exclusions [SSEXCLU]}{Exclusions from the subsidized Stafford loan who did not receive a Pell grant cohort within 150 percent of normal time

NOTE: Students who leave the institution for one of the reasons described in "Exclusions", but return prior to the 150 percent status date of August 31, may still be reported as exclusions.}

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - number completed a bachelor's or equivalent within 150 percent of normal time [SSCMBAC]}{The number of subsidized Stafford loan recipients who did not receive a pell grant of the cohorts specified:  total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree in 150 percent of normal time 6-years (4-year institutions only) }

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - number completed an award of less than 4 academic years within 150 percent of normal time [SSCMOBA]}{The number of subsidized Stafford loan recipients who did not receive a Pell grant of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed  other than a bachelor's degree including associate degrees or certificates of less-than 4-years in 150 percent of normal time (4-year institutions); or the number of subsidized Stafford loan recipients who did not receive a Pell grant in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - revised cohort [SSREVCT]}{Number of students in the cohort/subcohort identified by the cohort type (PSGRTYPE), who were recipients of a subsidized Stafford loan and did not receive a Pell grant in the cohort year.

Recipients are defined as those students receiving and using their subsidized Stafford loan.  These students must receive the award upon entry into the institution. Students that are recipients of an award at a later point after entry, are not included.}

\item{Subsidized Stafford Loan recipients not receiving Pell Grants - total number completed an award within 150 percent of normal time [SSCMTOT]}{This is the number of subsidized Stafford loan recipients who did not receive a Pell grant of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort(PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree or other than bachelor's degree in 150 percent of normal time (4-year institutions); or the number of subsidized Stafford loan recipients who did not receive a Pell grant in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Total adjusted cohort [TTADJCT]}{This is the number of students in the total revised cohort minus the number of exclusions.}

\item{Total exclusions [TTEXCLU]}{Exclusions from students in the total revised cohort within 150 percent of normal time.

Exclusions who left institution within 150 percent of normal time (have neither graduated nor transferred to another institution) because of one of the following documented reasons:
\itemize{
\item The student is deceased or is totally and permanently disabled and thus unable to return to school.
\item The student left school to serve in the armed forces or was called to active duty. ( students already in the military who transfer to another duty station are not included)
\item The student left school to serve with a foreign aid service of the Federal Government, such as the Peace Corps.
\item The student left school to serve on an official church mission. }

NOTE: Students who leave the institution for one of the above reasons, but return prior to the 150 percent status date of August 31, may still be reported as exclusions.}

\item{Total number that completed a bachelor's or equivalent degree within 150 percent of normal time [TTCMBAC]}{This is the number of students of the cohorts specified:  total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree in 150 percent of normal time 6-years (4-year institutions only) .}

\item{Total number that completed an award  within 150 percent of normal time [TTCMTOT]}{This is the number of  students of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort(PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed a bachelor's degree or other than bachelor's degree in 150 percent of normal time (4-year institutions); or the number of students in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Total number that completed an award of less than 4 academic years within 150 percent of normal time [TTCMOBA]}{This is the number of students of the cohorts specified: total cohort (PSGRTYPE=1), bachelor's cohort (PSGRTYPE=2), other than bachelor's cohort (PSGRTYPE=3)  that completed  other than a bachelor's degree including associate degrees or certtificates of less-than 4-years in 150 percent of normal time (4-year institutions); or  students in the cohort (PSGRTYPE=4) of two year or less-than-2-year schools that completed an award in 150 percent of normal time to complete. }

\item{Total revised cohort [TTREVCT]}{Number of students in the cohort/subcohort identified by the cohort type (PSGRTYPE)}

  }
}





