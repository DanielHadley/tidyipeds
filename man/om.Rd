\name{om}
\alias{om}

\title{Outcome Measures: OM}

\description{
This table contains award and enrollment data from degree-granting institutions on four cohorts and eight subcohorts of undergraduates who entered an institution 8-7 years prior to curent year at three points in time:
\itemize{
  \item four-year (August 31, 4 years prior to current)
  \item six-year (August 31, 2 years prior to current)
  \item eight-year (August 31, current year).
}
}

\section{Cohorts}{
The cohorts of degree/certificate-seeking undergraduates are:
\itemize{
  \item (1) First-time full-time entering;
         (a) Pell-recipient
         (b) Non-Pell recipient
  \item (2) First-time part-time entering
         (a) Pell-recipient
         (b) Non-Pell recipient
  \item (3) Non-first-time full-time entering
         (a) Pell-recipient
         (b) Non-Pell recipient
  \item (4) Non-first-time part-time entering
         (a) Pell-recipient
         (b) Non-Pell recipient
}
All degree/certificate-seeking undergraduate students entering the institution during the 12-month period July 1, 8 years prior to current and June 30, 7 years prior to current are reported in one of the subcohorts.

NOTE: For students earning multiple awards, only the highest award conferred to the student by the status point is reported.
}

\section{Data Layout}{
This table has multiple records per institution.  Records are uniquely defined by the variables IPEDS ID (UNITID), and the cohort (OMCHRT) described above.  Each record (cohort) will contain the revised and adjusted size of the cohorts, the number and percent that completed and the number and percent still enrolled.
}


\section{Glossary}{
\describe{

\item{Adjusted cohort [OMACHRT]}{Adjusted cohort is the revised cohort minus exclusions}

\item{Associate's degree}{An award that normally requires at least 2 but less than 4 years of full-time equivalent college work.}

\item{Bachelor's degree}{An award (baccalaureate or equivalent degree, as determined by the Secretary, U.S. Department of Education) that normally requires at least 4 but not more than 5 years of full-time equivalent college-level work. This includes all bachelor's degrees conferred in a 5-year cooperative (work-study) program. A cooperative plan provides for alternate class attendance and employment in business, industry, or government; thus, it allows students to combine actual work experience with their college studies. Also includes bachelor's degrees in which the normal 4 years of work are completed in 3 years.}

\item{Certificate}{A formal award certifying the satisfactory completion of a postsecondary education program.}

\item{Entering students (undergraduate)}{Students at the undergraduate level, both full-time and part-time, coming into the institution for the first time using a full-year cohort. Entering students are identified as those students who entered the institution between July 1 of one year and June 30 of the following year.  This includes all first-time undergraduate students, students, who transfer into the institution at the undergraduate level for the first time, and non- degree/certificate seeking undergraduates.}

\item{Exclusions to cohort [OMEXCLS]}{The total number of students who left institution after the point of entry through August 31 of current year for one of the following allowable reasons:
\itemize{
  \item The student is deceased or is totally and permanently disabled and thus unable to return to school.
  \item The student left school to serve in the armed forces or was called to active duty. (DO NOT include students already in the military who transfer to another duty station).
  \item The student left school to serve with a foreign aid service of the Federal Government, such as the Peace Corps.
  \item The student left school to serve on an official church mission.
}

Students who leave the institution for one of the reasons noted above, but return prior to the 8-year status point, may be subtracted from the cohort.}

\item{First-time entering (undergraduate)}{A student who has no prior postsecondary experience (except as noted below) attending any institution for the first time at the undergraduate level. This includes students enrolled in academic or occupational programs. It also includes students enrolled in the fall term, who attended college for the first time in the prior summer term, and students who entered with advanced standing (college credits earned before graduation from high school).}

\item{Full-time (undergraduate)}{A student enrolled for 12 or more semester credits, or 12 or more quarter credits, or 24 or more contact hours a week each term.}

\item{Full-year cohort [OMRCHRT]}{Counts all students who entered the institution between July 1 of one year and June 30 of the next year.}

\item{Non-first-time entering (undergraduate)}{A student who has prior postsecondary experience before attending the reporting IPEDS institution.}

\item{Number of adjusted cohort receiving an award}{The number of students from the adjusted cohort who have received an award (degree or certificate) from reporting institution by the status point (August 31 of the year point).}

\item{Number of adjusted cohort still enrolled at your institution at 8 years [OMENRYI]}{The number of entering students from the adjusted cohort who have not earned an award and still enrolled at the reporting institution through August 31 of current yeae. Included are students who immediately enroll in the next term following August 31 of current year at the reporting institution.}

\item{Number of adjusted cohort who did not receive an award from your institution at 8 years [OMNOAWD]}{The total number of entering students who did not receive an award (degree or certificate) from reporting institution by the status point (August 31 of current year; 8 years after entry) is calculated by summing three columns: number still enrolled at the reported institution (OMENRYI), number who subsequently enrolled at another institution (OMENRAI), and number of students whose subsequent enrollment status is unknown (OMENRUN). For students who have earned multiple awards at the reporting institution, students are counted once based on the highest award conferred.}

\item{Number of adjusted cohort who enrolled subsequently at another institution at 8 years [OMENRAI]}{The number of entering students from the adjusted cohort, who did not earn an award, transferred out of the reporting institution, and subsequently enrolled in another institution anytime between time of entry through August 31 of current year. This only includes students for whom the reporting institution can confirm subsequent enrollment.}

\item{Number of adjusted cohort whose subsequent enrollment status is unknown at 8 years [OMENRUN]}{This variable is calculated by subtracting the sum of the number still enrolled at reporting institution (OMENRYI), number who subsequently enrolled at another institution (OMENRAI), and the number awarded by the reporting institution (OMAWDN8) from the adjusted cohort (OMACHRT).}

\item{Part-time (undergraduate)}{A student enrolled for either less than 12 semester or quarter credits, or less than 24 contact hours a week each term.}

\item{Percent of adjusted cohort  enrolled subsequently at another institution at 8 years [OMENRAP]}{This variable is the proportion of students who left the reporting institution and subsequently enrolled at another institution. These students are still enrolled through August 31, of current year at another institution. The number who subsequently enrolled at another institution (OMENRAI) is divided by the adjusted cohort  (OMACHRT).}

\item{Percent of adjusted cohort  enrollment status unknown at 8 years [OMENRUP]}{This variable is the proportion of adjusted cohort who did not receive an award (degree or certificate) from reporting institution and whose enrollment status is unknown by the status point (August 31 of current year; 8 years after entry). Enrollment unknown (OMENRUN) is calculated by subtracting the sum of the number still enrolled at reporting institution (OMENRYI), number who subsequently enrolled at another institution (OMENRAI), and the number awarded by the reporting institution  (OMAWDN8) from the adjusted cohort (OMACHRT). The enrollment unknown (OMENRUN is divided by the adjusted cohort (OMACHRT).}

\item{Percent of adjusted cohort receiving an award}{This variable is the proportion of students from the adjusted cohort who received an award (degree or certificate) from the reporting institution for each entering cohort and subcohort. Number of students receiving an award from the reporting institution through August 31 of the year point divided by the adjusted cohort.}

\item{Percent of adjusted cohort still enrolled at your institution at 8 years [OMENRYP]}{This variable is the proportion of entering students who did not receive an award (degree or certificate) and are still enrolled by the status point (August 31 of current year; 8 years after entry) at the reporting institution. Number still enrolled at the reporting institution (OMENRYI) is divided by the adjusted cohort (OMACHRT).}

\item{Percent of adjusted cohort still or subsequently enrolled at 8 years [OMENRTP]}{This variable is the proportion of entering students who did not receive an award (degree or certificate) from reporting institution and are still enrolled by the status point (August 31 of current year; 8 years after entry) either at the reporting institution or at another institution. The sum of data from the number still enrolled at the reporting institution (OMENRYI) and number who subsequently enrolled at another institution (OMENRAI) are divided by the adjusted cohort (OMACHRT).}
  }
}

