\name{sal_is}
\alias{sal_is}

\title{Instructional Staff/Salaries: SAL_IS}

\description{
This file contains the number of staff,  total salary outlays and average salaries of full-time, nonmedical, instructional staff by academic rank, number of months worked, and gender as of November 1.
}

\section{Data Layout}{
This data file has multiple records for each institution. Each record is uniquely defined by the variables IPEDS ID (UNITID), and the variable academic rank (ARANK) of full-time instructional staff.
}

\section{Characteristics: Months}{
The categories for number of months worked are 9-, 10-, 11- and 12-months.

Number of staff working less than 9 months are also available, but salary data are not available for this group.

Equated 9-month salary outlays and equated 9-month average salaries are also available by academic rank and gender.
}

\section{Amendments}{
Between 2012 and 2015 salary information were not available by number of months worked. Equated 9-month average salaries were available, but because of the lack of detailed salary data by the number of months worked, it is not consistent with equated 9-month salary data beginning with 2016.

Prior to 2012, number of staff and salary information were available for 9/10 months worked and 11/12 months worked. Equated 9-month average salaries and equated 9-month salary outlays were available and is more consistent with equated salaries beginning with 2016.

Prior to 2016 the number of months worked were referred to as contract lengths.  Beginning with 2016, the salary survey collection screens used the  "number of months worked" terminology.
}


\section{Glossary}{
\describe{

\item{Academic rank [ARANK]}{Assigned by the institution and includes Professors, Associate professors, Assistant professors, Instructors, Lecturers, and No academic rank.}

\item{Annual salary outlays (combined salaries of all staff) }{Include base salaries only – no supplements, overloads or bonuses. Additional stipends for administrative, managerial or other responsibilities are NOT included in the salary outlays data for instructional staff.}

\item{Average salary for instructional staff equated to a 9-month contract}{This is  calculated by dividing  the total salary outlays of full-time, non-medical, instructional staff  equated to month contract by the number of full-time, non-medical, instructional staff.}

\item{Contract length}{The contracted teaching period. The number of months should correspond with the number of months worked (which may differ from the number of months over which staff are paid).}

\item{Instructional Staff}{An occupational category that is comprised of staff who are either:
\itemize{
  \item Primarily Instruction (See "Instruction (only) category" )
  \item or Instruction combined with research and/or public service. (See "Instruction combined with research and/or public service (IRPS)")
}
 The intent of the Instructional Staff category is to include all individuals whose primary occupation includes instruction at the institution.  Excludes medical staff.}

\item{Medical staff}{Staff employed by or staff working in the medical school (Doctor of Medicine [M.D.] and/or Doctor of Osteopathic Medicine [D.O.]) component of a postsecondary institution or in a free standing medical school. Does not include staff employed by or employees working strictly in a hospital associated with a medical school or those who work in health or allied health schools or departments such as dentistry, veterinary medicine, nursing or dental hygiene unless the health or allied health schools or departments are affiliated with (housed in or under the authority of) the medical school.}

\item{Salary outlays for instructional staff equated to a 9-month contract}{Salary outlays of full-time, non-medical, instructional staff equated to month contract.

This is  calculated by adjusting the salary outlays at 10-, 11- 12-month contracts to a 9-month contract as follows:

(salary outlays at 9-months) + (.90 * salary outlays at 10 months) + (.818 *salary outlays at 11 months) + (.75 * salary outlays at 12 months)}
  }
}

