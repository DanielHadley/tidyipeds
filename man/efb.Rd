\name{efb}
\alias{efb}

\title{Fall Enrollment: EFB}

\description{
This file contains the number of students enrolled in the fall, by age categories, gender, attendance (full- or part-time) status and level of student.
}

\section{Reporting}{
Institutions with traditional academic year calendar systems (semester, quarter, trimester or  4-1-4) report their enrollment as of October 15 or the official fall reporting date of the institution.

Institutions with calendar systems that differ by program or allow continuous enrollment report students that are enrolled at any time between August 1 and October 31.
}

\section{Data Layout}{
This file has multiple records per institution.  Records are uniquely defined by the variables IPEDS ID (UNITID), age categories (EFBAGE)  and level of student (LSTUDY).   Each record will contain the total enrollment,  enrollment for men and women, and the total enrollment and enrollment for men and women for full- and part-time students.
}

\section{Optional Data: Enrollment by Age}{
Submission of enrollment by age is mandatory in odd-numbered years and optional in even-numbered years. Optional data should not be used to produce national or state estimates as many institutions will not have data. Optional data are provided in even-numbered years only to allow users to compare institutional level data among reporting institutions.
}


\section{Glossary}{
\describe{

\item{Credit}{Recognition of attendance or performance in an instructional activity (course or program) that can be applied by a recipient toward the requirements for a degree, diploma, certificate, or other formal award.}

\item{First-professional student}{
  A student enrolled in any of the following degree programs :

  Chiropractic (D.C. or D.C.M.), Dentistry (D.D.S. or D.M.D.), Law (L.L.B., J.D.),
  Medicine (M.D.), Optometry (O.D.), Osteopathic Medicine (D.O.),
  Pharmacy (Pharm.D.), Podiatry (D.P.M., D.P., or Pod.D.),
  Theology (M.Div., M.H.L., B.D., or Ordination), Veterinary Medicine (D.V.M.)}

\item{Full Time Student}{
  \itemize{
    \item Undergraduate: A student enrolled for 12 or more semester credits, or 12 or more quarter credits, or 24 or more contact hours a week each term.
    \item Graduate: A student enrolled for 9 or more semester credits, or 9 or more quarter credits, or students involved in thesis or dissertation preparation that are considered full time by the institution.
    \item First-Professional: As defined by the institution.}
}


  \item{Graduate student}{A student who holds a bachelor's or first-professional degree, or equivalent, and is taking courses at the post-baccalaureate level. These students may or may not be enrolled in graduate programs .}


\item{Part Time Student}{
  \itemize{
    \item Undergraduate: A student enrolled for either 11 semester credits or less, or 11 quarter credits or less, or less than 24 contact hours a week each term.
    \item Graduate: A student enrolled for either 8 semester credits or less, or 8 quarter credits or less.
    \item First-Professional: As defined by the institution.}
}

\item{Undergraduate}{A student enrolled in a 4- or 5-year bachelor's degree program, an associate's degree program, or a vocational or technical program below the baccalaureate. }

  }
}
