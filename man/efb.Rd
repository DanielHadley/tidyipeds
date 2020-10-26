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

\section{Data Releases}{
Preliminary release data have been edited but are subject to further NCES quality control procedures. Imputed data for nonresponding institutions are not included. These data are used for First Look (Preliminary Data) publications.

Provisional release data have undergone all NCES data quality control procedures. Data are imputed for nonresponding institutions. These data are used for First Look (Provisional Data) publications.

Final release data include revisions to the provisional release data that have been made by institutions during the subsequent data collection year. The final release data can be used when the most up to date data are required; however, these data may not match tables from the First Look reports based on preliminary and provisional data.
}

\section{Glossary}{
\describe{
  \item{First-professional student}{
  A student enrolled in any of the following degree programs :

  Chiropractic (D.C. or D.C.M.), Dentistry (D.D.S. or D.M.D.), Law (L.L.B., J.D.),
  Medicine (M.D.), Optometry (O.D.), Osteopathic Medicine (D.O.),
  Pharmacy (Pharm.D.), Podiatry (D.P.M., D.P., or Pod.D.),
  Theology (M.Div., M.H.L., B.D., or Ordination), Veterinary Medicine (D.V.M.)
  }
  \item{Full Time Student}{
  \itemize{
    \item Undergraduate: A student enrolled for 12 or more semester credits, or 12 or more quarter credits, or 24 or more contact hours a week each term.
    \item Graduate: A student enrolled for 9 or more semester credits, or 9 or more quarter credits, or students involved in thesis or dissertation preparation that are considered full time by the institution.
    \item First-Professional: As defined by the institution.
  }
  }
  \item{Part Time Student}{
  \itemize{
    \item Undergraduate: A student enrolled for either 11 semester credits or less, or 11 quarter credits or less, or less than 24 contact hours a week each term.
    \item Graduate: A student enrolled for either 8 semester credits or less, or 8 quarter credits or less.
    \item First-Professional: As defined by the institution.
  }
  }
  }
}
