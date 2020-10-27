\name{efa_dist}
\alias{efa_dist}

\title{Fall Enrollment: EFA_DIST}

\description{
This file contains the number of students enrolled in the fall, by distance education status and level of student.
}

\section{Reporting}{
Institutions with traditional academic year calendar systems (semester, quarter, trimester or  4-1-4) report their enrollment as of October 15 or the official fall reporting date of the institution.

Institutions with calendar systems that differ by program or allow continuous enrollment report students that are enrolled at any time between August 1 and October 31.
}

\section{Data Layout}{
This file has multiple records per institution. Records are uniquely defined by the variables IPEDS ID (UNITID), and level of student (EFDELEV). Each record will contain the total students enrolled, students enrolled exclusively in distance education courses, students enrolled in some education distance education courses and students not enrolled in any distance education courses.

The number of students enrolled in exclusively distance education courses are also reported by the following location categories:
\itemize{
    \item in the same state/jurisdiction as the institution;
    \item in a different state/jurisdiction than the institution;
    \item in the U.S. but the state/jurisdiction is unknown;
    \item residing outside the U.S..
    }
}

\section{Data Releases}{
Preliminary release data have been edited but are subject to further NCES quality control procedures. Imputed data for nonresponding institutions are not included. These data are used for First Look (Preliminary Data) publications.

Provisional release data have undergone all NCES data quality control procedures. Data are imputed for nonresponding institutions. These data are used for First Look (Provisional Data) publications.

Final release data include revisions to the provisional release data that have been made by institutions during the subsequent data collection year. The final release data can be used when the most up to date data are required; however, these data may not match tables from the First Look reports based on preliminary and provisional data.
}

\section{Glossary}{
\describe{
  \item{Distance education course}{
  A course in which the instructional content is delivered exclusively via distance education. Requirements for coming to campus for orientation, testing, or academic support services do not exclude a course from being classified as distance education.
  }
  \item{Distance education}{
   Education that uses one or more technologies to deliver instruction to students who are separated from the instructor and to support regular and substantive interaction between the students and the instructor synchronously or asynchronously.

   Technologies used for instruction may include the following: Internet; one-way and two-way transmissions through open broadcasts, closed circuit, cable, microwave, broadband lines, fiber optics, satellite or wireless communication devices; audio conferencing; and video cassette, DVDs, and CD-ROMs, if the cassette, DVDs, and CD-ROMs are used in a course in conjunction with the technologies listed above.
  }
  }
}
