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

\section{Glossary}{
\describe{

\item{Credit}{Recognition of attendance or performance in an instructional activity (course or program) that can be applied by a recipient toward the requirements for a degree, diploma, certificate, or other formal award.}

\item{Degree/certificate-seeking students}{Students enrolled in courses for credit and recognized by the institution as seeking a degree, certificate, or other formal award. High school students also enrolled in postsecondary courses for credit are not considered degree/certificate-seeking. }

\item{Distance education}{Education that uses one or more technologies to deliver instruction to students who are separated from the instructor and to support regular and substantive interaction between the students and the instructor synchronously or asynchronously.

Technologies used for instruction may include the following: Internet; one-way and two-way transmissions through open broadcasts, closed circuit, cable, microwave, broadband lines, fiber optics, satellite or wireless communication devices; audio conferencing; and video cassette, DVDs, and CD-ROMs, if the cassette, DVDs, and CD-ROMs are used in a course in conjunction with the technologies listed above.}

\item{Distance education course}{A course in which the instructional content is delivered exclusively via distance education.  Requirements for coming to campus for orientation, testing, or academic support services do not exclude a course from being classified as distance education.  }

\item{Graduate student}{A student who holds a bachelor's degree or above and is taking courses at the postbaccalaureate level. These students may or may not be enrolled in graduate programs.}

\item{Level of student [EFDELEV]}{Level and degree-seeking/non degree-seeking status of student  - This variable identifies the level of enrollment data for the institution.  Enrollment counts are available by level of student (undergraduate or graduate).  Undergraduate enrollments are disaggregated by degree-seeking and non-degree seeking status.}

\item{Undergraduate}{A student enrolled in a 4- or 5-year bachelor's degree program, an associate's degree program, or a vocational or technical program below the baccalaureate. }
  }
}
