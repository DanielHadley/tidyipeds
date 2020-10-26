\name{cdep}
\alias{cdep}

\title{Completions: CDEP}

\description{
This file contains the number of programs offered, by type of program, level of award (certificate or degree) and distance education status.
}

\section{Reporting}{
Program offerings were derived from completions data that were granted between July 1 of previous year and June 30 of current year.
}

\section{Type of Instructional Program}{
Type of program is categorized according to the Classification of Instructional Programs (CIP), a detailed coding system for postsecondary instructional programs.
}

\section{Data Layout}{
This file contains multiple records per institution. Each record is uniquely defined by the variables IPEDS ID (UNITID) and classification of instructional program (CIPCODE).  Each record will contain the total number of programs offered and the total number of programs offered via distance education.

Six digit CIPCODE (XX.NNNN) records will identify which award levels are offered and which award levels are offered via distance learning (1=Yes, 0=No).

Two digit CIPCODE (XX) summary records will contain the number of 6-digit CIPCODES under the  2-digit group that are offered by award level and distance education status and CIPCODE (99)  will contain totals for the institution.
}

\section{Data Releases}{
Preliminary release data have been edited but are subject to further NCES quality control procedures. Imputed data for nonresponding institutions are not included. These data are used for First Look (Preliminary Data) publications.

Provisional release data have undergone all NCES data quality control procedures. Data are imputed for nonresponding institutions. These data are used for First Look (Provisional Data) publications.

Final release data include revisions to the provisional release data that have been made by institutions during the subsequent data collection year. The final release data can be used when the most up to date data are required; however, these data may not match tables from the First Look reports based on preliminary and provisional data.
}

\section{Glossary}{
  \describe{
  \item{Classification of instrucional Program (CIP) code}{A six-digit code in the form xx.xxxx that identifies instructional  program specialties within educational institutions.}
  \item{Distance education program}{A program for which all the required coursework for program completion is able to be completed via distance education courses}
  }
}
