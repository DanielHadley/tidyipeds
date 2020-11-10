\name{sal_b}
\alias{sal_b}

\title{Instructional Staff/Salaries: SAL_B}

\description{
This file contains the number of full-time instructional staff receiving fringe benefits and the total and average expenditures  for the full-time instructional staff, by length of contract and type of benefit plan.
}

\section{Data Layout}{
This data file has multiple records for each institution. Each record is uniquely defined by the variables IPEDS ID (UNITID), and the type of benefit plan (BENTYPE).
}


\section{Glossary}{
\describe{

\item{11/12-month contract [SAB11]}{The contracted teaching period of faculty employed for the entire year, usually for a period of 11 or 12 months.}

\item{9/10-month contract [SAB09]}{The contracted teaching period of faculty employed for 2 semesters, 3 quarters, 2 trimesters, 2 4-month sessions, or the equivalent.}

\item{Equated 9-month contract [SABEQ9]}{For each type of fringe benefit the number covered of equated 9-month contract is equal to the sum of the number covered of both 9/10-month and 11/12-month contracts.

For the following 5 fringe benefits plans:
\itemize{
  \item 1) Retirement plans (other than Social Security) - Vested within 5-years.
  \item 2)  Retirement plans (other than Social Security) - Vested after 5-year.
  \item 3)  Group life insurance.
  \item 4)  Social Security taxes.
  \item 5)  Worker's compensation taxes.
}

11/12-month expenditures are equated to 9-month data expenditure data by multiplying the 11/12 month by .8182 and then added to the 9/10 month expenditures. These benefits are based on the percentage of a faculty member's salary.

For all other fringe benefit plans:
\itemize{
  \item 1)  Medical/dental plans.
  \item 2)  Other insurance benefits (cafeteria plan, etc.).
  \item 3)  Guaranteed disability income protection.
  \item 4)  Tuition plan-restricted and unrestricted (dependents only).
  \item 5)  Housing plan-restricted and unrestricted.
  \item 6)  Unemployment compensation taxes.
  \item 7)  Other benefits in kind with cash options.
}

Expenditures of equated 9-month contract are equal to the sum of expenditures of both 9/10 month and 11/12 month contracts. No adjustment is needed.

Average expenditures of equated 9-month contract are then calclulated as the expenditure of equated 9-month contract divided by the number covered of equated 9-month contract.}

\item{Fringe benefits [BENTYPE]}{Cash contributions in the form of supplementary or deferred compensation other than salary. Excludes the employee's contribution. Employee fringe benefits include retirement plans, social security taxes, medical/dental plans, guaranteed disability income protection plans, tuition plans, housing plans, unemployment compensation plans, group life insurance plans, worker's compensation plans, and other benefits in-kind with cash options.}
  }
}

