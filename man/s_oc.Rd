\name{s_oc}
\alias{s_oc}

\title{Fall Staff: S_OC}

\description{
This file contains the number of staff on the payroll of the institution as of November 1,  by occupational categories, race/ethnicity and gender.
}

\section{Data Layout}{
This file has multiple records per institution.  Each record is uniquely defined by the variables IPEDS ID (UNITID), and occupational category and full- and part-time status (STAFFCAT).
}

\section{Amendments}{
Prior to the IPEDS Web-based data collection in 2001, human resource data reported by race/ethnicity and gender was only available in odd-numbered years.  Beginning with 2001, human resource data reported by race/ethnicity and gender was mandatory in odd-numbered years and optional in even-numbered years.  Beginning with 2016 reporting human resource data by race/ethnicity and gender is mandatory annually.
}


\section{Glossary}{
\describe{

\item{11/12-month salary contract/teaching period}{The contracted teaching period of faculty employed for the entire year, usually a period of 11 or 12 months. }

\item{9/10-month salary contract/teaching period}{The contracted teaching period of faculty employed for 2 semesters, 3 quarters, 2 trimesters,  4-month sessions, or the equivalent.
}

\item{Clerical and secretarial}{Persons whose assignments typically are associated with clerical activities or are specifically of a secretarial nature. Includes personnel who are responsible for internal and external communications, recording and retrieval of data (other than computer programmer) and/or information and other paperwork required in an office. Also includes such occupational titles such as switchboard operators, including answering service; telephone operators; bill and account collectors; billing and posting clerks and machine operators; bookkeeping, accounting, and auditing clerks; payroll and timekeeping clerks; procurement clerks; file clerks; clerical library assistants; human resources assistants, except payroll and timekeeping; shipping, receiving, and traffic clerks; secretaries and administrative assistants; computer operators; data entry and information processing workers; desktop publishers; mail clerks and mail machine operators (except postal service); office clerks (general); office machine operators (except computer); and proofreaders and copy markers.}

\item{Executive, administrative, and managerial }{Persons whose assignments require management of the institution, or a customarily recognized department or subdivision thereof. Assignments require the performance of work directly related to management policies or general business operations of the institution, department or subdivision. Assignments in this category customarily and regularly require the incumbent to exercise discretion and independent judgment. Included in this category are employees holding titles such as: top executives; chief executives; general and operations managers; advertising, marketing, promotions, public relations, and sales managers; operations specialties managers; administrative services managers; computer and information systems managers; financial managers; human resources managers; purchasing managers; postsecondary education administrators such as: presidents, vice presidents (including assistants and associates), deans (including assistants and associates) if their principal activity is administrative and not primarily instruction, research or public service, directors (including assistants and associates), department heads (including assistants and associates) if their principal activity is administrative and not primarily instruction, research or public service, assistant and associate managers (including first-line managers of service, production and sales workers who spend more than 80 percent of their time performing supervisory activities); engineering managers; food service managers; lodging managers; and medical and health services managers. }

\item{Faculty}{Persons identified by the institution as such and typically those whose initial assignments are made for the purpose of conducting instruction, research or public service as a principal activity (or activities). They may hold academic rank titles of professor, associate professor, assistant professor, instructor, lecturer or the equivalent of any of those academic ranks. Faculty may also include the chancellor/president, provost, vice provosts, deans, directors or the equivalent, as well as associate deans, assistant deans and executive officers of academic departments (chairpersons, heads or the equivalent) if their principal activity is instruction combined with research and/or public service. The designation as "faculty" is separate from the activities to which they may be currently assigned. For example, a newly appointed president of an institution may also be appointed as a faculty member. Graduate, instruction, and research assistants are not included in this category .}

\item{Full-time staff (employees) }{As defined by the institution. The type of appointment at the snapshot date determines whether an employee is full time or part time. The employee's term of contract is not considered in making the determination of full or part time.}

\item{Graduate assistants}{Students employed on a part-time basis for the primary purpose of assisting in classroom or laboratory instruction or in the conduct of research. Graduate students having titles such as graduate assistant, teaching assistant, teaching associate, teaching fellow, or research assistant typically hold these positions. Students in the College Work-Study Program are not included in this category. }

\item{Less-than-9-month salary contract/teaching period}{The contracted teaching period of faculty employed for less than 2 semesters, 3 quarters, 2 trimesters, or 2 4-month sessions. }

\item{Occupation and full- and part-time status [STAFFCAT]}{Occupation and full and part-time status. (See "Occupation category", "Full-time staff (employees)", and "Part-time staff (employees)")}

\item{Occupation category [OCCUPCAT]}{This variable identifies postsecondary staff by IPEDS Human Resource occupation categories.

The IPEDS HR survey component uses the Standard Occupational Classification (SOC) system to assist institutions in classifying employees for IPEDS reporting purposes. The SOC is a system used by Federal statistical agencies to classify workers and jobs into occupational categories for the purpose of collecting, calculating, analyzing, or disseminating data. In 2010 and 2018, updated versions of the SOC system (http://www.bls.gov/soc/) was released. Consequently, the IPEDS HR survey was revised to incorporate the 2010 and then 2018 SOC information. The new IPEDS occupational categories are effective with  2012-13 and then 2018-19 Human Resources reporting.}

\item{Other professional staff (support/service) }{Persons employed for the primary purpose of performing academic support, student service, and institutional support, whose assignments would require either a baccalaureate degree or higher or experience of such kind and amount as to provide a comparable background. Included in this category are all employees holding titles such as business operations specialists; buyers and purchasing agents; human resources, training, and labor relations specialists; management analysts; meeting and convention planners; miscellaneous business operations specialists; financial specialists; accountants and auditors; budget analysts; financial analysts and advisors; financial examiners; loan counselors and officers; computer specialists; computer and information scientists, research; computer programmers; computer software engineers; computer support specialists; computer systems analysts; database administrators; network and computer systems administrators; network systems and data communication analysts; counselors, social workers, and other community and social service specialists; counselors; social workers; health educators; clergy; directors, religious activities and education; lawyers; librarians, curators, and archivists; museum technicians and conservators; librarians; artists and related workers; designers; athletes, coaches, umpires; dancers and choreographers; music directors and composers; chiropractors; dentists; dietitians and nutritionists; optometrists; pharmacists; physicians and surgeons; podiatrists; registered nurses; therapists; and veterinarians. }

\item{Part-time staff (employees)}{As determined by the institution. The type of appointment at the snapshot date determines whether an employee is full time or part time. The employee's term of contract is not considered in making the determination of full or part time. Casual employees (hired on an ad-hoc basis or occasional basis to meet short-term needs) and students in the College Work-Study Program (CWS) are not considered part-time staff.}

\item{Primary occupational activity}{The principal activity of a staff member as determined by the institution. If an individual participates in two or more activities, the primary activity is normally determined by the amount of time spent in each activity.}

\item{Technical and paraprofessional (staff) }{Persons whose assignments require specialized knowledge or skills which may be acquired through experience, apprenticeship, on-the-job-training, or academic work in occupationally specific programs that result in a 2-year degree or other certificate or diploma. Includes persons who perform some of the duties of a professional in a supportive role, which usually requires less formal training and/or experience than normally required for professional status. Included in this category are mathematical technicians; life, physical, and social science technicians; agricultural and food science technicians; chemical technicians; geological and petroleum technicians; nuclear technicians; paralegals and legal assistants; miscellaneous legal support workers; health technologists and technicians; dietetic technicians; pharmacy technicians; licensed practical and licensed vocational nurses; medical records and health information technicians; opticians, dispensing; healthcare support occupations; nursing aides, orderlies, and attendants; physical therapist assistants and aides; massage therapists; dental assistants; medical assistants; and pharmacy aides. }
  }
}

