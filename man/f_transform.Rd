% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/f_transform.R
\name{f_transform}
\alias{f_transform}
\title{f_transform}
\usage{
f_transform(x, trafos)
}
\arguments{
\item{x}{Nt x Nn matrix}

\item{trafos}{Vector or dataframe of transformation codes (of length Nn)}
}
\value{
Matrix of transformed series
}
\description{
Transforms the FRED-MD series to achieve stationarity. The codes are (see McCracken and Ng 2016, Appendix):
1 = no transformation
2 = first difference
3 = second difference
4 = log
5 = first difference of logs
6 = second difference of logs
7 = first difference of percentage change
}
\examples{
x_transformed <- f_transform(x, trafos)

}
