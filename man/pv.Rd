% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radmac2.R
\name{pv}
\alias{pv}
\title{Calculate patient volume benchmarks for SurgeSim}
\usage{
pv(db = "medstat1_surgesim", sim, protocol)
}
\arguments{
\item{db}{Simulation database containing datamine}

\item{sim}{Simulation group database}

\item{protocol}{Simulation protocol for comparison}
}
\description{
Calculate patient volume benchmarks for SurgeSim
}
\examples{
z<-pv(protocol='Geyserville',sim='emdm2016a');
}

