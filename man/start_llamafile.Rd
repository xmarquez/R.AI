% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/llamafile.R
\name{start_llamafile}
\alias{start_llamafile}
\title{Start Llamafile Instance}
\usage{
start_llamafile(llamafile_path, threads = 10)
}
\arguments{
\item{llamafile_path}{The path to the Llamafile executable.}

\item{threads}{The number of threads to allocate for Llamafile. Default is
10. Doesn't work on Windows}
}
\description{
This function attempts to start the Llamafile executable if it is not already
running.
}
\seealso{
Other llamafile: 
\code{\link{is_llamafile_running}()},
\code{\link{kill_llamafile}()},
\code{\link{which_llamafile_running}()}
}
\concept{llamafile}
