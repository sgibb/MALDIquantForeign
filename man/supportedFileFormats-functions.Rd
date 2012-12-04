\name{supportedFileFormats}
\alias{supportedFileFormats}
\title{Supported file formats}
\usage{
  supportedFileFormats()
}
\value{
  a \code{list} with two named elements (\code{import} and
  \code{export}) containing a \code{character} vector of
  supported file types.
}
\description{
  This function prints all file formats supported by
  \code{\link{MALDIquantForeign}}.
}
\details{
  \subsection{Import}{

  \tabular{ll}{ txt \tab
  \code{\link[MALDIquantForeign]{importTxt}} \cr tab \tab
  \code{\link[MALDIquantForeign]{importTab}} \cr csv \tab
  \code{\link[MALDIquantForeign]{importCsv}} \cr fid \tab
  \code{\link[MALDIquantForeign]{importBrukerFlex}} \cr
  mzXML \tab \code{\link[MALDIquantForeign]{importMzXml}}
  \cr mzML \tab \code{\link[MALDIquantForeign]{importMzMl}}
  \cr } }

  \subsection{Export}{

  \tabular{ll}{ tab &
  \code{\link[MALDIquantForeign]{exportTab}} \\ csv &
  \code{\link[MALDIquantForeign]{exportCsv}} \\ msd &
  \code{\link[MALDIquantForeign]{exportMsd}} \\ } }
}
\examples{
library("MALDIquantForeign")

supportedFileFormats()
}
\author{
  Sebastian Gibb
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[MALDIquantForeign]{export}},
  \code{\link[MALDIquantForeign]{import}}
}

