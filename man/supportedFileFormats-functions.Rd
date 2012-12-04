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

  \tabular{ll}{ txt &
  \code{\link[MALDIquantForeign]{importTxt}} \\ tab &
  \code{\link[MALDIquantForeign]{importTab}} \\ csv &
  \code{\link[MALDIquantForeign]{importCsv}} \\ fid &
  \code{\link[MALDIquantForeign]{importBrukerFlex}} \\
  mzXML & \code{\link[MALDIquantForeign]{importMzXml}} \\
  mzML & \code{\link[MALDIquantForeign]{importMzMl}} \\ } }

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
  Sebastian Gibb \email{mail@sebastiangibb.de}
}
\references{
  \url{http://strimmerlab.org/software/maldiquant/}
}
\seealso{
  \code{\link[MALDIquantForeign]{export}},
  \code{\link[MALDIquantForeign]{import}}
}

