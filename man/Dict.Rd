% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dict.R
\name{Dict}
\alias{Dict}
\alias{dict}
\title{R6 Based Key-Value Dictionary Implementation}
\usage{
dict(..., .class = "any", .overwrite = TRUE)
}
\arguments{
\item{...}{Any length of key and value pairs. If you would like to use
a not valid R name as a key, you must wrap it by backquotes or convert it
using \code{\link{make.names}}.}

\item{.class}{A character scalar of value object's class. It must be an
output from \code{\link{class}}. If \code{"any"} (default), value can contain
any type of object.}

\item{.overwrite}{A logical scalar whether to overwrite the value if the
key is overlapped.}
}
\value{
A \code{Dict} class object.
}
\description{
A key-value dictionary data structure based on R6 class which is designed to
be similar usages with other languages dictionary (e.g. Python) with
reference semantics and extendabilities by R6.
}
\examples{

## ------------------------------------------------
## Method `Dict$new`
## ------------------------------------------------

ages <- Dict$new(
  Charlie = 40L,
  Alice = 30L,
  Bob = 25L,
  .class = "integer",
  .overwrite = TRUE
)

## ------------------------------------------------
## Method `Dict$print`
## ------------------------------------------------

ages$print(n = Inf)

## ------------------------------------------------
## Method `Dict$add`
## ------------------------------------------------

ages$add(John = 18L)
ages["John"] <- 18L

## ------------------------------------------------
## Method `Dict$has`
## ------------------------------------------------

ages$has("Bob")

## ------------------------------------------------
## Method `Dict$get`
## ------------------------------------------------

ages$get("Bob")
ages["Bob"]
ages[3] # also by integer index

## ------------------------------------------------
## Method `Dict$remove`
## ------------------------------------------------

ages$remove("Bob")

## ------------------------------------------------
## Method `Dict$sort`
## ------------------------------------------------

ages$sort()

## ------------------------------------------------
## Method `Dict$clear`
## ------------------------------------------------

ages$clear()
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{items}}{A \code{tbl_df} of the dictionary items.}

\item{\code{keys}}{A character vector of the dictionary keys.}

\item{\code{values}}{A list of of the dictionary values.}

\item{\code{length}}{A integer scalar of the items length.}

\item{\code{class}}{A character scalar of value class.}

\item{\code{overwrite}}{A logical scalar whether to overwrite value if key is
overlapped.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Dict-new}{\code{Dict$new()}}
\item \href{#method-Dict-print}{\code{Dict$print()}}
\item \href{#method-Dict-add}{\code{Dict$add()}}
\item \href{#method-Dict-has}{\code{Dict$has()}}
\item \href{#method-Dict-get}{\code{Dict$get()}}
\item \href{#method-Dict-remove}{\code{Dict$remove()}}
\item \href{#method-Dict-sort}{\code{Dict$sort()}}
\item \href{#method-Dict-clear}{\code{Dict$clear()}}
\item \href{#method-Dict-clone}{\code{Dict$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-new"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-new}{}}}
\subsection{Method \code{new()}}{
Construct a new Dict object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$new(..., .class = "any", .overwrite = TRUE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Any length of key and value pairs. If you would like to use
a not valid R name as a key, you must wrap it by backquotes or convert it
using \code{\link{make.names}}.}

\item{\code{.class}}{A character scalar of value object's class. It must be an
output from \code{\link{class}}. If \code{"any"} (default), value can
contain any type of object.}

\item{\code{.overwrite}}{A logical scalar whether to overwrite the value if the
key is overlapped.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{Dict} class object.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages <- Dict$new(
  Charlie = 40L,
  Alice = 30L,
  Bob = 25L,
  .class = "integer",
  .overwrite = TRUE
)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-print"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-print}{}}}
\subsection{Method \code{print()}}{
Print Dict \code{items} which is a \code{\link[tibble]{tbl_df-class}}
object by tibble package.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Additional arguments passed to \code{print.tbl}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{Dict} object by \code{invisible(self)}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$print(n = Inf)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-add"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-add}{}}}
\subsection{Method \code{add()}}{
Add key-value objects to the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$add(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Any length of key and value pairs. If you would like to use
a not valid R name as a key, you must wrap it by backquotes or convert it
using \code{\link{make.names}}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{Dict} object by \code{invisible(self)}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$add(John = 18L)
ages["John"] <- 18L
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-has"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-has}{}}}
\subsection{Method \code{has()}}{
Check if the object contains the key.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$has(key = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{A character scalar of the dictionary key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A logical scalar.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$has("Bob")
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-get"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-get}{}}}
\subsection{Method \code{get()}}{
Retrieves object with a key from the dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$get(key = NULL, default = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{A character scalar, integer scalar of items index or NULL.
If key is NULL and items is not empty, the first value is returned.}

\item{\code{default}}{A default value returned, if the key is not found. Default
is \code{NULL}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A object with the key.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$get("Bob")
ages["Bob"]
ages[3] # also by integer index
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-remove"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-remove}{}}}
\subsection{Method \code{remove()}}{
Removes a key-value from the dictionary by a key. If the key is a not
valid key, this function throw an error. Use \code{self$has()} to check
key availability.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$remove(key = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{A character scalar of the dictionary key.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{Dict} object by \code{invisible(self)}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$remove("Bob")
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-sort"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-sort}{}}}
\subsection{Method \code{sort()}}{
Sort dictionary by keys.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$sort(desc = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{desc}}{A logical scalar whether to sort in descending order. Default
is \code{FALSE}.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{Dict} object by \code{invisible(self)}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$sort()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-clear"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-clear}{}}}
\subsection{Method \code{clear()}}{
Clear dictionary.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$clear()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\code{Dict} object by \code{invisible(self)}.
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{ages$clear()
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Dict-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Dict-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Dict$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
