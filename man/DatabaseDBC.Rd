% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatabaseDBC.R
\name{DatabaseDBC}
\alias{DatabaseDBC}
\title{DatabaseDBC Class}
\description{
This class extends the \code{Database} class, to query a database using
\code{DatabaseConnector} with JDBC.
}
\section{Super classes}{
\code{\link[DarwinShinyModules:ShinyModule]{DarwinShinyModules::ShinyModule}} -> \code{\link[DarwinShinyModules:Database]{DarwinShinyModules::Database}} -> \code{DatabaseDBC}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DatabaseDBC-new}{\code{DatabaseDBC$new()}}
\item \href{#method-DatabaseDBC-query}{\code{DatabaseDBC$query()}}
\item \href{#method-DatabaseDBC-execute}{\code{DatabaseDBC$execute()}}
\item \href{#method-DatabaseDBC-clone}{\code{DatabaseDBC$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="DarwinShinyModules" data-topic="ShinyModule" data-id="UI"><a href='../../DarwinShinyModules/html/ShinyModule.html#method-ShinyModule-UI'><code>DarwinShinyModules::ShinyModule$UI()</code></a></span></li>
<li><span class="pkg-link" data-pkg="DarwinShinyModules" data-topic="ShinyModule" data-id="server"><a href='../../DarwinShinyModules/html/ShinyModule.html#method-ShinyModule-server'><code>DarwinShinyModules::ShinyModule$server()</code></a></span></li>
<li><span class="pkg-link" data-pkg="DarwinShinyModules" data-topic="ShinyModule" data-id="validate"><a href='../../DarwinShinyModules/html/ShinyModule.html#method-ShinyModule-validate'><code>DarwinShinyModules::ShinyModule$validate()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatabaseDBC-new"></a>}}
\if{latex}{\out{\hypertarget{method-DatabaseDBC-new}{}}}
\subsection{Method \code{new()}}{
Initializer method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatabaseDBC$new(connectionDetails)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{connectionDetails}}{(\code{ConnectionDetails}) Connection Details from \code{DatabaseConnector::createConnectionDetails}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{invisible(self)}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatabaseDBC-query"></a>}}
\if{latex}{\out{\hypertarget{method-DatabaseDBC-query}{}}}
\subsection{Method \code{query()}}{
Query method to query the database using \code{DatabaseConnector::renderTranslateQuerySql}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatabaseDBC$query(sql, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sql}}{(\code{character(1)}) SQL Query}

\item{\code{...}}{Additional parameters for \code{DatabaseConnector::renderTranslateQuerySql}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{data.frame}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatabaseDBC-execute"></a>}}
\if{latex}{\out{\hypertarget{method-DatabaseDBC-execute}{}}}
\subsection{Method \code{execute()}}{
Execute method to execute a query on the database using \code{DatabaseConnector::renderTranslateExecuteSql}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatabaseDBC$execute(sql, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sql}}{(\code{character(1)}) SQL Query to execute}

\item{\code{...}}{Additional parameters for \code{DatabaseConnector::renderTranslateExecuteSql}}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{invisible(self)}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DatabaseDBC-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DatabaseDBC-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DatabaseDBC$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
