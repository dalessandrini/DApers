% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limpieza_TS_TT.R
\name{importar_tt}
\alias{importar_tt}
\title{Importa formato TT y lo apronta para TS}
\usage{
importar_tt(archivo_csv, ...)
}
\arguments{
\item{archivo_csv}{ubicacion del archivo CSV YA CORREGIDO}

\item{...}{No usado}
}
\description{
Partiendo de archivo_csv (corregido) se hacen depuraciones adicionales para
conformar al formato TT
}
\details{
Fundamental haber corregido CSV ANTES: edicion MANUAL previa de archivo TT
(agregar comillas necesarias, controlar '1 fila 1 registro', guardar con separador TAB)

Es la parte 1 del proceso importar_tt() >> etiq_rp(2): crear RP20xx
}
