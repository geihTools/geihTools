#' Ruta del bundle embebido (GEIH 2024 de ejemplo)
#'
#' Retorna la ruta absoluta al *bundle* de datos incluido dentro del paquete,
#' ubicado en `inst/extdata/geih_2024/`.
#'
#' Usado internamente por [merge_month()] y otras funciones cuando
#' no se especifica carpeta del usuario.
#'
#' @return Cadena con la ruta absoluta.
#' @keywords internal
#' @export
geih_data_base <- function() {
  system.file("extdata", "geih_2024", package = "geihTools")
}

#' Configurar carpeta de datos del usuario
#'
#' Permite establecer una carpeta propia donde se encuentren los datos GEIH.
#' Una vez configurada, esta ruta queda guardada como opción global del paquete
#' y será utilizada automáticamente por funciones como [merge_month()].
#'
#' @param path Ruta a la carpeta base (ejemplo: `"C:/mi_geih_data"`).
#' @export
set_geih_data_dir <- function(path) {
  stopifnot(dir.exists(path))
  options(geihTools.data_dir = normalizePath(path, winslash = "/", mustWork = TRUE))
  invisible(path)
}

#' Obtener carpeta de datos del usuario
#'
#' Devuelve la carpeta configurada previamente con [set_geih_data_dir()].
#'
#' @return Ruta absoluta configurada, o `NULL` si no existe.
#' @keywords internal
#' @export
get_geih_data_dir <- function() {
  opt <- getOption("geihTools.data_dir", NULL)
  if (!is.null(opt) && dir.exists(opt)) {
    return(opt)
  }
  NULL
}

#' Resolver carpeta de datos
#'
#' Determina de dónde leer los datos según la prioridad:
#' 1. Ruta explícita pasada como argumento,
#' 2. Carpeta configurada con [set_geih_data_dir()],
#' 3. Bundle embebido con GEIH 2024.
#'
#' @param data_dir Ruta explícita (opcional).
#' @return Ruta absoluta a la carpeta base seleccionada.
#' @keywords internal
#' @export
resolve_data_dir <- function(data_dir = NULL) {
  if (!is.null(data_dir)) {
    return(normalizePath(data_dir, winslash = "/", mustWork = TRUE))
  }
  user <- get_geih_data_dir()
  if (!is.null(user)) {
    return(user)
  }
  geih_data_base()
}

#' Normalizar nombre de mes en español
#'
#' Convierte el texto a minúsculas, elimina espacios, y valida que corresponda
#' a un mes válido en español.
#'
#' @param month Nombre del mes (ejemplo: `"Enero"`, `" febrero "`).
#' @return Cadena con el mes normalizado en minúsculas.
#' @keywords internal
#' @export
normalize_month_es <- function(month) {
  meses <- c("enero","febrero","marzo","abril","mayo","junio",
             "julio","agosto","septiembre","octubre","noviembre","diciembre")
  m <- tolower(trimws(as.character(month)))
  if (!m %in% meses) {
    stop(sprintf("Mes no válido: %s. Usa uno de: %s",
                 month, paste(meses, collapse = ", ")))
  }
  m
}
