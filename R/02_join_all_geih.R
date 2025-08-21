#' Fusionar todos los meses de un año de la GEIH
#'
#' Une todos los módulos de cada mes de un año en un solo `data.table`.
#'
#' @param year Año a consolidar (default 2024).
#' @param data_dir Ruta base con los datos (opcional). Si es `NULL`,
#'   se utiliza el bundle embebido comprimido dentro del paquete.
#' @param verbose Si TRUE, muestra mensajes de progreso.
#'
#' @return data.table consolidado con toda la GEIH del año.
#' @export
join_all_months <- function(year = 2024, data_dir = NULL, verbose = TRUE) {
  # 1. Resolver carpeta base
  if (is.null(data_dir)) {
    # Caso bundle embebido → descomprimir zip en tempdir()
    base_root <- get_geih_bundle()
  } else {
    # Caso carpeta usuario
    base_root <- resolve_data_dir(data_dir)
  }

  # Asegurar que la carpeta existe
  base_dir <- file.path(base_root, as.character(year))
  if (!dir.exists(base_dir)) base_dir <- base_root
  if (!dir.exists(base_dir)) {
    stop(sprintf("No existe carpeta: %s", base_dir))
  }

  # 2. Listar carpetas de meses
  months <- list.dirs(path = base_dir, full.names = FALSE, recursive = FALSE)
  if (!length(months)) stop("No se encontraron subcarpetas de meses en: ", base_dir)

  # 3. Consolidar todos los meses
  all_months <- data.table::rbindlist(
    lapply(months, function(m) {
      if (verbose) message("Procesando mes: ", m)
      merge_month(m, year = year, data_dir = base_dir, verbose = verbose)
    }),
    fill = TRUE
  )

  return(all_months)
}

#' Consolidar GEIH completa de un año
#' @inheritParams join_all_months
#' @export
join_all_geih <- function(year = 2024, data_dir = NULL, verbose = TRUE) {
  join_all_months(year, data_dir, verbose)
}
