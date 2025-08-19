#' Fusionar todos los meses de un año de la GEIH
#' @param year Año (default 2024)
#' @param data_dir Carpeta base (opcional)
#' @param verbose Mensajes de progreso
#' @return data.table consolidado del año
#' @export
join_all_months <- function(year = 2024, data_dir = NULL, verbose = TRUE) {
  base_root <- resolve_data_dir(data_dir)
  base_dir <- if (identical(base_root, geih_data_base())) base_root else file.path(base_root, as.character(year))
  if (!dir.exists(base_dir)) stop(sprintf("No existe carpeta: %s", base_dir))

  months <- list.dirs(path = base_dir, full.names = FALSE, recursive = FALSE)
  if (!length(months)) stop("No se encontraron meses.")

  all_months <- data.table::rbindlist(
    lapply(months, function(m) {
      if (verbose) message("Procesando mes: ", m)
      merge_month(m, year = year, data_dir = base_root, verbose = verbose)
    }),
    fill = TRUE
  )
  all_months
}

#' Consolidar GEIH completa de un año
#' @inheritParams join_all_months
#' @export
join_all_geih <- function(year = 2024, data_dir = NULL, verbose = TRUE) {
  join_all_months(year, data_dir, verbose)
}
