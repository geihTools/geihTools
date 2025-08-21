#' Fusionar y consolidar módulos mensuales de la GEIH
#'
#' Lee todos los módulos de un mes de la GEIH almacenados como CSV
#' y los combina en un único `data.table`.
#'
#' @param month Nombre del mes en español (ejemplo: `"enero"`, `"febrero"`).
#' @param year Año de los datos (por defecto `2024`).
#' @param data_dir Ruta a la carpeta base con los datos. Si es `NULL`,
#'   se usa la configuración del usuario o el bundle embebido.
#' @param verbose Si es `TRUE`, muestra mensajes de progreso.
#'
#' @return `data.table` consolidado con la información del mes.
#' @export
#' @import data.table
merge_month <- function(month, year = 2024, data_dir = NULL, verbose = TRUE) {

  # Variables clave para unir módulos
  key_vars <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR")

  # Normalizar mes
  month <- normalize_month_es(month)

  # Resolver carpeta base (puede ser: zip embebido -> tempdir, o carpeta usuario)
  base_root <- resolve_data_dir(data_dir)

  if (identical(base_root, geih_data_base())) {
    # Caso: bundle embebido ya descomprimido
    base_dir <- file.path(base_root, month)
  } else {
    # Caso: carpeta usuario → probar varias estructuras
    base_dir <- file.path(base_root, as.character(year), month)
    if (!dir.exists(base_dir)) base_dir <- file.path(base_root, month)
    if (!dir.exists(base_dir)) base_dir <- base_root
  }

  if (!dir.exists(base_dir)) {
    stop(sprintf("No existe carpeta: %s", base_dir))
  }

  # Buscar CSV
  files <- list.files(base_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) {
    stop(sprintf("Sin CSV en: %s", base_dir))
  }

  if (verbose) message(sprintf("Leyendo %d archivo(s) desde %s", length(files), base_dir))

  # Leer primer archivo
  final_dt <- data.table::fread(files[1], showProgress = verbose)

  # Iterar sobre los demás y unir
  if (length(files) > 1L) {
    for (f in files[-1]) {
      if (verbose) message("Uniendo: ", basename(f))
      df <- data.table::fread(f, showProgress = FALSE)

      new_key <- intersect(names(df), key_vars)
      if (!length(new_key)) stop("Sin claves comunes; revisa estructura de CSV.")

      final_dt <- merge(final_dt, df, by = new_key, all.x = TRUE, suffixes = c("", ".y"))

      # Resolver duplicados
      cols_x <- grep("\\.x$", names(final_dt), value = TRUE)
      if (length(cols_x)) data.table::setnames(final_dt, old = cols_x, new = sub("\\.x$", "", cols_x))

      cols_y <- grep("\\.y$", names(final_dt), value = TRUE)
      if (length(cols_y)) final_dt[, (cols_y) := NULL]
    }
  }

  return(final_dt)
}
