library(data.table)

join_month <- local({
  merge_month <- function(month) {

    key_variables <- c("DIRECTORIO", "SECUENCIA_P", "ORDEN", "HOGAR")

    base_dir <- file.path(getwd(), "datos", month)

    all_files <- list.files(path = base_dir, pattern = "*.csv", full.names = TRUE, ignore.case = TRUE)

    final_df <- fread(file = all_files[1])

    for (file in all_files[-1]) {
      df <- fread(file = file)

      new_key_variables <- intersect(colnames(df), key_variables)

      final_df <- merge(final_df, df, by = new_key_variables, all.x = TRUE)

      cols_x <- grep("\\.x$", colnames(final_df), value = TRUE)
      setnames(final_df, old = cols_x, new = gsub("\\.x$", "", cols_x))

      cols_y <- grep("\\.y$", colnames(final_df), value = TRUE)
      final_df[, (cols_y) := NULL]

    }

    return(final_df)
  }
})

join_all_months <- local({
  geih_completed <- function () {

    base_dir <- file.path(getwd(), "datos")

    months <- list.dirs(path = base_dir, full.names = FALSE, recursive = FALSE)

    all_months <- data.table()

    for (month in months) {

      if (length(all_months) == 0) {
        all_months <- merge_month(month)
      } else {
        all_months <- rbindlist(list(all_months, merge_month(month)), fill = T)
      }
    }

    fwrite(all_months, file = "geih_complete.csv")
    return (all_months)

  }
})


#R/join_all_geih.R

#' Hace un empalme de los modulos y los meses de la GEIH
#' Los datos disponibles son del año 2024
#'
#' `join_all_geih()` Crea un archivo .csv llamado "geih_complete.csv".
#' @return Archivo con la GEIH empleta, para el año 2024
#' @example
#' geih_completa <- join_all_geih()
#' @export
join_all_geih <- local({
  function() {
    join_all_months()
  }
})



