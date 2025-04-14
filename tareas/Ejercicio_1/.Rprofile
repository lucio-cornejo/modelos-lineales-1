source("renv/activate.R")

# Definir el directorio de Pandoc
pandoc_path <-  "C:\\Program Files\\pandoc-3.6.4"
if (dir.exists(pandoc_path)) {
  Sys.setenv(RSTUDIO_PANDOC = pandoc_path)
} else {
  message("No se encontró el directorio especificado de Pandoc")
}
