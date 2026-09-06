## 05_analizar_emocion_odio_ironia.R
##
## Versión R (vía reticulate) de 05_analizar_emocion_odio_ironia.py.
## Llama exactamente a los mismos analizadores de pysentimiento (Python)
## desde R, sin reescribir el modelo -reticulate solo traduce la llamada.
##
## Requisitos (una sola vez):
##   install.packages("reticulate")
##   reticulate::conda_create("pysentimiento_env", python_version = "3.10")
##   reticulate::conda_install("pysentimiento_env",
##                              packages = c("pysentimiento", "torch", "pandas", "tqdm"),
##                              pip = TRUE)
##
## Input : dataset_unido.csv (generado por 01_preparar_datos.py)
## Output: analisis_emocion_odio_ironia.csv

library(reticulate)
library(dplyr)
library(readr)
library(purrr)
library(tibble)

use_condaenv("pysentimiento_env", required = TRUE)
ps <- import("pysentimiento")

# ------------------------------------------------------------------
# Configuración
# ------------------------------------------------------------------
INPUT_PATH        <- Sys.getenv("INPUT_PATH", "dataset_unido.csv")
OUTPUT_PATH       <- Sys.getenv("OUTPUT_PATH", "analisis_emocion_odio_ironia.csv")
TASKS             <- c("emotion", "hate_speech", "irony")
LANG              <- "es"
BATCH_SIZE        <- as.integer(Sys.getenv("BATCH_SIZE", 32))
CHECKPOINT_EVERY  <- as.integer(Sys.getenv("CHECKPOINT_EVERY", 200))

sanitize <- function(nombre) {
  nombre <- trimws(tolower(nombre))
  gsub(" ", "_", nombre, fixed = TRUE)
}

clasificar_lote <- function(analyzers, textos) {
  # Une, en un solo dataframe (una fila por texto), las probabilidades y la
  # etiqueta derivada de las 3 tareas. Mismo criterio que la versión Python:
  # single-label -> "<tarea>_etiqueta" (un valor); multi-label ->
  # "<tarea>_etiquetas" (lista separada por coma, o "ninguna").
  filas <- vector("list", length(textos))
  for (i in seq_along(filas)) filas[[i]] <- list()

  for (task in names(analyzers)) {
    analyzer <- analyzers[[task]]
    # as.list() es clave: un vector de largo 1 se convertiría en un str de
    # Python (no una lista), y predict() devolvería un solo objeto en vez
    # de una lista -rompiendo la indexación de abajo en el último lote
    # parcial. as.list() fuerza siempre una lista de Python.
    salidas <- analyzer$predict(as.list(textos))

    for (i in seq_along(textos)) {
      salida <- salidas[[i]]
      # reticulate ya convierte automáticamente $probas (dict -> lista con
      # nombres), $is_multilabel (bool -> logical) y $output (str o list ->
      # character) al acceder con "$", así que no hace falta llamar
      # py_to_r() explícitamente sobre ellos.
      probas <- salida$probas
      for (label in names(probas)) {
        col <- paste0("prob_", task, "_", sanitize(label))
        filas[[i]][[col]] <- probas[[label]]
      }
      es_multilabel <- salida$is_multilabel
      if (es_multilabel) {
        activas <- salida$output
        col <- paste0(task, "_etiquetas")
        filas[[i]][[col]] <- if (length(activas) == 0) "ninguna" else paste(activas, collapse = ", ")
      } else {
        col <- paste0(task, "_etiqueta")
        filas[[i]][[col]] <- salida$output
      }
    }
  }
  filas
}

filas_a_tibble <- function(filas_resultado) {
  bind_rows(lapply(filas_resultado, as_tibble))
}

main <- function() {
  df <- read_csv(INPUT_PATH, show_col_types = FALSE)
  n_total <- nrow(df)
  cat(sprintf("Publicaciones a clasificar: %d\n", n_total))
  cat(sprintf("Tareas: %s (lang=%s)\n", paste(TASKS, collapse = ", "), LANG))

  textos <- as.character(df$descripcion)

  # --- Retomar desde checkpoint si existe ---
  checkpoint_path <- paste0(OUTPUT_PATH, ".checkpoint.csv")
  filas_resultado <- list()
  inicio <- 0

  if (file.exists(checkpoint_path)) {
    parcial_previo <- read_csv(checkpoint_path, show_col_types = FALSE)
    columnas_nuevas <- setdiff(names(parcial_previo), names(df))
    if (nrow(parcial_previo) <= n_total && length(columnas_nuevas) > 0) {
      inicio <- nrow(parcial_previo)
      filas_resultado <- pmap(parcial_previo[columnas_nuevas], function(...) list(...))
      cat(sprintf("Checkpoint encontrado: retomando desde la publicación %d/%d.\n", inicio, n_total))
    } else {
      cat("Aviso: el checkpoint no coincide con el dataset actual; se ignora y se parte de cero.\n")
    }
  }

  if (inicio >= n_total) {
    cat("El checkpoint ya tiene todas las publicaciones clasificadas; solo falta consolidar el resultado.\n")
  }

  t0 <- Sys.time()
  analyzers <- setNames(
    lapply(TASKS, function(task) ps$create_analyzer(task = task, lang = LANG, batch_size = BATCH_SIZE)),
    TASKS
  )
  cat(sprintf("Analizadores cargados en %.1fs\n", as.numeric(Sys.time() - t0, units = "secs")))

  batches_por_checkpoint <- max(1L, CHECKPOINT_EVERY %/% BATCH_SIZE)
  contador_batches <- 0
  # Nota: R es 1-indexado (a diferencia de Python), por eso el rango de
  # inicio y el largo del último lote parcial se calculan distinto.
  starts <- seq(inicio + 1, n_total, by = BATCH_SIZE)

  t0 <- Sys.time()
  pb <- txtProgressBar(min = 0, max = max(length(starts), 1), style = 3)
  for (k in seq_along(starts)) {
    start <- starts[k]
    fin <- min(start + BATCH_SIZE - 1, n_total)
    lote <- textos[start:fin]
    filas_resultado <- c(filas_resultado, clasificar_lote(analyzers, lote))
    contador_batches <- contador_batches + 1
    setTxtProgressBar(pb, k)

    if (contador_batches %% batches_por_checkpoint == 0) {
      parcial <- df[seq_len(length(filas_resultado)), ]
      parcial <- bind_cols(parcial, filas_a_tibble(filas_resultado))
      write_excel_csv(parcial, checkpoint_path)  # utf-8 con BOM, como utf-8-sig en pandas
    }
  }
  close(pb)
  cat(sprintf("\nClasificación completa en %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

  df <- bind_cols(df, filas_a_tibble(filas_resultado))
  write_excel_csv(df, OUTPUT_PATH)
  cat(sprintf("Guardado: %s\n", OUTPUT_PATH))

  if (file.exists(checkpoint_path)) file.remove(checkpoint_path)

  cat("\nDistribución de emotion_etiqueta:\n")
  print(table(df$emotion_etiqueta))

  cat("\nDistribución de irony_etiqueta:\n")
  print(table(df$irony_etiqueta))

  cat("\nFrecuencia de cada categoría de hate_speech (multi-etiqueta, no excluyentes):\n")
  for (label in c("hateful", "targeted", "aggressive")) {
    col <- paste0("prob_hate_speech_", label)
    if (col %in% names(df)) {
      n <- sum(df[[col]] > 0.5)
      cat(sprintf("  %s: %d (%.1f%%)\n", label, n, 100 * n / nrow(df)))
    }
  }
}

if (sys.nframe() == 0) {
  main()
}
