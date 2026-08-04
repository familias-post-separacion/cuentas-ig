
# SCRAPE MANUAL DE LA CUENTA --------------------------------------------------

# Paso a paso: 
# 1. Antes de correr este script es necesario ejecutar previamente "scrape_cuenta.R" y "cookies_instagram.R"
# Para no ejecutar manualmente cada línea de esos script, puedes correr lo siguiente en la terminal:
#
# source("scrape_manual.R")
# source(cookies_intagram.R")
#
# 2. Descomenta cada cuenta con los datos para ir scrapeando
# 3. Cada vez que descomentes una cuenta nueva debes eliminar la anterior
# 4. Guarda los cambios
# 5. Luego, para no ejecutar cada línea del código, corre lo siguiente en la terminal:
#
# source("scrape_cuenta_manual.R")
# Debes correr ese código cada vez que se estanque la búsqueda hasta terminar de revisar las publicaciones totales

# ---- 1. CONFIGURACIÓN EDITABLE DE ESTA CUENTA -------------------------------

# Cuenta: abogadavaleriaoportus 156/444

#URL_PERFIL       <- "https://www.instagram.com/abogadavaleriaoportus?igsh=MTdpZXV6dzI0MXJrNA=="
#NOMBRE_CUENTA    <- "abogadavaleriaoportus"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "abogadavaleriaoportus")

# archivos que deberían aparecer en files luego de ejectuar el código completo
#ARCHIVO_SALIDA   <- glue::glue("instagram_abogadavaleriaoportus.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_abogadavaleriaoportus.rds")


# Cuenta: nomashijosrehenes.cl

URL_PERFIL       <- "https://www.instagram.com/nomashijosrehenes.cl/"
NOMBRE_CUENTA    <- "nomashijosrehenes.cl"
NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "nomashijosrehenes.cl")
ARCHIVO_SALIDA   <- glue::glue("instagram_nomashijosrehenes.cl.csv")
ARCHIVO_PROGRESO <- glue::glue("progreso_nomashijosrehenes.cl.rds")

# Cuenta: vanessaferrerradovic

#URL_PERFIL       <- "https://www.instagram.com/vanessaferrerradovic/"
#NOMBRE_CUENTA    <- "vanessaferrerradovic"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "vanessaferrerradovic")
#ARCHIVO_SALIDA   <- glue::glue("instagram_vanessaferrerradovic.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_vanessaferrerradovic.rds")

# Cuenta: chile_in_justo_

#URL_PERFIL       <- "https://www.instagram.com/chile_in_justo_/"
#NOMBRE_CUENTA    <- "chile_in_justo_"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "chile_in_justo_")
#ARCHIVO_SALIDA   <- glue::glue("instagram_chile_in_justo_.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_chile_in_justo_.rds")

# Cuenta: hombresmaltratadoschile

#URL_PERFIL       <- "https://www.instagram.com/hombresmaltratadoschile/"
#NOMBRE_CUENTA    <- "hombresmaltratadoschile"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "hombresmaltratadoschile")
#ARCHIVO_SALIDA   <- glue::glue("instagram_hombresmaltratadoschile.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_hombresmaltratadoschile.rds")

# Cuenta: abogadachilena

#URL_PERFIL       <- "https://www.instagram.com/abogadachilena?igsh=Ymg3NzUzNXp4OTQ4"
#NOMBRE_CUENTA    <- "abogadachilena"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "abogadachilena")
#ARCHIVO_SALIDA   <- glue::glue("instagram_abogadachilena.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_abogadachilena.rds")

# Cuenta: ps_perito_caroval

#URL_PERFIL       <- "https://www.instagram.com/ps_perito_caroval/"
#NOMBRE_CUENTA    <- "ps_perito_caroval"
#NOMBRE_CUENTA_ARCHIVO <- gsub("[/\\\\]+", "", "ps_perito_caroval")
#ARCHIVO_SALIDA   <- glue::glue("instagram_ps_perito_caroval.csv")
#ARCHIVO_PROGRESO <- glue::glue("progreso_ps_perito_caroval.rds")

config <- config_cuenta_default

ua <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
)


# ---- 2. CARGAR PROGRESO PREVIO, SI EXISTE -----------------------------------

`%||%` <- function(a, b) if (!is.null(a)) a else b

ya_completado <- FALSE

if (file.exists(ARCHIVO_PROGRESO)) {
  progreso <- readRDS(ARCHIVO_PROGRESO)
  links_vistos            <- progreso$links_vistos
  resultados              <- progreso$resultados
  posts_antiguos_seguidos <- progreso$posts_antiguos_seguidos %||% 0
  ya_completado           <- isTRUE(progreso$completado)
  
  cat(glue::glue(
    "[{NOMBRE_CUENTA}] Retomando progreso guardado: {length(links_vistos)} ",
    "publicaciones ya vistas, {length(resultados)} guardadas en rango.\n"
  ))
  
  if (ya_completado) {
    cat(glue::glue(
      "[{NOMBRE_CUENTA}] Este progreso ya estaba marcado como COMPLETADO ",
      "(se llegó al corte por fecha o al tope de publicaciones). Si quieres ",
      "forzar una corrida nueva desde cero, borra el archivo ",
      "'{ARCHIVO_PROGRESO}' primero.\n"
    ))
  }
} else {
  links_vistos            <- character(0)
  resultados              <- list()
  posts_antiguos_seguidos <- 0
  cat(glue::glue("[{NOMBRE_CUENTA}] No hay progreso previo - empezando de cero.\n"))
}

guardar_progreso <- function(completado = FALSE) {
  saveRDS(
    list(
      links_vistos            = links_vistos,
      resultados              = resultados,
      posts_antiguos_seguidos = posts_antiguos_seguidos,
      completado              = completado
    ),
    ARCHIVO_PROGRESO
  )
  if (length(resultados) > 0) {
    readr::write_csv(dplyr::bind_rows(resultados), ARCHIVO_SALIDA)
  }
}

# Si ya estaba completo, no tiene sentido volver a scrollear - devolvemos
# lo que ya había y cortamos acá.
if (ya_completado) {
  resultado_final <- if (length(resultados) == 0) tibble_vacio() else dplyr::bind_rows(resultados)
  resultado_final
} else {
  

# ---- 3. ABRIR LAS DOS PESTAÑAS (cuadrícula + detalle) -----------------------

  # Pestaña 1: se queda SIEMPRE en la página de perfil, haciendo scroll.
  b <- ChromoteSession$new()
  aplicar_stealth(b)
  b$Network$setUserAgentOverride(userAgent = ua)
  aplicar_cookies(b, cookies_instagram)
  b$Page$navigate(URL_PERFIL)
  tryCatch(b$Page$loadEventFired(wait_ = TRUE), error = function(e) NULL)
  Sys.sleep(runif(1, config$espera_carga_inicial[1], config$espera_carga_inicial[2]))
  
  # --- Espera activa a que la cuadrícula realmente muestre publicaciones ---
  intentos_espera_inicial <- 0
  while (length(obtener_links_pagina(b)) == 0 && intentos_espera_inicial < 4) {
    intentos_espera_inicial <- intentos_espera_inicial + 1
    message(glue::glue(
      "[{NOMBRE_CUENTA}] Todavía no se ven publicaciones cargadas, ",
      "esperando un poco más ({intentos_espera_inicial}/4)..."
    ))
    Sys.sleep(3)
  }
  cat(glue::glue("[{NOMBRE_CUENTA}] Links iniciales visibles: {length(obtener_links_pagina(b))}\n"))
  
  # Pestaña 2 (detalle): YA NO se crea acá de entrada. Un diagnóstico aparte
  # (una sola pestaña, sin pestaña de detalle) mostró que el scroll en sí
  # funciona perfecto - scrollY se movió y aparecieron links nuevos al
  # primer intento. Corriendo el proceso real justo después, con la pestaña
  # de detalle creada de entrada (aunque en esta reanudación no llegara a
  # usarse, porque todos los links ya estaban vistos), el scroll volvió a
  # trabarse. La sospecha es que tener esa segunda pestaña autenticada
  # abierta y conectada - aunque esté en about:blank y sin usarse - ya es
  # suficiente para que Instagram trate la sesión como más sospechosa. Por
  # eso ahora se crea recién más abajo, la primera vez que hay un link nuevo
  # que visitar.
  b_detalle <- NULL
  

# ---- 4. LOOP PRINCIPAL: scroll + extracción + corte por fecha ---------------
 
  detener <- FALSE
  
  for (num_scroll in seq_len(config$max_scrolls + 1)) {
    links_pagina <- obtener_links_pagina(b)
    links_nuevos <- setdiff(links_pagina, links_vistos)
    message(glue::glue(
      "[{NOMBRE_CUENTA}] Vuelta #{num_scroll}: {length(links_pagina)} links ",
      "totales en la página, {length(links_nuevos)} nuevos ",
      "(de {length(links_vistos)} ya vistos en total)."
    ))
    
    for (link in links_nuevos) {
      links_vistos <- c(links_vistos, link)
      
      if (is.null(b_detalle)) {
        b_detalle <- ChromoteSession$new()
        aplicar_stealth(b_detalle)
        b_detalle$Network$setUserAgentOverride(userAgent = ua)
        aplicar_cookies(b_detalle, cookies_instagram)
        message(glue::glue(
          "[{NOMBRE_CUENTA}] Se abre recién ahora la pestaña de detalle ",
          "(primera publicación nueva a visitar en esta corrida)."
        ))
      }
      
      datos <- tryCatch(
        extraer_datos_publicacion(b_detalle, link),
        error = function(e) {
          message(glue::glue("[{NOMBRE_CUENTA}] Error en {link}: {e$message}"))
          NULL
        }
      )
      Sys.sleep(runif(1, config$espera_post[1], config$espera_post[2]))
      
      if (is.null(datos) || is.na(datos$fecha)) {
        message(glue::glue("[{NOMBRE_CUENTA}] Sin fecha legible en {link}, se omite."))
        next
      }
      
      fecha_post <- as.Date(datos$fecha)
      
      if (fecha_post > config$fecha_limite_superior) {
        message(glue::glue(
          "[{NOMBRE_CUENTA}] Publicación del {fecha_post} es más reciente ",
          "que {config$fecha_limite_superior}, se descarta (se sigue ",
          "avanzando por el feed)."
        ))
        next
      }
      
      if (fecha_post < config$fecha_limite_inferior) {
        posts_antiguos_seguidos <- posts_antiguos_seguidos + 1
        message(glue::glue(
          "[{NOMBRE_CUENTA}] Publicación del {fecha_post} es anterior a ",
          "{config$fecha_limite_inferior} ({posts_antiguos_seguidos}/",
          "{config$confirmacion_fin_rango} para confirmar el corte)."
        ))
        if (posts_antiguos_seguidos >= config$confirmacion_fin_rango) {
          detener <- TRUE
          break
        }
        next
      }
      
      posts_antiguos_seguidos <- 0
      resultados[[length(resultados) + 1]] <- datos
      message(glue::glue(
        "[{NOMBRE_CUENTA}] +1 publicación en rango ({fecha_post}) - ",
        "van {length(resultados)} guardadas."
      ))
      guardar_progreso()  # checkpoint real: CSV + .rds con links_vistos
      
      if (length(resultados) >= config$max_publicaciones) {
        message(glue::glue("[{NOMBRE_CUENTA}] Se alcanzó max_publicaciones ({config$max_publicaciones})."))
        detener <- TRUE
        break
      }
    }
    
    # Guardamos progreso también al final de cada vuelta de scroll (no solo
    # cuando hay una publicación nueva en rango) - así, si el proceso se
    # corta durante el scroll mismo, no se pierde el avance de qué ya se
    # revisó.
    guardar_progreso()
    
    if (detener) break
    
    if (num_scroll > config$max_scrolls) {
      message(glue::glue(
        "[{NOMBRE_CUENTA}] Se alcanzó max_scrolls ({config$max_scrolls}) sin ",
        "confirmar el corte por fecha - el progreso queda guardado, puedes ",
        "volver a correr este script (subiendo max_scrolls si hace falta) ",
        "para seguir desde acá."
      ))
      break
    }
    
    # Mandamos la pestaña de detalle a una página neutra antes de scrollear,
    # para soltar cualquier conexión viva de la última publicación visitada.
    # (Si nunca se llegó a crear en esta vuelta - todos los links ya vistos -
    # no hay nada que hacer acá.)
    if (!is.null(b_detalle)) {
      tryCatch(b_detalle$Page$navigate("about:blank"), error = function(e) NULL)
    }
    
    # --- Scroll con reintentos ---
    # Más intentos (10) y espera CRECIENTE entre uno y otro (hasta +10 seg
    # extra) - útil sobre todo justo después de reanudar una cuenta, cuando
    # la cuadrícula recién se cargó y a veces necesita más tiempo para
    # disparar la carga de la siguiente tanda. Esto no soluciona un freno
    # real de Instagram (si la sesión está limitada, se van a agotar los 10
    # intentos igual), pero evita rendirse demasiado rápido cuando solo era
    # lentitud.
    intentos_scroll_max <- 10
    links_nuevos_tras_scroll <- character(0)
    for (intento_scroll in seq_len(intentos_scroll_max)) {
      hacer_scroll_wheel(b)
      espera_extra <- min(intento_scroll - 1, 5) * 2  # +0, +2, +4, ... hasta +10 seg
      Sys.sleep(runif(1, config$espera_scroll[1], config$espera_scroll[2]) + espera_extra)
      links_nuevos_tras_scroll <- setdiff(obtener_links_pagina(b), links_vistos)
      if (length(links_nuevos_tras_scroll) > 0) break
      message(glue::glue(
        "[{NOMBRE_CUENTA}] El scroll #{intento_scroll} no trajo publicaciones ",
        "nuevas todavía, reintentando ({intento_scroll}/{intentos_scroll_max})..."
      ))
    }
    
    if (length(links_nuevos_tras_scroll) == 0) {
      message(glue::glue(
        "[{NOMBRE_CUENTA}] No se cargaron más publicaciones nuevas después de ",
        "varios intentos de scroll - el progreso queda guardado. Puede ser el ",
        "fin real del perfil, o que Instagram frenó la sesión por ahora (en ",
        "ese caso, espera un rato y vuelve a correr este mismo script - va a ",
        "retomar desde acá sin repetir lo ya visto)."
      ))
      break
    }
  }
  

# ----- 5. CIERRE Y RESULTADO FINAL -------------------------------------------
  
  b$close()
  if (!is.null(b_detalle)) b_detalle$close()
  
  # Se marca "completado" solo si de verdad se llegó al corte por fecha o al
  # tope de publicaciones (detener == TRUE) - si se cortó por max_scrolls o
  # por falta de contenido nuevo, queda como pendiente de reanudar.
  guardar_progreso(completado = isTRUE(detener))
  
  resultado_final <- if (length(resultados) == 0) tibble_vacio() else dplyr::bind_rows(resultados)
  message(glue::glue(
    "[{NOMBRE_CUENTA}] {if (isTRUE(detener)) 'Completado.' else 'Pausado (se puede reanudar).'} ",
    "{nrow(resultado_final)} publicaciones guardadas en '{ARCHIVO_SALIDA}'."
  ))
  resultado_final
  
}