
# SCRIPT CON FUNCIONES A UTILIZAR EN "scrape_cuenta_manual.R" -----------------

# ---- Librerías --------------------------------------------------------------

pacman::p_load(chromote, rvest, dplyr, stringr, purrr, tibble, glue, jsonlite)


# ---- PARÁMETROS EDITABLES POR CUENTA ----------------------------------------

config_cuenta_default <- list(
  fecha_limite_inferior = as.Date("2022-01-01"),
  fecha_limite_superior = as.Date("2025-12-31"),
  max_scrolls          = 200,
  espera_scroll        = c(4, 6),  
  espera_post          = c(3, 9),  
  espera_carga_inicial = c(3, 5),  
  max_publicaciones = 500,
  confirmacion_fin_rango = 2
)


# ---- SESIÓN AUTENTICADA -----------------------------------------------------

aplicar_cookies <- function(session, cookies_nombradas) {
  if (is.null(cookies_nombradas) || length(cookies_nombradas) == 0) {
    return(invisible(NULL))
  }
  for (nombre in names(cookies_nombradas)) {
    session$Network$setCookie(
      name   = nombre,
      value  = cookies_nombradas[[nombre]],
      domain = ".instagram.com",
      path   = "/",
      secure = TRUE
    )
  }
  invisible(NULL)
}


# ---- CONFIRMADO CON UNA PRUEBA MANUAL ---------------------------------------

aplicar_stealth <- function(session) {
  js_stealth <- r"(
(function() {
  // navigator.webdriver: la señal de automatización más común. Un Chrome
  // controlado por CDP la deja en `true` por defecto; un navegador humano
  // normal la tiene en `undefined`.
  Object.defineProperty(navigator, 'webdriver', { get: () => undefined });

  // window.chrome: Chrome headless a veces no lo define, o lo define de
  // una forma distinta a un Chrome normal con interfaz.
  if (!window.chrome) {
    window.chrome = { runtime: {} };
  }

  // navigator.languages: en algunos arranques de Chrome headless viene
  // vacío o distinto a lo que un usuario real tendría configurado.
  Object.defineProperty(navigator, 'languages', { get: () => ['es-CL', 'es', 'en-US', 'en'] });

  // navigator.plugins: Chrome headless clásico suele reportar 0 plugins,
  // algo que casi ningún navegador real tiene.
  Object.defineProperty(navigator, 'plugins', {
    get: () => [1, 2, 3, 4, 5].map(function() { return { name: 'Chrome PDF Plugin' }; })
  });

  // navigator.permissions.query('notifications'): en Chrome headless
  // clásico el resultado no coincide con Notification.permission, algo
  // que varios scripts de detección de bots comparan.
  var permisosOriginal = navigator.permissions && navigator.permissions.query;
  if (permisosOriginal) {
    navigator.permissions.query = function(params) {
      return (params && params.name === 'notifications')
        ? Promise.resolve({ state: Notification.permission })
        : permisosOriginal(params);
    };
  }

  // WebGL vendor/renderer: Chrome headless suele reportar un renderer de
  // software ("Google SwiftShader", "llvmpipe"), distinto al de una GPU
  // real - otra señal común de detección.
  if (window.WebGLRenderingContext) {
    var getParameterOriginal = WebGLRenderingContext.prototype.getParameter;
    WebGLRenderingContext.prototype.getParameter = function(parametro) {
      if (parametro === 37445) return 'Intel Inc.';               // UNMASKED_VENDOR_WEBGL
      if (parametro === 37446) return 'Intel Iris OpenGL Engine'; // UNMASKED_RENDERER_WEBGL
      return getParameterOriginal.call(this, parametro);
    };
  }
})();
)"
  
  tryCatch(
    session$Page$addScriptToEvaluateOnNewDocument(source = js_stealth),
    error = function(e) {
      warning(glue("No se pudo aplicar el parche de stealth: {e$message}"))
      NULL
    }
  )
  invisible(NULL)
}


# ---- CONFIRMADO EN VIVO -----------------------------------------------------

hacer_scroll_wheel <- function(session, n_ticks = 6, delta_por_tick = 350) {
  dims <- tryCatch(
    session$Runtime$evaluate(
      "JSON.stringify({w: window.innerWidth, h: window.innerHeight})",
      returnByValue = TRUE
    )$result$value,
    error = function(e) NULL
  )
  w <- 800; h <- 600
  if (!is.null(dims)) {
    dims_parseados <- tryCatch(jsonlite::fromJSON(dims), error = function(e) NULL)
    if (!is.null(dims_parseados)) { w <- dims_parseados$w; h <- dims_parseados$h }
  }
  
  fallo_input <- FALSE
  
  x <- max(20, min(w - 20, w / 2 + runif(1, -120, 120)))
  y <- max(20, min(h - 20, h / 2 + runif(1, -100, 100)))
  x_actual <- max(10, min(w - 10, x + runif(1, -150, 150)))
  y_actual <- max(10, min(h - 10, y + runif(1, -150, 150)))
  
  n_movimientos <- sample(2:4, 1)
  for (m in seq_len(n_movimientos)) {
    x_actual <- max(10, min(w - 10, x_actual + (x - x_actual) / 2 + runif(1, -40, 40)))
    y_actual <- max(10, min(h - 10, y_actual + (y - y_actual) / 2 + runif(1, -40, 40)))
    tryCatch(
      session$Input$dispatchMouseEvent(type = "mouseMoved", x = x_actual, y = y_actual),
      error = function(e) { fallo_input <<- TRUE }
    )
    if (fallo_input) break
    Sys.sleep(runif(1, 0.05, 0.15))
  }
  
  
  if (!fallo_input) Sys.sleep(runif(1, 0.1, 0.3))
  
  if (!fallo_input) {
    n_ticks_real <- max(1, n_ticks + sample(c(-1, 0, 0, 1), 1))
    for (i in seq_len(n_ticks_real)) {
      delta_real <- delta_por_tick + sample(-80:80, 1)
      resultado <- tryCatch(
        session$Input$dispatchMouseEvent(
          type = "mouseWheel",
          x = x_actual,
          y = y_actual,
          deltaX = 0,
          deltaY = delta_real
        ),
        error = function(e) { fallo_input <<- TRUE; NULL }
      )
      if (fallo_input) break
      Sys.sleep(runif(1, 0.08, 0.28))
    }
  }
  
  if (fallo_input) {

    session$Runtime$evaluate("window.scrollBy(0, document.body.scrollHeight);")
  }
  invisible(NULL)
}


# ---- 1. Visitar una publicación y extraer -----------------------------------

extraer_datos_publicacion <- function(session, url_publicacion) {
  session$Page$navigate(url_publicacion)

  tryCatch(session$Page$loadEventFired(wait_ = TRUE), error = function(e) NULL)
  Sys.sleep(runif(1, 2, 4))
  
  html <- session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  pagina <- read_html(html)
  
  
  fecha <- pagina %>% html_element("time") %>% html_attr("datetime")
  
  likes <- NA_real_
  comentarios <- NA_real_
  visualizaciones <- NA_real_
  
  json_ld <- pagina %>%
    html_elements("script[type='application/ld+json']") %>%
    html_text()
  
  if (length(json_ld) > 0) {
    for (bloque in json_ld) {
      datos <- tryCatch(jsonlite::fromJSON(bloque), error = function(e) NULL)
      if (!is.null(datos) && !is.null(datos$interactionStatistic)) {
        stats <- datos$interactionStatistic
        if (is.data.frame(stats)) {
          for (j in seq_len(nrow(stats))) {
            tipo <- stats$interactionType[[j]]
            valor <- suppressWarnings(as.numeric(stats$userInteractionCount[j]))
            if (is.character(tipo) && grepl("LikeAction", tipo, fixed = TRUE)) likes <- valor
            if (is.character(tipo) && grepl("CommentAction", tipo, fixed = TRUE)) comentarios <- valor
            if (is.character(tipo) && grepl("WatchAction", tipo, fixed = TRUE)) visualizaciones <- valor
          }
        }
      }
    }
  }
  

  if (is.na(likes) || is.na(comentarios)) {
    meta_desc <- pagina %>% html_element("meta[name='description']") %>% html_attr("content")
    if (!is.na(meta_desc)) {
      m <- str_match(
        meta_desc,
        "([0-9.,]+[KMkm]?)\\s*[Ll]ikes,\\s*([0-9.,]+[KMkm]?)\\s*[Cc]omments"
      )
      if (!is.na(m[1, 1])) {
        if (is.na(likes)) likes <- convertir_numero_abreviado(m[1, 2])
        if (is.na(comentarios)) comentarios <- convertir_numero_abreviado(m[1, 3])
      }
    }
  }
  

  if (is.na(visualizaciones)) {

    visualizaciones <- NA_real_
  }
  
  
  descripcion <- extraer_descripcion(pagina)
  
  tibble(
    fecha = fecha,
    me_gusta = likes,
    comentarios = comentarios,
    visualizaciones = visualizaciones,
    descripcion = descripcion,
    link = url_publicacion
  )
}


## ---- 1.1. Extrae el texto de la descripción/caption de una publicación -----

extraer_descripcion <- function(pagina) {
  
  json_ld <- pagina %>%
    html_elements("script[type='application/ld+json']") %>%
    html_text()
  
  if (length(json_ld) > 0) {
    for (bloque in json_ld) {
      datos <- tryCatch(jsonlite::fromJSON(bloque), error = function(e) NULL)
      if (!is.null(datos)) {
        if (!is.null(datos$caption) && is.character(datos$caption) && nzchar(datos$caption)) {
          return(datos$caption)
        }
        if (!is.null(datos$articleBody) && is.character(datos$articleBody) && nzchar(datos$articleBody)) {
          return(datos$articleBody)
        }
      }
    }
  }
  

  patron_entre_comillas <- stringr::regex("[“\"](.*)[”\"]", dotall = TRUE)
  
  meta_desc <- pagina %>% html_element("meta[name='description']") %>% html_attr("content")
  if (!is.na(meta_desc)) {
    m <- str_match(meta_desc, patron_entre_comillas)
    if (!is.na(m[1, 2]) && nzchar(m[1, 2])) return(m[1, 2])
  }
  

  og_desc <- pagina %>% html_element("meta[property='og:description']") %>% html_attr("content")
  if (!is.na(og_desc)) {
    m <- str_match(og_desc, patron_entre_comillas)
    if (!is.na(m[1, 2]) && nzchar(m[1, 2])) return(m[1, 2])
  }
  

  h1_texto <- pagina %>% html_element("h1") %>% html_text2()
  if (!is.na(h1_texto) && nzchar(h1_texto)) return(h1_texto)
  
  NA_character_
}


convertir_numero_abreviado <- function(x) {
  if (is.na(x)) return(NA_real_)
  x <- str_replace_all(x, ",", "")
  mult <- 1
  if (str_detect(x, "[Kk]$")) { mult <- 1e3; x <- str_remove(x, "[Kk]$") }
  if (str_detect(x, "[Mm]$")) { mult <- 1e6; x <- str_remove(x, "[Mm]$") }
  suppressWarnings(as.numeric(x) * mult)
}


tibble_vacio <- function() {
  tibble(
    fecha = character(), me_gusta = numeric(), comentarios = numeric(),
    visualizaciones = numeric(), descripcion = character(), link = character()
  )
}


## ---- 1.2. Patrón (versión R) para reconocer un link a una publicación ------

PATRON_LINK_PUBLICACION <- "^/([A-Za-z0-9._]+/)?(p|reel)/[A-Za-z0-9_-]+/?$"

obtener_links_pagina <- function(session) {
  js <- r"[
    Array.from(document.querySelectorAll('a'))
      .map(a => a.getAttribute('href'))
      .filter(h => h && /^\/([A-Za-z0-9._]+\/)?(p|reel)\/[A-Za-z0-9_-]+\/?$/.test(h))
  ]"

  resultado <- session$Runtime$evaluate(js, returnByValue = TRUE)
  links <- resultado$result$value
  if (is.null(links) || length(links) == 0) return(character(0))
  links <- unique(unlist(links))
  unique(paste0("https://www.instagram.com", links))
}


# ---- 2. Scroll + extracción combinados, con corte automático por fecha ------

extraer_publicaciones_en_rango <- function(session, url_perfil, nombre_cuenta, config,
                                           user_agent = NULL, cookies = NULL) {

  session$Page$navigate(url_perfil)

  tryCatch(session$Page$loadEventFired(wait_ = TRUE), error = function(e) NULL)
  Sys.sleep(runif(1, config$espera_carga_inicial[1], config$espera_carga_inicial[2]))
  

  intentos_espera_inicial <- 0
  while (length(obtener_links_pagina(session)) == 0 && intentos_espera_inicial < 4) {
    intentos_espera_inicial <- intentos_espera_inicial + 1
    message(glue(
      "[{nombre_cuenta}] Todavía no se ven publicaciones cargadas, ",
      "esperando un poco más ({intentos_espera_inicial}/4)..."
    ))
    Sys.sleep(3)
  }
  
  links_vistos <- character(0)
  resultados <- list()
  detener <- FALSE
  posts_antiguos_seguidos <- 0
  
  
  session_detalle <- NULL
  
  for (num_scroll in seq_len(config$max_scrolls + 1)) {
    # --- Extraer los links de publicación visibles hasta ahora ---
    links_pagina <- obtener_links_pagina(session)
    links_nuevos <- setdiff(links_pagina, links_vistos)
    
  
    message(glue(
      "[{nombre_cuenta}] Vuelta #{num_scroll}: {length(links_pagina)} links ",
      "totales en la página, {length(links_nuevos)} nuevos."
    ))
    
    
    for (link in links_nuevos) {
      links_vistos <- c(links_vistos, link)
      
      if (is.null(session_detalle)) {
        session_detalle <- ChromoteSession$new()
        on.exit(session_detalle$close(), add = TRUE)
        aplicar_stealth(session_detalle)
        if (!is.null(user_agent)) session_detalle$Network$setUserAgentOverride(userAgent = user_agent)
        if (!is.null(cookies)) aplicar_cookies(session_detalle, cookies)
        message(glue(
          "[{nombre_cuenta}] Se abre recién ahora la pestaña de detalle ",
          "(primera publicación nueva a visitar en esta corrida)."
        ))
      }
      
      datos <- tryCatch(
        extraer_datos_publicacion(session_detalle, link),
        error = function(e) {
          warning(glue("[{nombre_cuenta}] Error en {link}: {e$message}"))
          NULL
        }
      )
      Sys.sleep(runif(1, config$espera_post[1], config$espera_post[2]))
      
      if (is.null(datos) || is.na(datos$fecha)) {
        message(glue("[{nombre_cuenta}] Sin fecha legible en {link}, se omite."))
        next
      }
      
      fecha_post <- as.Date(datos$fecha)
      
      if (fecha_post > config$fecha_limite_superior) {
        
        message(glue(
          "[{nombre_cuenta}] Publicación del {fecha_post} es más reciente ",
          "que {config$fecha_limite_superior}, se descarta (se sigue ",
          "avanzando por el feed)."
        ))
        next
      }
      
      if (fecha_post < config$fecha_limite_inferior) {
        posts_antiguos_seguidos <- posts_antiguos_seguidos + 1
        message(glue(
          "[{nombre_cuenta}] Publicación del {fecha_post} es anterior a ",
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
      message(glue(
        "[{nombre_cuenta}] +1 publicación en rango ({fecha_post}) - ",
        "van {length(resultados)} guardadas."
      ))
      
      if (length(resultados) >= config$max_publicaciones) {
        message(glue("[{nombre_cuenta}] Se alcanzó max_publicaciones ({config$max_publicaciones})."))
        detener <- TRUE
        break
      }
    }
    
    if (detener) break
    
    if (num_scroll > config$max_scrolls) {
      message(glue(
        "[{nombre_cuenta}] Se alcanzó max_scrolls ({config$max_scrolls}) ",
        "sin confirmar el corte por fecha - revisa si esta cuenta necesita ",
        "un max_scrolls más alto, o si algo está bloqueando la carga."
      ))
      break
    }
    
  
    if (!is.null(session_detalle)) {
      tryCatch(session_detalle$Page$navigate("about:blank"), error = function(e) NULL)
    }
    
   
    intentos_scroll_max <- 10
    links_nuevos_tras_scroll <- character(0)
    for (intento_scroll in seq_len(intentos_scroll_max)) {
      hacer_scroll_wheel(session)
      espera_extra <- min(intento_scroll - 1, 5) * 2  # +0, +2, +4, ... hasta +10 seg
      Sys.sleep(runif(1, config$espera_scroll[1], config$espera_scroll[2]) + espera_extra)
      links_nuevos_tras_scroll <- setdiff(obtener_links_pagina(session), links_vistos)
      if (length(links_nuevos_tras_scroll) > 0) break
      if (intento_scroll < intentos_scroll_max) {
        message(glue(
          "[{nombre_cuenta}] El scroll #{intento_scroll} no trajo ",
          "publicaciones nuevas todavía, reintentando ",
          "({intento_scroll}/{intentos_scroll_max})..."
        ))
      }
    }
    
    if (length(links_nuevos_tras_scroll) == 0) {
      message(glue(
        "[{nombre_cuenta}] No se cargaron más publicaciones nuevas después ",
        "de {intentos_scroll_max} intentos de scroll - fin del perfil, o ",
        "Instagram dejó de entregar contenido."
      ))
      break
    }
  }
  
  if (length(resultados) == 0) return(tibble_vacio())
  bind_rows(resultados)
}


# ---- 3. Función principal: scrapea UNA cuenta completa ----------------------

scrape_instagram_account <- function(url_perfil, nombre_cuenta,
                                     config = config_cuenta_default,
                                     cookies = NULL) {
  user_agent_escritorio <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "(KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36"
  )
  
  b <- ChromoteSession$new()
  on.exit(b$close(), add = TRUE)
  aplicar_stealth(b)
  b$Network$setUserAgentOverride(userAgent = user_agent_escritorio)
  if (!is.null(cookies)) aplicar_cookies(b, cookies)
  
  
  message(glue(
    "[{nombre_cuenta}] Cargando perfil y buscando publicaciones entre ",
    "{config$fecha_limite_inferior} y {config$fecha_limite_superior}..."
  ))
  
  resultados <- tryCatch(
    extraer_publicaciones_en_rango(b, url_perfil, nombre_cuenta, config,
                                   user_agent = user_agent_escritorio, cookies = cookies),
    error = function(e) {
      warning(glue("[{nombre_cuenta}] Error general en el perfil: {e$message}"))
      tibble_vacio()
    }
  )
  
  resultados
}