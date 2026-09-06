"""
05_analizar_emocion_odio_ironia.py

Clasificación de las descripciones de Instagram con los analizadores en
español de la librería pysentimiento (modelos RoBERTuito), para las tareas:

  - emotion      (pysentimiento/robertuito-emotion-analysis)
        7 clases, single-label (softmax, excluyentes entre sí):
        others, joy, sadness, anger, surprise, disgust, fear.

  - hate_speech  (pysentimiento/robertuito-hate-speech)
        3 categorías NO excluyentes, multi-label (sigmoide independiente
        por categoría -mismo espíritu que las 12 hipótesis NLI de
        02_clasificar_hipotesis.py-): hateful, targeted, aggressive.
        Basado en el esquema HatEval: "hateful" = es discurso de odio,
        "targeted" = ataca a un individuo específico (no a un grupo en
        general), "aggressive" = usa lenguaje agresivo/ofensivo.
        pysentimiento decide internamente qué categorías quedan
        "activas" con el mismo criterio simple de umbral 0.5 aplicado a
        cada probabilidad por separado (ver AnalyzerOutput en la
        librería) -no hay nada adicional ahí, igual que con UMBRAL en
        02_clasificar_hipotesis.py.

  - irony        (pysentimiento/robertuito-irony)
        2 clases, single-label (softmax): "ironic", "not ironic".

No incluye la tarea general "sentiment" (POS/NEG/NEU) de pysentimiento
porque no fue pedida. Si se quiere agregar después, basta con sumar
"sentiment" a la lista TASKS de abajo -el resto del script funciona
igual sin más cambios, ya que las columnas de salida se arman
dinámicamente a partir de lo que cada analizador devuelve.

------------------------------------------------------------------------
NOTA METODOLÓGICA
------------------------------------------------------------------------
Los modelos RoBERTuito de pysentimiento fueron preentrenados y afinados
sobre tuits en español (no sobre descripciones de Instagram), pero su
registro (texto corto, hashtags, emojis, menciones) es razonablemente
cercano al de las descripciones de este corpus. La función interna
preprocess_tweet() ya normaliza URLs, menciones y hashtags antes de
clasificar, así que no hace falta preprocesar el texto a mano.

NOTA DE COMPATIBILIDAD: pysentimiento usa internamente la clase
Trainer/TrainingArguments de transformers para hacer inferencia por
lotes. Si tu versión de transformers es muy reciente, esa API interna
puede haber cambiado y el import podría fallar; si eso ocurre, la
solución más simple es instalar pysentimiento en un entorno separado
con una versión de transformers algo más antigua (4.x), o revisar el
mensaje de error para fijar la versión que pide.
------------------------------------------------------------------------

Requisitos (instalar una sola vez en tu entorno local):
    pip install pysentimiento torch pandas tqdm

Input : dataset_unido.csv (generado por 01_preparar_datos.py)
Output: analisis_emocion_odio_ironia.csv
"""

import os
import time

import pandas as pd
from tqdm import tqdm
from pysentimiento import create_analyzer

# ------------------------------------------------------------------
# Configuración
# ------------------------------------------------------------------
INPUT_PATH = os.environ.get("INPUT_PATH", "dataset_unido.csv")
OUTPUT_PATH = os.environ.get("OUTPUT_PATH", "analisis_emocion_odio_ironia.csv")

TASKS = ["emotion", "hate_speech", "irony"]
LANG = "es"

BATCH_SIZE = int(os.environ.get("BATCH_SIZE", 32))
CHECKPOINT_EVERY = int(os.environ.get("CHECKPOINT_EVERY", 200))  # filas


def _sanitize(nombre):
    """Convierte un label de pysentimiento (p.ej. 'not ironic') en un
    nombre de columna seguro (p.ej. 'not_ironic')."""
    return nombre.strip().lower().replace(" ", "_")


def clasificar_lote(analyzers, textos):
    """Corre las 3 tareas sobre un lote de textos y devuelve una lista de
    dicts {columna: valor} -uno por texto- con las probabilidades de cada
    tarea (prob_<tarea>_<clase>) más la etiqueta que resulta de esa tarea:
    - tareas single-label  -> columna '<tarea>_etiqueta' (un solo valor)
    - tareas multi-label   -> columna '<tarea>_etiquetas' (lista separada
      por coma, o 'ninguna' si ninguna categoría superó el umbral)."""
    filas = [dict() for _ in textos]

    for task, analyzer in analyzers.items():
        salidas = analyzer.predict(textos)
        if not isinstance(salidas, list):  # por si acaso llega un solo output
            salidas = [salidas]

        for fila, salida in zip(filas, salidas):
            for label, prob in salida.probas.items():
                fila[f"prob_{task}_{_sanitize(label)}"] = prob
            if salida.is_multilabel:
                fila[f"{task}_etiquetas"] = ", ".join(salida.output) if salida.output else "ninguna"
            else:
                fila[f"{task}_etiqueta"] = salida.output

    return filas


def main():
    df = pd.read_csv(INPUT_PATH)
    n_total = len(df)
    print(f"Publicaciones a clasificar: {n_total}")
    print(f"Tareas: {', '.join(TASKS)} (lang={LANG})")

    textos = df["descripcion"].astype(str).tolist()

    # --- Retomar desde checkpoint si existe (por si una corrida anterior se cortó) ---
    checkpoint_path = OUTPUT_PATH + ".checkpoint.csv"
    filas_resultado = []
    inicio = 0

    if os.path.exists(checkpoint_path):
        parcial_previo = pd.read_csv(checkpoint_path)
        columnas_nuevas = [c for c in parcial_previo.columns if c not in df.columns]
        if len(parcial_previo) <= n_total and columnas_nuevas:
            inicio = len(parcial_previo)
            filas_resultado = parcial_previo[columnas_nuevas].to_dict(orient="records")
            print(f"Checkpoint encontrado: retomando desde la publicación {inicio}/{n_total}.")
        else:
            print("Aviso: el checkpoint no coincide con el dataset actual; se ignora y se parte de cero.")

    if inicio >= n_total:
        print("El checkpoint ya tiene todas las publicaciones clasificadas; solo falta consolidar el resultado.")

    t0 = time.time()
    analyzers = {task: create_analyzer(task=task, lang=LANG, batch_size=BATCH_SIZE) for task in TASKS}
    print(f"Analizadores cargados en {time.time() - t0:.1f}s")

    BATCHES_POR_CHECKPOINT = max(1, CHECKPOINT_EVERY // BATCH_SIZE)
    contador_batches = 0
    t0 = time.time()
    for start in tqdm(range(inicio, len(textos), BATCH_SIZE), desc="Clasificando"):
        lote = textos[start:start + BATCH_SIZE]
        filas_resultado.extend(clasificar_lote(analyzers, lote))
        contador_batches += 1

        # checkpoint incremental por si el proceso se corta
        if contador_batches % BATCHES_POR_CHECKPOINT == 0:
            parcial = df.iloc[:len(filas_resultado)].copy()
            nuevas = pd.DataFrame(filas_resultado)
            for col in nuevas.columns:
                parcial[col] = nuevas[col].values
            parcial.to_csv(checkpoint_path, index=False, encoding="utf-8-sig")

    print(f"Clasificación completa en {(time.time() - t0)/60:.1f} min")

    nuevas = pd.DataFrame(filas_resultado)
    for col in nuevas.columns:
        df[col] = nuevas[col].values

    df.to_csv(OUTPUT_PATH, index=False, encoding="utf-8-sig")
    print(f"Guardado: {OUTPUT_PATH}")

    # Limpiar checkpoint si todo salió bien
    ckpt = OUTPUT_PATH + ".checkpoint.csv"
    if os.path.exists(ckpt):
        os.remove(ckpt)

    # --- Resúmenes rápidos ---
    print("\nDistribución de emotion_etiqueta:")
    print(df["emotion_etiqueta"].value_counts())

    print("\nDistribución de irony_etiqueta:")
    print(df["irony_etiqueta"].value_counts())

    print("\nFrecuencia de cada categoría de hate_speech (multi-etiqueta, no excluyentes):")
    for label in ["hateful", "targeted", "aggressive"]:
        col = f"prob_hate_speech_{label}"
        if col in df.columns:
            n = int((df[col] > 0.5).sum())
            print(f"  {label}: {n} ({n/len(df):.1%})")


if __name__ == "__main__":
    main()
