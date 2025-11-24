# 🧠 PS3_Equipo3

Repositorio correspondiente al **Tercer Conjunto de Problemas (PS3)** del curso **Big Data y Machine Learning para Economía Aplicada (BDML)**.

---

## 📂 Estructura del repositorio

```
PS3_Equipo3/
├── scripts/            # Códigos fuente utilizados en la estimación y análisis
│   ├── taller3_datos.R / taller3_datosesp.R   # Limpieza y construcción de variables base (train.csv / test.csv)
│   ├── data_descriptive.R / mapas.R   # Generación de estadísticas descriptivas
│   ├── nn_sencilla.R        # Modelo Red Neuronal (ganador)
│   └── ...                 # Otros modelos de predicción
│  │
├── stores/
│   ├── train_final.csv           # Datos finales de entrenamiento (generados por data.R)
│   ├── test_final.csv            # Datos finales de prueba
│   ├── models/            # Archivos de predicciones de cada modelo
│
├── views/                  # Figuras y visualizaciones (e.g., maps)
│
├── document/               # Archivos LaTeX del informe final
│
└── slides/                 # Presentaciones utilizadas en clase
```

---

## 📦 Requisitos previos

Los archivos para extraer las variables geoespaciales en este proyecto son demasiado pesados para GitHub.  
Puedes descargarlos aquí:

https://uniandes-my.sharepoint.com/:f:/g/personal/c_lealr_uniandes_edu_co/IgBFK3xq7VwiRKMUvQaVva21AcXin9jL7b6K13Rq-bMl8fU?e=Z1TVLU

> 💡 **Importante:**  
> Descomprima dentro de la carpeta `stores/` antes de correr cualquier script.

---

## 🚀 Cómo ejecutar el proyecto

1. **Cree las bases de datos de modelado**  
   Ejecute los scripts principales de datos:

   ```r
   source("scripts/taller3_datos.R")
   source("scripts/taller3_datosesp.R")
   ```

   Este código:
   - Limpia y combina los archivos de entrada  
   - Crea las variables utilizadas en la estimación  
   - Genera los archivos `train_final.csv` y `test_final.csv` usados en los modelos predictivos  

2. **Entrene los modelos de predicción**  
   Cada script de modelo (por ejemplo `linear_regression.R`, `lasso.R`, `XGBOOST.R`) entrena un modelo distinto y genera su archivo de predicciones en:

   ```
   stores/modelos/
   ```

3. **Análisis descriptivo y visualizaciones**
   - `descriptive.R`: produce tablas descriptivas de los datos de entrenamiento.  
   - `maps.R`: genera los mapas y los guarda `views/`.

4. **Documentación**
   - Los archivos `.tex` en `document/` se usan para compilar el informe final.  
   - Las diapositivas de presentación están en `slides/`.

---

## 📊 Salidas principales

- **Predicciones**: `stores/models/*.csv`  
- **Historgramas y mapas**: `views/*.png`  
- **Informe académico (LaTeX)**: `document/*.tex`  
- **Presentaciones**: `slides/*.pdf`

---

## 🧩 Integrantes del equipo

- *Catalina Leal Rojas*  
- *Lucas Daniel Carrillo Aguirre*  
- *Lucas Eduardo Veras Costa*
- *Mateo Hernández*
 
---

## 📝 Notas finales

- Este repositorio está diseñado para replicar el flujo completo de trabajo del PS2.  
