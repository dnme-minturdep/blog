# blog
Bitácora de Turismo


### Descripción del proyecto :speech_balloon:

Este repositorio contiene el código fuente del _blog_ Bitácora de Turismo, espacio virtual de la Dirección Nacional de Mercados y Estadística (DNMyE) 
en el que nos proponemos compartir avances de investigaciones, datos y reportes relacionados con el desarrollo de la industria turística de Argentina.

### Objetivo del Repositorio :dart:

- Almacenamiento de código base para el despliegue del sitio 

### Instrucciones de usos :building_construction:

Para actualizar el contenido del *blog* es necesario hacer un *fork* del repo.

-   El blog está armado con el[ 📦 `{distill}`](<https://rstudio.github.io/distill/>)

-   Para crear un nuevo *post* ejecutar el siguiente código `distill::create_post(title = "Titulo del Post", draft = TRUE)` . El mismo genera un nuevo archivo `.Rmd` a partir del cual se genera la publicación, pero setéa en el `YAML` (encabezado del documento) que el mismo será borrador.

-   Para empezar a listar la publicación en el landing del *blog* hay que cambiar el parámetro `draft = TRUE` en el `YAML` del `.Rmd`

