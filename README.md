# Dashboard R Shiny para el Instituto Nacional de Estadísticas (INE)

Se puede acceder al dashboard desde este link: https://miguel412.shinyapps.io/app_ine/

## Descripción

Este dashboard fue creado para el Instituto Nacional de Estadísticas (INE), en su sede regional en Talca. Muestra varias tasas y totales relacionados al Mercado Laboral, más específicamente a lo relacionado a la Encuesta Nacional de Empleo (ENE). También se presentan gráficos de líneas, de barras y separados por sexo para cada elemento.

## Imagen referencial del dashboard

![dashboard_ine](https://github.com/miguellopez4/R-Shiny-Dashborad-INE-Chile/assets/89881027/952fe87f-f037-4841-97fa-f56ce23abf50)

## Datos Usados

Link a los datos: https://www.ine.gob.cl/estadisticas/sociales/mercado-laboral/ocupacion-y-desocupacion

Los datos utilizados son públicos y están alojados en la página web oficial del INE en la sección Estaísticas &rarr; Mercado Laboral &rarr; Ocupación y desocupación &rarr; Bases de datos. Para la creación del dashboard se utilizaron los datos comprendidos desde el año 2020 en el trimestre Junio-Julio-Agosto (JJA) hasta el año 2023 en el mismo trimestre JJA. En caso de ser necesarios otros periodos los datos de interés deben ser alojados dentro de la carpeta **data** del presente repositorio.

## Funcionamiento

Para el funcionamiento del dashboard es necesario crear los archivos de datos **tabla.csv** y **tabla_sexos.csv**, como se explicó anteriormente estos archivos ya están creados utilizando el periodo anteriormente indicado. La creación de estos datos es posible utilizando el archivo **crear_tablas.R** del repositorio.

Una vez creados los archivos de datos anteriormente indicados es necesario alojarlos en el mismo directorio de trabajo que el archivo **App_Shiny.r**, este archivo da origen al dashboard y es en el que se pueden realizar modificaciones visuales en caso de ser necesario.

## Notas de Version

### Versión 1.0.0 (28 de diciembre de 2023)

* Publicación inicial: Primera Versión del dashboard.

Este proyecto está licenciado bajo la Licencia MIT. Consulta el archivo [LICENSE](LICENSE) para obtener más detalles.
