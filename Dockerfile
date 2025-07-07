# Imagen base de R con Shiny Server
FROM rocker/shiny:latest

# Instalar paquetes R necesarios para tu app
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'lavaan', 'readxl', 'readr', 'DT', 'semPlot', 'openxlsx'))"

# Copiar todos los archivos de tu proyecto al contenedor
COPY . /srv/shiny-server/

# Exponer el puerto donde correrá la app
EXPOSE 3838

# Iniciar Shiny Server al arrancar
CMD ["/usr/bin/shiny-server"]
