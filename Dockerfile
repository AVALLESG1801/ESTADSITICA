FROM rocker/shiny:4.4.0

# Instalar las librerías necesarias
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'readxl', 'readr', 'lavaan', 'semPlot', 'openxlsx'), repos='https://cloud.r-project.org/')"

# Copiar los archivos de la app al servidor shiny
COPY . /srv/shiny-server/

# Cambiar permisos de la carpeta
RUN chown -R shiny:shiny /srv/shiny-server

# Exponer el puerto 3838
EXPOSE 3838

# Ejecutar shiny-server al levantar el contenedor
CMD ["/usr/bin/shiny-server"]
