FROM rocker/verse:4.1
COPY .  /simaerep
RUN R -e "devtools::install('/simaerep/.', upgrade = 'never', dependencies = TRUE, repos = 'http://cran.us.r-project.org')" 
RUN rm /simaerep -r -f