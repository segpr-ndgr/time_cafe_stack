# docker build --platform linux/amd64 -t hugoavmedeiros/suprimentos_ses-app:latest -f docker/Dockerfile .
# docker build --platform linux/amd64 -t hugoavmedeiros/suprimentos_ses-app:latest -f docker/Dockerfile . --no-cache
# docker push hugoavmedeiros/suprimentos_ses-app:latest
# docker run -p 3838:3838 hugoavmedeiros/suprimentos_ses-app:latest
# http://localhost:3838/
ARG DEBIAN_FRONTEND=noninteractive
FROM hugoavmedeiros/r-min-dev:latest

RUN installr -d \
  -t "R-dev openssl-dev libpq-dev linux-headers gfortran proj-dev gdal-dev sqlite-dev geos-dev udunits-dev" \
  -a "libssl3 proj gdal geos expat udunits libpq" \
  bsicons bslib cachem DBI dbplyr dplyr glue gt htmltools htmlwidgets \
  later lubridate plotly pool purrr qs reactable rlang RPostgres safer \
  scales shiny shinyFeedback shinyjs shinymanager sodium spsComps \
  stringi stringr tidyr vialactea waiter

COPY app_suprimentos_ses /app
WORKDIR /app

EXPOSE 3838
CMD ["R", "-e", "options(tz='America/Recife');shiny::runApp('/app', port = 3838, host = '0.0.0.0')"]
