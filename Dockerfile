# base image found at https://hub.docker.com/r/rocker/shiny
FROM rocker/shiny:3.6.3

# install system packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
	libxml2-dev \
	libcurl4-openssl-dev \
	libssl-dev 

# install additional R packages
RUN R -e "install.packages(c('tidyverse', 'shinythemes', 'reactable', 'datasets', 'devtools'))"

# copy app file
COPY vizLearn/ /srv/shiny-server/

# ensure permissions allow file readability
RUN chmod -R +r /srv/shiny-server/

# expose port
EXPOSE 3838

# chsh
SHELL ["/bin/bash", "-c"]

# run app at start
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/vizLearn.R', host='0.0.0.0', port=3838)"]
