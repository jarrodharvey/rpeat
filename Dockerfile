FROM openanalytics/r-base

MAINTAINER Jarrod Harvey

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0 \
    libxml2-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.ms.unimelb.edu.au/')"

# install dependencies of the RPEAT app
RUN R -e "install.packages(c('elastic', 'stringr', 'tibble', 'dplyr', 'XML', 'shinyjs', 'filesstrings', 'DT'), repos='https://cran.ms.unimelb.edu.au/')"

# copy the app to the image
RUN mkdir /root/RPEAT
COPY RPEAT /root/RPEAT

# Set the port as 3838 - this is the port that ShinyProxy is expecting
COPY Rprofile.site /usr/lib/R/etc/

# Expose the RPEAT port
EXPOSE 3838
# Expose the elasticsearch port (RPEAT uses this to check for duplicate digital objects)
EXPOSE 9200

CMD ["R", "-e", "shiny::runApp('/root/RPEAT')"]