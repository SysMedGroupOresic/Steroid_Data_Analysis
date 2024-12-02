# Käytä RStudio-palvelinta esiasennettuna (rocker-projekti tarjoaa virallisia RStudio Docker-kuvia)
#FROM rocker/r-base:latest
FROM rocker/rstudio:latest

# Aseta ympäristömuuttujat
ENV PASSWORD=asdf


# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/* \
	wget \ 
    graphviz \ 
    texlive-latex-extra \ 
    lmodern \ 
    perl && \ 
    /rocker_scripts/install_pandoc.sh && \
    install2.r rmarkdown

# Set environment variables for R package installation
ENV R_LIBS_USER=/usr/local/lib/R/site-library
#/mnt/c/Program\ Files/R/R-4.4.1/library
#ENV R_LIBS_USER=/mnt/c/Users/patati/Documents/GitHub/Steroid_Data_Analysis/renv/library/windows/R-4.4/x86_64-w64-mingw32
#RUN mkdir -p $R_LIBS_USER && chmod 777  $R_LIBS_USER

# Set environment variable for R libraries
#ENV R_LIBS_USER=/usr/local/lib/R/site-library


# run the container with the host cache mounted in the container
RUN Rscript -e "install.packages('remotes')"

RUN Rscript -e "remotes::install_version('lavaan', '0.6.14')"

ENV RSTUDIO_PANDOC=/usr/lib/rstudio/bin/pandoc

# Copy the existing library folder
COPY renv /usr/local/lib/R/site-library
#/home/rstudio/renv/library/R-4.4/x86_64-pc-linux-gnu
RUN chmod -R 777 /usr/local/lib/R/site-library

# Kopioi projektin tiedostot konttiin
# mkdir /mydocker3
WORKDIR /home/rstudio
COPY . /home/rstudio
COPY renv.lock renv.lock
RUN chmod -R 777 /home/rstudio/renv
RUN chmod -R 777 /home/rstudio/renv/library/R-4.4/x86_64-pc-linux-gnu
RUN chmod -R 777 /home/rstudio/renv/library/windows/R-4.4/x86_64-w64-mingw32
RUN chmod -R 777 /home/rstudio/renv/library/R-4.3/x86_64-w64-mingw32


# Set environment variables for renv to locate the packages
#ENV RENV_PATHS_LIBRARY=/mnt/c/Users/patati/Documents/GitHub/Steroid_Data_Analysis/renv/library/R-4.3/x86_64-w64-mingw32
#C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis/renv/library/windows/R-4.4/x86_64-w64-mingw32

# Optional: Install some R packages (you can add more as needed)
#RUN R -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
#RUN R -e ".libPaths( c(.libPaths(),'/mnt/c/Users/patati/Documents/GitHub/Steroid_Data_Analysis/renv/library/windows/R-4.4/x86_64-w64-mingw32') )"
#RUN R -e ".libPaths( c(.libPaths(),'C:\Users\patati\Documents\GitHub\Steroid_Data_Analysis\renv\library\windows\R-4.4\x86_64-w64-mingw32') )"

# Asenna riippuvuudet
#RUN R -e "install.packages(open ./.packages)"
#RUN R -e "install.packages(renv)"
#RUN R -e "library(renv)"

# the location of the renv cache on the host machine
#RENV_PATHS_CACHE_HOST=C:/Program Files/R/R-4.4.1/library
#/mnt/c/Users/patati/Documents/GitHub/Steroid_Data_Analysis/renv/library/R-4.3/x86_64-w64-mingw32

# where the cache should be mounted in the container
#mkdir /home/rstudio/renv/cache
#RENV_PATHS_CACHE_CONTAINER=/home/rstudio/renv/cache



# approach one
#ENV RENV_PATHS_LIBRARY /home/rstudio/renv/
#/usr/local/lib/R/site-library

# approach two
RUN mkdir -p renv
COPY .Rprofile .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json

RUN R -e "renv::restore()"



# Avaa säiliön portti RStudio-palvelinta varten
EXPOSE 8787
#CMD ["R"]

#docker run --rm \
#    -e "RENV_PATHS_CACHE=${RENV_PATHS_CACHE_CONTAINER}" \
#    -v "${RENV_PATHS_CACHE_HOST}:${RENV_PATHS_CACHE_CONTAINER}" \
#    -p 8787:8787 \
#    R -s -e 'renv::restore(); shiny::runApp(host = "0.0.0.0", port = 8787)'

#14618
