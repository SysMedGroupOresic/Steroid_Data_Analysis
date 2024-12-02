FROM eddelbuettel/r2u:20.04
RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    && rm -rf /var/lib/apt/lists/*
RUN install.r shiny rmarkdown
RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis
COPY runtime-shiny .
RUN chown app:app -R C:/Users/patati/Documents/GitHub/Steroid_Data_Analysis
USER app
EXPOSE 3838
CMD ["R", "-e", "rmarkdown::run(shiny_args = list(port = 3838, host = '0.0.0.0'))"]