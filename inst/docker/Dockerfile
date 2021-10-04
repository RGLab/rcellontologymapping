

FROM rglab/hipccyto@sha256:34f605c8820c6946c81a5a101dfc31afe767b65421c29d40236e7a073c9c2d81 AS build


# set a user? USER who:who

RUN curl -sSL https://get.haskellstack.org/ | sh


# need to install rjson

COPY rjson_0.2.20.tar.gz rcellontologymapping_0.0.2.0.tar.gz /root/
RUN R CMD INSTALL /root/rjson_0.2.20.tar.gz
USER root:root
RUN ["mkdir", "-p", "/root/.local/bin"]
RUN ["cp", "/usr/local/bin/stack",  "/root/.local/bin/stack"]
RUN R CMD INSTALL /root/rcellontologymapping_0.0.2.0.tar.gz
RUN ["cp", "./root/.local/bin/cell-ontology-mapping",  "/usr/local/bin/cell-ontology-mapping"]


FROM rglab/hipccyto@sha256:34f605c8820c6946c81a5a101dfc31afe767b65421c29d40236e7a073c9c2d81 AS final
COPY --from=build /usr/local/bin/cell-ontology-mapping /usr/local/bin/cell-ontology-mapping
COPY --from=build /usr/local/lib/R/site-library/rjson/ /usr/local/lib/R/site-library/rjson/
COPY --from=build /usr/local/lib/R/site-library/rcellontologymapping/ /usr/local/lib/R/site-library/rcellontologymapping/



