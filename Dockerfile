FROM alpine:3.16 AS build
ARG VERSION=master

RUN apk upgrade --update musl \
    && apk add \
    autoconf \
    automake \
    bash \
    build-base \
    curl \
    elixir \
    erlang-odbc \
    erlang-reltool \
    expat-dev \
    file \
    gd-dev \
    git \
    jpeg-dev \
    libpng-dev \
    libwebp-dev \
    linux-pam-dev \
    openssl \
    openssl-dev \
    sqlite-dev \
    yaml-dev \
    nodejs \
    npm \
    nano \
    zlib-dev \
    python3
RUN ln -sf python3 /usr/bin/python
RUN python3 -m ensurepip
RUN pip3 install --no-cache --upgrade pip setuptools

RUN mix local.hex --force \
    && mix local.rebar --force

COPY . ./ejabberd

WORKDIR ejabberd

RUN mv .github/container/ejabberdctl.template . \
    && ./autogen.sh \
    && ./configure --with-rebar=mix --enable-all \
    && make deps \
    && make rel

RUN cp -r _build/prod/rel/ejabberd/ /opt/ejabberd-$VERSION \
    && mkdir -p /opt/ejabberd \
    && mv /opt/ejabberd-$VERSION/conf /opt/ejabberd/conf
COPY ./js /opt/js
COPY ./python /opt/python
RUN BINPATH=$(dirname $(find /opt -name msgs))/bin/ \
    && mkdir -p $BINPATH \
    && cp tools/captcha*.sh $BINPATH

RUN [ ! -d .ejabberd-modules ] || cp -r .ejabberd-modules /opt/ejabberd/

RUN export PEM=/opt/ejabberd/conf/server.pem \
    && curl -o "/opt/ejabberd/conf/cacert.pem" 'https://curl.se/ca/cacert.pem' \
    && openssl req -x509 \
    -batch \
    -nodes \
    -newkey rsa:4096 \
    -keyout $PEM \
    -out $PEM \
    -days 3650 \
    -subj "/CN=localhost" \
    && sed -i '/^loglevel:/a \ \
\nca_file: /opt/ejabberd/conf/cacert.pem \
    \ncertfiles: \
    \n  - /opt/ejabberd/conf/server.pem' "/opt/ejabberd/conf/ejabberd.yml"

FROM alpine:3.16
ENV HOME=/opt/ejabberd
ARG VERSION=master

RUN apk upgrade --update musl \
    && apk add \
    expat \
    freetds \
    gd \
    jpeg \
    libgd \
    libpng \
    libstdc++ \
    libwebp \
    linux-pam \
    ncurses-libs \
    openssl \
    sqlite \
    sqlite-libs \
    unixodbc \
    yaml \
    zlib \
    nodejs \
    npm \
    nano \
    python3 \ 
    && ln -fs /usr/lib/libtdsodbc.so.0 /usr/lib/libtdsodbc.so \
    && rm -rf /var/cache/apk/*

RUN ln -sf python3 /usr/bin/python
RUN python3 -m ensurepip
RUN pip3 install --no-cache --upgrade pip setuptools

COPY --from=build /opt /opt
RUN echo -e \
    "#!/bin/sh \
    \n[ -z \$ERLANG_NODE_ARG ] && export ERLANG_NODE_ARG=ejabberd@localhost \
    \nexport CONFIG_DIR=/opt/ejabberd/conf \
    \nexport LOGS_DIR=/opt/ejabberd/logs \
    \nexport SPOOL_DIR=/opt/ejabberd/database \
    \nexec /opt/ejabberd-$VERSION/bin/ejabberdctl \"\$@\"" > /usr/local/bin/ejabberdctl \
    && chmod +x /usr/local/bin/ejabberdctl

RUN addgroup ejabberd -g 9000 \
    && adduser -s /bin/sh -D -G ejabberd ejabberd -u 9000 \
    && mkdir -p $HOME/conf $HOME/database $HOME/logs $HOME/upload \
    && chown -R ejabberd:ejabberd $HOME

HEALTHCHECK \
    --interval=1m \
    --timeout=5s \
    --start-period=5s \
    --retries=10 \
    CMD /usr/local/bin/ejabberdctl status

WORKDIR $HOME
USER ejabberd
VOLUME ["$HOME/conf", "$HOME/database", "$HOME/logs", "$HOME/upload"]
EXPOSE 1883 4369-4399 5210 5222 5269 5280 5443

# RUN node index.js

ENTRYPOINT ["/usr/local/bin/ejabberdctl"]
CMD ["foreground"]
