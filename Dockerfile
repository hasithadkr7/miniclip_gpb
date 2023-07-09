# Build stage 0
FROM erlang:25-alpine

# Add git
RUN apk update
RUN apk add git

RUN mkdir /build
WORKDIR /build

RUN git clone https://github.com/hasithadkr7/miniclip_gpb.git

WORKDIR miniclip_gpb

#Build the release
RUN rebar3 as prod release

RUN rebar3 as prod tar

FROM alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
     apk add --no-cache libstdc++ && \
      apk add --no-cache libgcc

COPY --from=0 /build/miniclip_gpb/_build/prod/rel/miniclip_gpb /miniclip_gpb

# specific env variables to act as defaults
ENV LOG_LEVEL=debug \
    WORKER_COUNT=5 \
    PORT=5555 \
    ACCESS_KEY_ID="access_key_id" \
    SECRET_ACCESS_KEY="secret_access_key" \
    KMS_KEY_ID="kms_key_id" \
    TABLE_NAME="gpb_data"

# Expose relevant ports
EXPOSE $PORT

CMD ["/miniclip_gpb/bin/miniclip_gpb", "foreground"]
