FROM erlang:22-alpine as builder

WORKDIR /build
COPY . /build
RUN rebar3 as prod tar

FROM alpine:3.9

RUN apk --upgrade add --no-cache ncurses-dev zlib-dev bash strace

ENV god_token="myToken"

COPY --from=builder /build/_build/prod/rel/minimonkey/minimonkey-0.0.1.tar.gz /tmp
RUN tar xvfh /tmp/minimonkey-0.0.1.tar.gz -C /usr/local

EXPOSE 1773

CMD ["/usr/local/bin/minimonkey", "foreground"]
