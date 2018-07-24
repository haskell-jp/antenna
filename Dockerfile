FROM mcuadros/ofelia AS ofelia
FROM antenna-bin

RUN apt-get update && apt-get install -y \
    ca-certificates \
    git \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

COPY --from=ofelia /usr/bin/ofelia /usr/bin/ofelia
COPY ofelia/config.ini /etc/ofelia/
COPY bin/append-slack-webhook.sh /usr/bin/
COPY bin/run.sh /usr/bin/

WORKDIR /work

COPY sites.yaml /work/

ENTRYPOINT ["/usr/bin/append-slack-webhook.sh"]

CMD ["/usr/bin/ofelia", "daemon", "--config", "/etc/ofelia/config.ini"]
