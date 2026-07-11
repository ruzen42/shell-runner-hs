FROM haskell:9.10.2 AS build

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        git \
        pkg-config \
        zlib1g-dev && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY package.yaml stack.yaml ./
COPY src/ src/
COPY app/ app/

RUN stack install --copy-bins --local-bin-path /app/.local/bin

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        zlib1g \
        libgmp-dev \
        ca-certificates \
        bash && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/.local/bin/shell-runner-exe /app/shell-runner

EXPOSE 8080
CMD ["/app/shell-runner" "8080"]
