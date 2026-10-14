# DockerDemo: the Linux64 daemon of a MARS server in a container.
# Build DockerDemoServerDaemon for Linux64 first (it lands in bin), then:
#   docker build -t mars-dockerdemo .
#   docker run -d --name mars-dockerdemo -p 8080:8080 mars-dockerdemo
# or: docker compose up -d --build

# Close to the Linux SDK the binary is built with (RAD Studio 13: Ubuntu 24.04)
FROM --platform=linux/amd64 ubuntu:24.04

# The daemon needs only libc, libz and the like, already in the base image.
# OpenSSL only if the server terminates HTTPS itself:
# RUN apt-get update \
#  && apt-get install -y --no-install-recommends libssl3 ca-certificates \
#  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY bin/DockerDemoServerDaemon bin/DockerDemoServerDaemon.ini bin/Server.ini /app/
RUN chmod +x /app/DockerDemoServerDaemon

# not as root
RUN useradd --system --no-create-home mars && chown -R mars /app
USER mars

EXPOSE 8080
# in foreground: the main process of the container, logs on stdout, SIGTERM (docker stop) stops it
CMD ["./DockerDemoServerDaemon", "--foreground"]
