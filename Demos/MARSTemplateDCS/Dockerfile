# The Linux64 daemon of this MARS server in a container.
# Build MARSTemplateServerDCSDaemon for Linux64 first (docker-build.bat on Windows: it lands in bin), then,
# where Docker runs, in this folder:
#   docker compose up -d --build
# or:
#   docker build -t myserver .
#   docker run -d --name myserver -p 8080:8080 myserver

# Close to the Linux SDK the binary is built with (RAD Studio 13: Ubuntu 24.04).
# The binary is x86-64: on ARM hosts (Apple Silicon) Docker runs it through emulation.
FROM --platform=linux/amd64 ubuntu:24.04

# The daemon needs only libc, libz and the like, already in the base image.
# OpenSSL only if the server terminates HTTPS itself:
# RUN apt-get update \
#  && apt-get install -y --no-install-recommends libssl3 ca-certificates \
#  && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY bin/MARSTemplateServerDCSDaemon bin/MARSTemplateServerDCSDaemon.ini bin/Server.ini /app/
RUN chmod +x /app/MARSTemplateServerDCSDaemon

# not as root
RUN useradd --system --no-create-home mars && chown -R mars /app
USER mars

EXPOSE 8080
# in foreground: the main process of the container, logs on stdout, SIGTERM (docker stop) stops it
CMD ["./MARSTemplateServerDCSDaemon", "--foreground"]
