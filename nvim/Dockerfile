FROM --platform=amd64 ubuntu:jammy

ARG USER=debdut

RUN useradd -mG sudo $USER -s /bin/bash && apt-get update && apt-get install --no-install-recommends sudo && echo "ALL ALL = (ALL) NOPASSWD: ALL" >> /etc/sudoers.d/passwordless-sudo

USER debdut

COPY ./setup.sh /setup.sh

RUN bash /setup.sh

CMD ["/bin/bash"]
