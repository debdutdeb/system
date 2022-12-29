FROM --platform=amd64 ubuntu:focal

ARG UID=
ARG GID=
ARG USER=

RUN DEBIAN_FRONTEND=noninteractive apt-get update && \
	DEBIAN_FRONTEND=noninteractive apt-get install -y \
	build-essential \
	cmake \
	curl \
	python3 \
	python-is-python3 \
	git \
	ca-certificates

RUN mkdir -p /opt/nvm && \
	curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh |NVM_DIR=/opt/nvm  bash && \
	BASH_ENV=/opt/nvm/nvm.sh bash -c 'nvm install 16' && \
	BASH_ENV=/opt/nvm/nvm.sh bash -c 'npm install tree-sitter -g'

RUN { groupadd -g $GID $USER || true; } && \
	useradd -mG sudo $USER -u $UID -g $GID && \
	chown -R $UID:$GID /opt/nvm && \
	echo 'export NVM_DIR="/opt/nvm"' >>/home/$USER/.bashrc && \
	echo '[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm '>>/home/$USER/.bashrc && \
	echo '[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion' >> /home/$USER/.bashrc

RUN curl -fsSL https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.deb -O && \
	curl -fsSL https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.deb.sha256sum -O && \
	sha256sum -c ./nvim-linux64.deb.sha256sum && \
	dpkg -i ./nvim-linux64.deb && \
	rm -f ./nvim-linux64.deb ./nvim-linux64.deb.sha256sum && \
	mkdir /home/$USER/.local/share/nvim /home/$USER/.config -p && \
	chown $UID:$GID /home/$USER/.local /home/$USER/.config -R 

# Install go because I love go :P
ADD https://go.dev/dl/go1.19.4.linux-amd64.tar.gz /tmp/go1.19.4.linux-amd64.tar.gz

RUN tar -C /usr/local -xzf /tmp/go1.19.4.linux-amd64.tar.gz && \
	rm /tmp/go1.19.4.linux-amd64.tar.gz

COPY --chown=$UID:$GID . /home/$USER/.config/nvim

USER $USER

VOLUME [ "/home/$USER/.local/share/nvim" ]

WORKDIR /home/$USER

ENV PATH $PATH:/usr/local/go/bin

CMD ["/bin/bash"]
