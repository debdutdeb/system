FROM --platform=linux/amd64 ubuntu:22.04

RUN apt update && \
  apt install -y \
    golang \
    git \
    build-essential \
    unzip \
    sudo \
    curl \
    ca-certificates \
    cmake \
    fd-find \
    ripgrep \
    fzf && \
  useradd -mG sudo debdut -s /bin/bash && \
  echo "%debdut ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/debdut 

ADD https://github.com/neovim/neovim/releases/download/nightly/nvim-linux64.deb /tmp/nvim.deb

RUN dpkg -i /tmp/nvim.deb && \
  rm /tmp/nvim.deb && \
  mkdir /opt/nvm && \
  chown debdut:debdut /opt/nvm -R

USER debdut

WORKDIR "/home/debdut"

RUN curl -L https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | NVM_DIR=/opt/nvm bash && \
  BASH_ENV=/opt/nvm/nvm.sh bash -c "nvm install 16" && \
  git clone https://github.com/debdutdeb/neoconfig .config/nvim && \
  curl https://sh.rustup.rs -sSf | bash -s -- -y && \
  BASH_ENV=.cargo/env bash -c "cargo install tree-sitter-cli"

VOLUME [ "$HOME/.local/share/nvim" ]

CMD [ "/bin/bash" ]
