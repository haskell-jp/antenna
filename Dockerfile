FROM heroku/heroku:16

ENV LANG C.UTF-8

# Install required packages.
RUN apt-get update
RUN apt-get upgrade -y --assume-yes
# Install packages for stack and ghc.
RUN apt-get install -y --assume-yes xz-utils gcc libgmp-dev zlib1g-dev
# Install packages needed for libraries used by our app.
RUN apt-get install -y --assume-yes libpq-dev
# Install convenience utilities, like tree, ping, and vim.
RUN apt-get install -y --assume-yes tree iputils-ping vim-nox

# Remove apt caches to reduce the size of our container.
RUN rm -rf /var/lib/apt/lists/*

# Install stack to /opt/stack/bin.
RUN mkdir -p /opt/stack/bin
RUN curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C /opt/stack/bin '*/stack'

# Create /opt/antenna/bin and /opt/antenna/src.  Set
# /opt/antenna/src as the working directory.
RUN mkdir -p /opt/antenna/src
RUN mkdir -p /opt/antenna/bin
RUN mkdir -p /opt/antenna/config
WORKDIR /opt/antenna/src

# Set the PATH for the root user so they can use stack.
ENV PATH "$PATH:/opt/stack/bin:/opt/antenna/bin"

# Install GHC using stack, based on your app's stack.yaml file.
COPY ./stack.yaml /opt/antenna/src/stack.yaml
RUN stack --no-terminal setup

# Install all dependencies in app's .cabal file.
COPY ./antenna.cabal /opt/antenna/src/antenna.cabal
RUN stack --no-terminal test --only-dependencies

# Build application.
COPY . /opt/antenna/src
RUN stack --no-terminal build

# Install application binaries to /opt/antenna/bin.
RUN stack --no-terminal --local-bin-path /opt/antenna/bin install

# Copy config file
COPY ./config/feeds.yaml /opt/antenna/config

# Remove source code.
RUN rm -rf /opt/antenna/src

# Add the apiuser and setup their PATH.
RUN useradd -ms /bin/bash apiuser
RUN chown -R apiuser:apiuser /opt/antenna
USER apiuser
ENV PATH "$PATH:/opt/stack/bin:/opt/antenna/bin"

# Set the working directory as /opt/antenna/.
WORKDIR /opt/antenna

CMD /opt/antenna/bin/antenna-exe --env=prod
