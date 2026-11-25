FROM public.ecr.aws/amazonlinux/amazonlinux:2023 AS build
RUN dnf install -y tar gzip && dnf clean all
ENV ELAN_HOME=/opt/elan PATH=/opt/elan/bin:$PATH
WORKDIR /src
COPY lean-toolchain ./
RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | \
      sh -s -- -y --no-modify-path --default-toolchain "$(cat lean-toolchain)" && \
    elan toolchain install "$(cat lean-toolchain)"
COPY lakefile.toml lake-manifest.json Main.lean Linter.lean ./
COPY Linter Linter
RUN lake build linter

FROM public.ecr.aws/lambda/provided:al2023
RUN dnf install -y jq unzip gzip && dnf clean all
COPY --from=build /src/.lake/build/bin/linter /usr/local/bin/linter
COPY service/bootstrap ${LAMBDA_RUNTIME_DIR}/bootstrap
ENV LINTER_TIMEOUT=25 LEAN_STACK_SIZE_KB=8192
CMD ["linter"]
