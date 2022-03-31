stack-ci:
  #!/usr/bin/env bash
  set -eux

  for STACK_YAML in stack*yaml ; do
    export STACK_YAML
    echo testing $STACK_YAML
    stack test --ghc-options=-Wall
  done
