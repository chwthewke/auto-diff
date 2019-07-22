#!/usr/bin/env bash
set -e

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

$sbt_cmd publishLocal

$sbt_cmd coverage auto-diff-coverage/test auto-diff-coverage/coverageReport

if [[ "$TRAVIS_SCALA_VERSION" =~ ^2.13 ]]; then
  bash <(curl -s https://codecov.io/bash)
fi 
