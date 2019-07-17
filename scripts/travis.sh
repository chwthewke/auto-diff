#!/usr/bin/env bash

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"

$sbt_cmd package && \
  $sbt_cmd coverage test coverageReport && \
  $sbt_cmd publishLocal && \
  ( if [[ "$TRAVIS_SCALA_VERSION" == "2.13.0" ]]; then bash <(curl -s https://codecov.io/bash); fi )
