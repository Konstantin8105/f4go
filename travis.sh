#!/bin/bash

set -e

echo "" > coverage.txt

export PKGS=$(go list ./...)

# Make comma-separated.
export PKGS_DELIM=$(echo "$PKGS" | paste -sd "," -)

go list -f 'go test -v -covermode atomic -coverprofile {{.Name}}.coverprofile -coverpkg $PKGS_DELIM {{.ImportPath}} ./...' $PKGS | xargs -I{} bash -c "{}"

# Merge coverage profiles.
COVERAGE_FILES=`ls -1 *.coverprofile 2>/dev/null | wc -l`
if [ $COVERAGE_FILES != 0 ]; then
	# check program `gocovmerge` is exist
	if which gocovmerge >/dev/null 2>&1; then
		gocovmerge `ls *.coverprofile` > coverage.txt
		rm *.coverprofile
	fi
fi
