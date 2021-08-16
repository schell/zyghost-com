#! /bin/bash
#
# Builds and pushes the site
set -e

SITE=$1
if [ -z "${SITE}" ]; then
    echo "!!! First argument should be the name of the site, eg 'preview.zyghost.com'"
fi

CF_DISTRO=$2
if [ -z "${CF_DISTRO}" ]; then
    echo "!!! Second argument should be the id of the cloudfront distribution"
fi

echo "### Building and syncing ${SITE}"

echo " ## Building"
mdbook build

echo " ## Dryrun"
aws s3 sync book/html s3://${SITE} --dryrun

echo " ## Should we continue? ctrl-c to quit:"
echo -n "> Ok"
read VAR
echo "  # Ok"

echo " ## Syncing"
aws s3 sync book/html s3://${SITE}

echo " ## Invalidating CF cache"
# TODO: Diff and only invalidate changed things
aws cloudfront create-invalidation --distribution-id ${CF_DISTRO} --paths "/*"
