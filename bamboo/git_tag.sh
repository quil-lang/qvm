#!/bin/bash
set -ex
###############################################################################
# Bamboo script for using git to tag a commit and push the tag to the remote
###############################################################################
git clone ssh://git@bitbucket.lab.rigetti.com:7999/qcs/qvm.git
cd qvm

if [ -n "$bamboo_GIT_COMMIT_TAG" ]; then
    git tag $bamboo_GIT_COMMIT_TAG
    git push origin $bamboo_GIT_COMMIT_TAG
else
    echo "Commit tag variable is empty"
fi

cd ..
rm -rf qvm
