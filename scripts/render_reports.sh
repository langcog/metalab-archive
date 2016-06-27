#!/bin/bash

cd /home/ubuntu/metalab/
rm -rf reports/out || exit 0;
mkdir reports/out;

R CMD BATCH scripts/render_reports.R;

mv reports/out ..
git checkout gh-pages
cp -r ../out/* reports
rm -r ../out
git add .
git commit -m "deployed to github pages"
git push --force --quiet origin gh-pages
git checkout master
