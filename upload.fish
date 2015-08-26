#! /usr/local/bin/fish
cp -R _site site-dist
cd site-dist
pusher -vdb zyghost.com (find .)
cd ..
rm -rf site-dist
