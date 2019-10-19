#!/usr/bin/env bash

echo "Syncing images and fonts..."
s3cmd sync \
  --acl-public \
  --no-preserve \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=31557600" \
  --include "*.png" \
  --include "*.jpg" \
  --include "*.gif" \
  --include "*.ico" \
  --include "*.eot" \
  --include "*.svg" \
  --include "*.ttf" \
  --include "*.woff" \
  --include "*.woff2" \
  --include "*.otf" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing html..."
s3cmd put \
  --acl-public \
  --no-preserve \
  --recursive \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=86400, must-revalidate" \
  --add-header="Expires:access plus 1 day"  \
  --mime-type="text/html; charset=utf-8" \
  --include "*.html" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing css..."
s3cmd put \
  --acl-public \
  --no-preserve \
  --recursive \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=604800" \
  --mime-type="text/css" \
  --include "*.css" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing js..."
s3cmd sync \
  --acl-public \
  --no-preserve \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=604800" \
  --mime-type="application/javascript" \
  --include "*.js" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing json..."
s3cmd sync \
  --acl-public \
  --no-preserve \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=86400, must-revalidate" \
  --mime-type="application/json" \
  --include "*.json" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing xml..."
s3cmd sync \
  --acl-public \
  --no-preserve \
  --exclude "*.*" \
  --add-header="Cache-Control:public, max-age=2629800" \
  --mime-type="application/xml" \
  --include "*.xml" \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com

echo "Syncing remaining and cleaning up..."
s3cmd sync \
  --acl-public \
  --no-preserve \
  --delete-removed \
  -c .s3cmd \
  dist/ s3://redexer.maxhallinan.com
