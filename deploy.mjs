name: Deploy to JATOS

on:
  push:
    branches:
      - main
    paths:
      - '**.html'

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repo
        uses: actions/checkout@v4

      - name: Upload HTML files to JATOS mindprobe
        env:
          JATOS_REMOTE_URL: ${{ secrets.JATOS_REMOTE_URL }}
          JATOS_REMOTE_TOKEN: ${{ secrets.JATOS_REMOTE_TOKEN }}
        run: |
          for file in *.html; do
            echo "📤 Uploading $file..."
            curl -s -o /dev/null -w "Status: %{http_code}\n" \
              -X PUT \
              -H "Authorization: Bearer $JATOS_REMOTE_TOKEN" \
              -H "Content-Type: text/html" \
              --data-binary "@$file" \
              "$JATOS_REMOTE_URL/jatos/api/v1/study/component/properties/studyAssets/$file"
          done