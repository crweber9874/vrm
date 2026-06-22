# vrm site

Quarto landing page (`index.qmd`) plus a documentation **book** (`book/`).
The book renders into `_site/book/`, so the navbar/link `book/index.html`
resolves from the published site.

## Build

```sh
# render the landing site first (it cleans _site), then the book into _site/book
quarto render site
quarto render site/book
```

The full site lands in `site/_site/`. Open `site/_site/index.html`.

To publish to GitHub Pages: `quarto publish gh-pages site` (after rendering),
or point Pages at the `_site/` output.
