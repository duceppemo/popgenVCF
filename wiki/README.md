# Wiki source

This directory is the maintained source for the
[popgenVCF GitHub Wiki](https://github.com/duceppemo/popgenVCF/wiki).

- Edit pages here through the normal repository review process.
- Keep task-oriented guidance in the wiki and generated API documentation in
  pkgdown.
- Keep normative scientific contracts in `docs/`; wiki pages should link to
  those source documents instead of silently redefining them.
- Publish the Markdown pages with `scripts/publish-wiki.sh --push` after the
  corresponding repository change has been reviewed.

`README.md` is intentionally not published as a wiki page. All other Markdown
files in this directory are copied by the publishing script. The repository
logo is published with them as a Wiki-owned asset. The script updates managed
pages without deleting unrelated or historical wiki pages.
