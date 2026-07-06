.DEFAULT_GOAL := help

.PHONY: help check-main check-release-branch release release-patch release-minor release-major release-dev finish-release

help:
	@echo "Release targets:"
	@echo "  make release        # interactively choose version bump"
	@echo "  make release-patch  # bump patch version"
	@echo "  make release-minor  # bump minor version"
	@echo "  make release-major  # bump major version"
	@echo "  make release-dev    # bump to development version"

check-main:
	@test "$$(git branch --show-current)" = "main" || \
		(echo "Release must be run from main."; exit 1)
	@git fetch origin main
	@test "$$(git rev-parse HEAD)" = "$$(git rev-parse origin/main)" || \
		(echo "Local main is not equal to origin/main."; exit 1)

check-release-branch: check-main
	@test -z "$$(git status --porcelain)" || \
		(echo "Working tree is not clean."; exit 1)

release: check-release-branch
	Rscript -e 'usethis::use_version()'
	$(MAKE) finish-release

release-patch: check-release-branch
	Rscript -e 'usethis::use_version("patch")'
	$(MAKE) finish-release

release-minor: check-release-branch
	Rscript -e 'usethis::use_version("minor")'
	$(MAKE) finish-release

release-major: check-release-branch
	Rscript -e 'usethis::use_version("major")'
	$(MAKE) finish-release

release-dev: check-release-branch
	Rscript -e 'usethis::use_version("dev")'
	$(MAKE) finish-release

finish-release: check-main
	Rscript -e 'desc::desc_set("Date", format(Sys.Date()))'
	git diff
	@VERSION=$$(Rscript -e 'cat(as.character(desc::desc_get_version()))'); \
	TAG="v$$VERSION"; \
	if git rev-parse "$$TAG" >/dev/null 2>&1; then \
		echo "Tag $$TAG already exists."; \
		exit 1; \
	fi; \
	printf "\nAbout to commit release $$TAG. Press Enter to continue, Ctrl-C to abort."; \
	read ans; \
	git add DESCRIPTION NEWS.md; \
	git commit -m "Prepare release $$TAG"; \
	git tag "$$TAG"; \
	git push origin main; \
	git push origin "$$TAG"
