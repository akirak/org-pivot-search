{
  outputs = {...}: {
    elisp-rice = {
      packages = [
        "org-pivot-search"
      ];
      # TODO: Configure tests
      tests = {};
    };
  };
}
