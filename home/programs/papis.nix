{ ... }:
{
  programs.papis = {
    enable = true;
    libraries = {
      full = {
        name = "full";
        isDefault = true;
        settings = {
          dir = "~/papis/full/";
        };
      };
    };
    settings = {
      bibtex = {
        "default-read-bibfile" = "~/papis/full.bib";
        "default-save-bibfile" = "~/papis/full.bib";
      };
    };
  };
}
