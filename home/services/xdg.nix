{ ... }:
{
  programs.xdg.mime = {
    enable = true;
    defaultApplications = {
      "application/pdf" = "sioyek.desktop";
    };
  };
}
