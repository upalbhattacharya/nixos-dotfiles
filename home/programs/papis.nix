{ ... }:
{
  programs.papis = {
    enable = true;
    libraries = {
      full = {
        name = "full";
        isDefault = true;
        settings = {
          dir = "~/papis/default/";
        };
      };
    };
  };
}
