{ ... }:
{
  programs.papis = {
    enable = true;
    libraries = {
      full = {
        name = "full";
        isDefault = true;
        dir = "~/papis/default/"
      };
    };
  };
}
