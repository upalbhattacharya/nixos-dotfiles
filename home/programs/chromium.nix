{ ... }:
{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "bkkmolkhemgaeaeggcmfbghljjjoofoh"; } # Catppuccin Mocha
      { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; } # Dark Reader
      { id = "ekhagklcjbdpajgpjgmbionohlpdbjgc"; } # Zotero Connector
      { id = "hfjbmagddngcpeloejdejnfgbamkjaeg"; } # Vimium C
      { id = "ddkjiahejlhfcafbddmgiahcphecmpfh"; } # uBlock Origin Lite
    ];
  };
}
