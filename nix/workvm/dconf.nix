# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      saved-pathbar-path = "/org/gnome/terminal/legacy/";
      saved-view = "/org/gnome/terminal/legacy/";
      show-warning = false;
      window-height = 994;
      window-is-maximized = false;
      window-width = 950;
    };

    "org/gnome/Geary" = {
      compose-as-html = true;
      formatting-toolbar-visible = false;
      migrated-config = true;
      window-height = 890;
      window-maximize = true;
      window-width = 1277;
    };

    "org/gnome/nm-applet/eap/0a4917fa-1ee7-4216-9961-f17eacdbc530" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/5809ec9c-f74e-4fad-b67d-c1af51af01c5" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/ab6f6f4c-18cc-41c1-b282-a49044253543" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/nm-applet/eap/c8507276-b3ab-4313-a69c-df3b751e235a" = {
      ignore-ca-cert = false;
      ignore-phase2-ca-cert = false;
    };

    "org/gnome/terminal/legacy" = {
      default-show-menubar = false;
      schema-version = 3;
      theme-variant = "default";
    };

    "org/gnome/terminal/legacy/profiles:" = {
      default = "53d98703-ad0c-4092-850e-3dbea0422cd8";
      list = [ "53d98703-ad0c-4092-850e-3dbea0422cd8" ];
    };

    "org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
      background-color = "rgb(12,12,12)";
      bold-is-bright = true;
      foreground-color = "rgb(170,170,170)";
      palette = [ "rgb(46,52,54)" "rgb(204,0,0)" "rgb(78,154,6)" "rgb(196,160,0)" "rgb(52,101,164)" "rgb(117,80,123)" "rgb(6,152,154)" "rgb(211,215,207)" "rgb(85,87,83)" "rgb(239,41,41)" "rgb(138,226,52)" "rgb(252,233,79)" "rgb(114,159,207)" "rgb(173,127,168)" "rgb(52,226,226)" "rgb(238,238,236)" ];
      scroll-on-output = false;
      scrollbar-policy = "never";
      use-theme-colors = false;
      visible-name = "Default";
    };

    "org/gnome/terminal/legacy/profiles:/:53d98703-ad0c-4092-850e-3dbea0422cd8" = {
      audible-bell = true;
      background-color = "rgb(10,10,10)";
      backspace-binding = "ascii-delete";
      bold-color-same-as-fg = true;
      cursor-blink-mode = "system";
      cursor-colors-set = false;
      cursor-shape = "block";
      delete-binding = "delete-sequence";
      font = "FiraCode Nerd Font Mono 11";
      foreground-color = "rgb(131,148,150)";
      highlight-colors-set = false;
      login-shell = false;
      palette = [ "rgb(7,54,66)" "rgb(220,50,47)" "rgb(133,153,0)" "rgb(181,137,0)" "rgb(38,139,210)" "rgb(211,54,130)" "rgb(42,161,152)" "rgb(238,232,213)" "rgb(0,43,54)" "rgb(203,75,22)" "rgb(88,110,117)" "rgb(101,123,131)" "rgb(131,148,150)" "rgb(108,113,196)" "rgb(147,161,161)" "rgb(253,246,227)" ];
      scrollback-lines = 10000;
      scrollbar-policy = "never";
      use-custom-command = false;
      use-system-font = false;
      use-theme-colors = false;
      visible-name = "peninsula";
    };

    "org/gnome/terminal/legacy/profiles:/:default" = {
      audible-bell = true;
      backspace-binding = "ascii-delete";
      cursor-blink-mode = "system";
      cursor-shape = "block";
      delete-binding = "delete-sequence";
      login-shell = false;
      scrollback-lines = 10000;
      scrollbar-policy = "always";
      use-custom-command = false;
      use-system-font = true;
      use-theme-colors = true;
      visible-name = "dark";
    };

    "org/gtk/settings/color-chooser" = {
      custom-colors = [ (mkTuple [ 4.666666666666667e-2 4.666666666666667e-2 ]) (mkTuple [ 9.000000000000001e-2 9.000000000000001e-2 ]) (mkTuple [ 0.24333333333333335 0.24333333333333335 ]) ];
      selected-color = mkTuple [ true 4.666666666666667e-2 ];
    };

    "org/gtk/settings/file-chooser" = {
      date-format = "regular";
      location-mode = "path-bar";
      show-hidden = false;
      show-size-column = true;
      show-type-column = true;
      sidebar-width = 412;
      sort-column = "name";
      sort-directories-first = true;
      sort-order = "ascending";
      type-format = "category";
      window-position = mkTuple [ 131 147 ];
      window-size = mkTuple [ 1658 785 ];
    };

    "org/onboard" = {
      schema-version = "2.3";
      system-theme-associations = "{'HighContrast': 'HighContrast', 'HighContrastInverse': 'HighContrastInverse', 'LowContrast': 'LowContrast', 'ContrastHighInverse': 'HighContrastInverse', 'Default': ''}";
      use-system-defaults = false;
    };

  };
}
