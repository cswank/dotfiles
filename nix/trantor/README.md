# trantor

NixOS configuration for the `trantor` workstation.

## Rebuilding

trantor is built from `flake.nix` in this directory. home-manager runs as a NixOS module, so one command applies both `configuration.nix` and `home.nix` (there is no standalone `home-manager` CLI):

```
nixos-rebuild switch --flake ~/Projects/dotfiles/nix/trantor#trantor --use-remote-sudo
```

`--use-remote-sudo` evaluates as your user and only uses sudo to activate. Running the whole thing under sudo makes git refuse the repo (owned by craig, not root).

- Flakes only see files git tracks. `git add` a new file before rebuilding or the build won't find it. Git submodules (e.g. `files/emacs/plugins/vue-ts-mode`) are excluded too.
- Versions are pinned in `flake.lock`. Update everything with `nix flake update`, or a single input with `nix flake lock --update-input nixpkgs-unstable`, then rebuild and commit the lock file.
- `/etc/nixos/*` still symlinks here, but plain `nixos-rebuild` (no `--flake`) no longer works because `configuration.nix` takes `inputs` from the flake.

## Suspend debugging

If suspend misbehaves (won't suspend, hangs on resume, ghostty windows wedge after wake), the config includes hooks that capture state around every sleep cycle.

### History / working hypothesis

- 2026-05-01 (4ab505d): added `bluetooth-suspend` service to unload `btusb` before sleep — fixed a recurring suspend hang.
- ~7 weeks of clean suspends.
- 2026-06-22 (484d09a): re-enabled ghostty as the default terminal.
- 2026-06-24: first new suspend hang. Kernel got `PM: suspend entry (deep)` and nothing else — wedged in the device-suspend phase. `systemd-inhibit` showed no inhibitors held.
- 2026-06-25: hang recurred. `pm_debug_messages` had been added but turned out to be the wrong knob — it gates higher-level PM lifecycle pr_dbg messages, not per-device `calling X+` traces.
- 2026-06-26: confirmed `/sys/power/pm_print_times = 1` is the correct knob (344 per-device "calling" lines on a test cycle). Switched to a `systemd.tmpfiles` rule to enable it at boot.
- 2026-07-02 to 07-03: hang recurred on ghostty and again after switching to kitty (both GPU-accelerated OpenGL terminals). Hypothesis narrowed: it's the GL-terminal-class + i915 + this specific hardware, not ghostty-specific.
- 2026-07-03: switched trantor to gnome-terminal via `i3-sensible-terminal` (VTE + cairo, no OpenGL). Kitty/ghostty still installed for easy swap-back. If suspends stay clean on gnome-terminal, hypothesis is confirmed and next step is either an i915 workaround or trying a newer kernel.
- 2026-07-06 to 10-19: 194 clean suspends on gnome-terminal, with `LIBVA_DRIVER_NAME=i965` (which fails to initialise on this chip, so Firefox has no VA-API).
- 2026-10-19 (8244efc): `LIBVA_DRIVER_NAME=iHD` gave Firefox working VA-API; the next two suspends hung. Reverted to `i965`, which is deliberately kept as a "no VA-API" switch.
- 2026-10-20: added xscreensaver (non-GL hacks, lock + fade). The first suspend after that hung, right as the lock fade was drawing (the lock binding runs `xscreensaver-command -lock && sleep 1 && systemctl suspend`). Removed it the next day and went back to xss-lock + i3lock.

Current hypothesis: ghostty's EGL/GPU usage on `i915` triggers a driver suspend hang (no userspace inhibitor involved, which is why `systemd-inhibit --list` shows nothing). `pm_print_times` should name the offending driver on the next hang.

### What's installed

- `boot.kernelParams = [ "no_console_suspend" ]` — keeps the kernel console alive across suspend so any kernel-side hang leaves a trail.
- `systemd.tmpfiles.rules = [ "w /sys/power/pm_print_times - - - - 1" ]` — at boot, writes `1` to `/sys/power/pm_print_times`. This makes the kernel print `calling <driver>+ @ <pid>` and the matching `call <driver>+ returned 0 after <usecs>` for every device during suspend and resume. If suspend hangs in a driver `.suspend` callback, the last `calling ...+` line without a matching `returned` is the offender. Noisy (hundreds of lines per cycle) but harmless. Do **not** confuse with `pm_debug_messages` — that's a different (and less useful for this) knob.
- `systemd.services.suspend-debug` — runs `before = sleep.target`. Logs:
  - `systemd-inhibit --list` (who is holding sleep/idle locks)
  - all `ghostty` and `<defunct>` processes
  - `loginctl list-sessions`
- `systemd.services.resume-debug` — runs `after = suspend.target / hibernate.target`. Logs ghostty processes post-wake.

### Checking the logs

After a bad suspend, on the next boot:

```
journalctl -b -1 -u suspend-debug -u resume-debug
journalctl -b -1 -u systemd-suspend.service
journalctl -b -1 -k --grep 'suspend\|resume\|PM:'
```

A healthy deep suspend looks like:

```
PM: suspend entry (deep)
ACPI: PM: Preparing to enter system sleep state S3
ACPI: PM: Saving platform NVS memory
ACPI: PM: Low-level resume complete
ACPI: PM: Waking up from system sleep state S3
PM: suspend exit
```

If you only see `PM: suspend entry (deep)` and nothing else, the hang is in the device-suspend phase (a driver `.suspend` callback wedged). To find the offender:

```
journalctl -b -1 -k --no-pager | grep -E 'calling .* @|call .* returned' > /tmp/pm.log
# last 'calling X+' line without a matching 'call X+ returned' = the hung driver
tail -20 /tmp/pm.log
```

If you catch it live (system woke but is misbehaving):

```
journalctl --since "10 min ago"
systemd-inhibit --list
```

### Confirming ghostty as the cause

While ghostty is open, `systemd-inhibit --list` will show any inhibitor it (or a child shell) is holding. If nothing shows up there but suspend still fails when ghostty is running, the issue is more likely the upstream EGL / GTK hang on exit rather than an inhibitor lock — see ghostty discussions [#4337](https://github.com/ghostty-org/ghostty/discussions/4337) and [#9190](https://github.com/ghostty-org/ghostty/discussions/9190).
