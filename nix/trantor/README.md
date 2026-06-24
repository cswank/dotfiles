# trantor

NixOS configuration for the `trantor` workstation.

## Suspend debugging

If suspend misbehaves (won't suspend, hangs on resume, ghostty windows wedge after wake), the config includes hooks that capture state around every sleep cycle.

### History / working hypothesis

- 2026-05-01 (4ab505d): added `bluetooth-suspend` service to unload `btusb` before sleep — fixed a recurring suspend hang.
- ~7 weeks of clean suspends.
- 2026-06-22 (484d09a): re-enabled ghostty as the default terminal.
- 2026-06-24: first new suspend hang. Kernel got `PM: suspend entry (deep)` and nothing else — wedged in the device-suspend phase. `systemd-inhibit` showed no inhibitors held.

Current hypothesis: ghostty's EGL/GPU usage on `i915` triggers a driver suspend hang (no userspace inhibitor involved, which is why `systemd-inhibit --list` shows nothing). `pm_debug_messages` should name the offending driver next time.

### What's installed

- `boot.kernelParams = [ "no_console_suspend" "pm_debug_messages" ]`:
  - `no_console_suspend` keeps the kernel console alive across suspend so any kernel-side hang leaves a trail.
  - `pm_debug_messages` makes the kernel print `calling <driver>+` / `<driver> returned 0` for every device during the suspend transition. If suspend hangs in a driver `.suspend` callback, the last `calling ...+` line without a matching `returned` is the offender. Noisy but harmless.
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

If you only see `PM: suspend entry (deep)` and nothing else, the hang is in the device-suspend phase (a driver `.suspend` callback wedged) — `pm_debug_messages` will show which driver was last called.

If you catch it live (system woke but is misbehaving):

```
journalctl --since "10 min ago"
systemd-inhibit --list
```

### Confirming ghostty as the cause

While ghostty is open, `systemd-inhibit --list` will show any inhibitor it (or a child shell) is holding. If nothing shows up there but suspend still fails when ghostty is running, the issue is more likely the upstream EGL / GTK hang on exit rather than an inhibitor lock — see ghostty discussions [#4337](https://github.com/ghostty-org/ghostty/discussions/4337) and [#9190](https://github.com/ghostty-org/ghostty/discussions/9190).
