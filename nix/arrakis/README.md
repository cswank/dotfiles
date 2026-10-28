# arrakis

NixOS 25.05 server. `/etc/nixos/configuration.nix` is a symlink to
`~/Projects/dotfiles/nix/arrakis/configuration.nix` **in arrakis's own clone**
of this repo, so changes only take effect once they're in that working copy.

## Music storage for aurora (SMB share)

Goal: move `aurora.local:~/Music` (Intel MacBook, low on disk) onto arrakis's
`/mnt/music` and use it from Logic and Transcribe.

### History

- 2026-10-17: added `services.samba` + an Avahi `_smb._tcp` service to
  `configuration.nix` on arrakis, ran `nixos-rebuild switch` and
  `smbpasswd -a craig`. smbd confirmed listening on 445.
- 2026-10-17: found arrakis's link stuck at **100 Mbps**. The NIC (Realtek
  RTL8125, 2.5 GbE) is fine and a brand-new cable didn't help; the culprit is
  the **TRENDnet TE100** switch it's plugged into (10/100 only). trantor
  (gigabit, not on that switch) measured only 10.9 MB/s to arrakis.
  A gigabit switch has been ordered.
- 2026-10-23: gigabit switch (**TRENDnet TEG-S50g**, 5-port, unmanaged)
  installed. arrakis `enp1s0` now links at 1000 Mb/s full duplex, and
  `ethtool` shows the switch port advertising `1000baseT/Full`. trantor (then
  plugged straight into the router) -> arrakis was **still ~11.5 MB/s**. The
  cause turned out to be the router's ether4 port (see the uplink entry below).
- 2026-10-23: aurora put on Ethernet (`en0`, 1000baseT full duplex, used for
  the route to arrakis). Raw TCP with netcat, 500 MB arrakis -> aurora:
  **~50-60 MB/s** (arrakis's `dd` reported 59.6 MB/s, aurora's 49.7 MB/s), so
  the aurora <-> arrakis path is no longer limited to 100 Mbps. The opposite
  direction wasn't tested because arrakis's firewall blocks the netcat port.
  Neither machine has an ssh key the other accepts.
- 2026-10-23: copied `~/Music/Logic` (11 GB) from aurora to the share with
  `ditto ~/Music/Logic /Volumes/music/Logic` (the system `rsync` on macOS 15 is
  openrsync, which doesn't copy extended attributes), and watched it with
  `netstat -I en0 -w 1`. Sustained **~100-110 MB/s** out of aurora over SMB,
  close to gigabit line rate, with zero errors. It dropped to ~42-44 MB/s on
  stretches that were probably many small files (per-file SMB round trips).
  That's about twice the netcat result, which ran in the other direction as a
  single raw stream and probably hit a limit on aurora's receiving end.
- 2026-10-23: found and fixed the 100 Mbps uplink. The router is a MikroTik
  (RouterOS, 192.168.88.1). `/interface ethernet monitor [find] once` showed
  ether1-3 at 1 Gbps but **ether4 at 100 Mbps**, with the link partner
  advertising only up to 100M. `/interface bridge host print where
  interface=ether4` listed arrakis's MAC (and aurora, a Raspberry Pi, a Sony
  device), so the TEG-S50g was on ether4. Replacing the router <-> switch
  cable didn't help (still 100M). With trantor moved onto the TEG-S50g:
  trantor -> arrakis **104 MB/s**, but internet dropped from ~78 MB/s
  (~620 Mbps) to ~11 MB/s, which isolated the problem to the switch uplink.
  Moving the uplink to **ether5** fixed it: internet ~78 MB/s down,
  ~60-80 MB/s up, from trantor through the TEG-S50g. The new cable was used on
  both ports, so the **router's ether4 port is probably faulty**. Don't use it
  for anything that needs gigabit.
  The 2026-10-17 slowness (trantor -> arrakis 10.9 MB/s) may also have been
  partly ether4, not just the TE100.
- The work happened in a Claude Code session in `~/Projects/demucs-rs` on
  trantor (`cd ~/Projects/demucs-rs && claude --resume` to find it).

### Share config (in configuration.nix)

- Share `music` -> `/mnt/music` (ext4, 229 GB), writable, `valid users = craig`.
- `server min protocol = SMB3`; `hosts allow` limited to 192.168.88.0/24,
  the LAN's IPv6 ULA `fdf9:1a93:5ec1:425b::/64`, link-local and localhost.
- `vfs objects = catia fruit streams_xattr` + `fruit:*` options so Finder
  metadata, resource forks and `.logicx` packages survive the round trip.
- `openFirewall = true` (445/139 TCP, 137/138 UDP).
- Samba keeps its own password DB: `sudo smbpasswd -a craig` to (re)set it.

### When the new switch arrives

1. Swap the TE100 for the gigabit switch, then check the link:
   `cat /sys/class/net/enp1s0/speed` should print `1000` (not `100`).
   `ethtool enp1s0` (via `nix shell nixpkgs#ethtool`) shows what the switch
   port advertises under "Link partner advertised link modes".
2. Re-test throughput from trantor:
   `dd if=/dev/zero bs=1M count=200 | ssh arrakis.local 'cat > /dev/null'`
   (100 Mbps ≈ 11 MB/s; gigabit should be ~100 MB/s minus ssh overhead).
3. On aurora: Finder -> Cmd-K -> `smb://arrakis.local/music`, save the
   password in the keychain, and add the mounted share to
   System Settings -> General -> Login Items to remount at login.
4. Copy a large file over SMB from aurora to see real-world speed. aurora is
   now on Ethernet, and a `ditto` copy to the share ran at ~100-110 MB/s
   (see History). (On Wi-Fi only, 5 GHz 802.11ac at ~405 Mbps, expect
   roughly 20-35 MB/s.)
5. Move data with rsync, dry run first, and verify projects open in Logic
   before deleting anything on aurora.

### aurora ~/Music (as of 2026-10-17, 17.8 GB free on aurora)

| Folder | Size |
|---|---|
| `Logic` | 11 GB |
| `Music` (Apple Music library) | 946 MB |
| `Transcribe` | 337 MB |
| `GarageBand` | 80 MB |
| everything else | < 20 MB |

### Plan / guidance

- **Transcribe**: fine to run straight from the share (it loads the whole
  file into memory).
- **Logic**: use the share as an archive. Keep active projects local on
  aurora; move finished projects, old ideas and bounces to arrakis and copy a
  project back when working on it again. Recording or mixing large sessions
  directly over Wi-Fi SMB risks "Disk is too slow" errors and dropouts. That
  becomes realistic only with aurora on Ethernet. Apple doesn't officially
  support Logic projects on network volumes.

### Loose ends

- The Samba config is committed and pushed (`33daabb smb for arrakis`).
  arrakis itself can't reach GitHub over ssh (`Permission denied (publickey)`),
  so other clones (e.g. trantor's) need a `git pull` to see it.
- aurora's SSH key couldn't be used for non-interactive ssh to arrakis
  (probably a passphrase-protected key without an agent in that session).
