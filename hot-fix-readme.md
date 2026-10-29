# Hot fix: MariaDB error 1040 "Too many connections"

Visitors to the ithamaps pages on the ITHANET site sometimes get
`SQLSTATE[HY000] [1040] Too many connections`. This file records what we know
so far, the likely causes, how to check each one, and how to fix it.

MariaDB returns error 1040 when the whole server already has
`max_connections` connections open. The limit is shared by every client of
that MariaDB server: the Joomla site, all the ITHANET modules, the ithamaps
Shiny app, phpMyAdmin, cron jobs and backups. So the error page a user sees
doesn't tell you which client used up the connections.

---

## 1. What the first check showed (2026-10-29)

| Variable | Value | What it means |
|---|---|---|
| `max_connections` | 151 | MariaDB's default limit. |
| `Max_used_connections` | 152 | The limit was **fully reached** since the server last started. MariaDB keeps 1 extra connection free for an admin, so 151 + 1 = 152 is the highest possible value. |
| `Max_used_connections_time` | 2026-10-22 09:21:30 | The **first** time the peak was reached, in the server's time zone. The timestamp only changes when a new, higher peak is set, and 152 can't be exceeded. So later incidents, like the one users reported on **2026-10-27**, don't show up here. **Look up both dates in the logs.** |
| `Threads_connected` | 2 | Right now the server is almost idle. |
| `Aborted_connects` | 0 | No failed logins or handshakes. Connections refused because of the limit are **not** counted here; see `Connection_errors_max_connections` below. |
| `wait_timeout` | 28800 | Idle connections stay open for up to 8 hours. That's too long for a web server. |
| process list | 1 phpMyAdmin (Sleep), 1 admin query, the event scheduler | Nothing is holding connections open. |

The event scheduler had been running for 599,641 s (≈ 6 days 22.5 h). That
roughly matches when MariaDB was last started, which was around 2026-10-22.

### Conclusion so far

- **It's a short burst, not a leak.** If connections were leaking, or PHP were
  using persistent connections, we'd see dozens of `Sleep` rows sitting there.
  We see 2 connections. Something opened about 150 connections at the same
  time, around 2026-10-22 09:21:30, and they were then released.
- Bursts like this usually come from a traffic spike (often a bot or crawler)
  that runs into slow pages or slow queries. Each PHP request holds 2 or more
  connections until the page finishes, so about 60–80 requests at the same
  time are enough.
- **Possibility to rule out:** if MariaDB restarted *just before* 09:21:30
  (check `Uptime`, step 2.1), the peak may be a **reconnect storm**: every
  web worker that was waiting reconnected the moment the database came back.
  In that case, also find out *why* MariaDB restarted (OOM killer, crash,
  manual restart, update).
- The Shiny app is very unlikely to be the cause. It opens 2 connections once,
  when the R process starts ([global.R](global.R) lines 258 and 366), and
  closes them straight away. It connects through `host.docker.internal`, so
  it shows up in the process list as the Docker gateway IP (e.g.
  `172.17.0.1`), not as `localhost`.

---

## 2. Where to look

### 2.1 More MariaDB numbers (run as a DB admin)

```sql
SHOW GLOBAL STATUS LIKE 'Uptime';                            -- seconds since MariaDB started
SELECT NOW() - INTERVAL VARIABLE_VALUE SECOND AS started_at
  FROM information_schema.GLOBAL_STATUS WHERE VARIABLE_NAME = 'UPTIME';
SHOW GLOBAL STATUS LIKE 'Connection_errors_max_connections'; -- how many connections were REFUSED (= error 1040s)
SHOW GLOBAL STATUS LIKE 'Connections';                       -- total connections since start
SHOW GLOBAL STATUS LIKE 'Slow_queries';
SHOW VARIABLES LIKE 'slow_query_log%';
SHOW VARIABLES LIKE 'long_query_time';
SELECT @@log_error;                                          -- path of the MariaDB error log
SELECT @@system_time_zone, @@time_zone;                      -- so log times can be lined up
```

- If `started_at` is a few seconds or minutes before 2026-10-22 09:21:30, it's
  probably a reconnect storm (cause 5).
- `Connection_errors_max_connections` tells you how many requests actually got
  the error. Write it down and compare it on a later day.

### 2.2 Web server access log

Find it on the web server (the machine that runs Joomla):

| Setup | Usual location |
|---|---|
| Apache, Debian/Ubuntu | `/var/log/apache2/access.log`, `/var/log/apache2/other_vhosts_access.log`, or a per-site file in `/var/log/apache2/` |
| Apache, RHEL/CentOS/Rocky | `/var/log/httpd/access_log` (+ `ssl_access_log`) |
| nginx | `/var/log/nginx/access.log` (or per-site files in `/var/log/nginx/`) |
| Plesk | `/var/www/vhosts/system/<domain>/logs/access_log` and `access_ssl_log` |
| cPanel | `/usr/local/apache/domlogs/<domain>` or `~/access-logs/` |

If you're not sure, ask the web server where it writes the log:

```bash
sudo apachectl -S                                    # lists vhosts and their config files
sudo grep -Ri "CustomLog" /etc/apache2/ /etc/httpd/ 2>/dev/null
sudo nginx -T 2>/dev/null | grep -i access_log
ls -lt /var/log/apache2/ /var/log/httpd/ /var/log/nginx/ 2>/dev/null
```

Older logs are rotated and compressed (`access.log.2.gz`, etc.). Use `zgrep`
for those. 2026-10-22 is about a week old, so it's probably in `.1` or
`.2.gz`.

`zgrep` reads both plain and `.gz` files, so one command covers the current
log and the rotated ones. Apache and nginx write dates as
`[27/Oct/2026:14:05:12 +0200]`, so you search for that text. To see which
date range each file covers:

```bash
for f in /var/log/apache2/access.log*; do
  printf '%s  ' "$f"; zcat -f "$f" | head -1 | awk '{printf "%s ... ", $4}'
  zcat -f "$f" | tail -1 | awk '{print $4}'
done
```

Patterns for a date (`zgrep -c` counts the matching lines, plain `zgrep` prints them):

```bash
zgrep -c "27/Oct/2026"            /var/log/apache2/access.log*   # whole day, per file
zgrep -h "27/Oct/2026:14:"        /var/log/apache2/access.log*   # 14:00-14:59
zgrep -h "27/Oct/2026:14:[0-2]"   /var/log/apache2/access.log*   # 14:00-14:29
zgrep -h "27/Oct/2026:1[0-2]:"    /var/log/apache2/access.log*   # 10:00-12:59
```

To find the minutes on 27 Oct with the most requests (the burst should stand out):

```bash
zgrep -h "27/Oct/2026" /var/log/apache2/access.log* \
  | awk '{print substr($4,2,17)}' | sort | uniq -c | sort -rn | head -20
```

What to look for around the peak (adjust the file name):

```bash
LOG=/var/log/apache2/access.log.1       # or the .gz file, using zgrep instead of grep

# Requests per minute from 09:10 to 09:30. Look for a spike.
zgrep -h "22/Oct/2026:09:[123]" $LOG* | awk '{print substr($4,2,17)}' | sort | uniq -c

# Top IP addresses in the minutes around the peak
zgrep -h "22/Oct/2026:09:2[01]" $LOG* | awk '{print $1}' | sort | uniq -c | sort -rn | head -20

# Top user agents (bots identify themselves here)
zgrep -h "22/Oct/2026:09:2[01]" $LOG* | awk -F'"' '{print $6}' | sort | uniq -c | sort -rn | head -20

# Top URLs (which pages or modules were hit)
zgrep -h "22/Oct/2026:09:2[01]" $LOG* | awk '{print $7}' | cut -d'?' -f1 | sort | uniq -c | sort -rn | head -20

# Responses with errors (5xx) around then
zgrep -h "22/Oct/2026:09:2" $LOG* | awk '$9 ~ /^5/' | head
```

### 2.3 Other logs worth checking at the same time

| Log | Command / location | Why |
|---|---|---|
| MariaDB error log | path from `SELECT @@log_error`, or `journalctl -u mariadb --since "2026-10-22 09:00" --until "2026-10-22 09:40"` | Restarts, crashes, "Too many connections" warnings |
| Kernel / OOM killer | `journalctl -k --since "2026-10-22 09:00" --until "2026-10-22 09:40"`, or `grep -i "killed process" /var/log/syslog /var/log/messages` | Was MariaDB or PHP killed for lack of memory? |
| PHP-FPM log | `/var/log/php*-fpm.log`, or `journalctl -u php*-fpm` | `server reached pm.max_children` warnings |
| Apache error log | `/var/log/apache2/error.log` or `/var/log/httpd/error_log` | `MaxRequestWorkers` reached, PHP fatal errors |
| Joomla log | `administrator/logs/` in the Joomla root (`error.php`, `everything.php`) | Database errors as Joomla saw them |
| Shiny container | `docker compose logs --since 2026-10-22T09:00:00 --until 2026-10-22T09:40:00 ithamaps-shiny` (from this folder) | Did the app restart or fail to connect? |
| Cron jobs | `crontab -l`, `sudo ls /etc/cron.*`, `grep CRON /var/log/syslog` | A backup or import running at 09:21? |

---

## 3. Possible causes: how to check, how to fix

### Cause 1: traffic spike or bots (most likely)

Crawlers follow every search and filter link (ithamaps, ithagenes, hep
classification and so on). Each request opens a Joomla connection plus an
`ithabase` connection
([mod_ithamaps2.php](../ithanet-repos/modules/mod_ithamaps2/mod_ithamaps2.php)
lines 49–50, and the same pattern in the other modules).

- **Check:** in the access log (2.2), look for a jump in requests per minute
  around 09:21, one IP or a few IPs with hundreds of requests, or bot user
  agents (`bot`, `crawler`, `spider`, `GPTBot`, `ClaudeBot`, `Bytespider`,
  `AhrefsBot`, `SemrushBot`, `python-requests`, `curl`).
- **Fix:**
  - `robots.txt`: `Disallow:` the search and result URLs and add
    `Crawl-delay: 10`. Well-behaved bots follow it.
  - Rate-limit per IP: Apache `mod_evasive` / `mod_qos`, nginx
    `limit_req_zone`, or fail2ban on the access log. Bad bots ignore
    `robots.txt`, so you need this too.
  - Block clearly abusive IPs or user agents.

#### If the IP in the log belongs to Cloudflare

Cloudflare's addresses include `162.158.0.0/15`, `172.64.0.0/13`,
`104.16.0.0/13` and `173.245.48.0/20`; the full list is at
https://www.cloudflare.com/ips/. An address like this in the access log is a
Cloudflare server passing the request on, not the real visitor. **Don't block
it on the server**, because real visitors come through the same Cloudflare
servers.

1. **Is the site behind Cloudflare?** On a laptop, run
   `dig +short <site domain>`. If it returns Cloudflare addresses, the site is
   proxied through Cloudflare.
   - **Yes:** every visitor appears as a Cloudflare IP. Make the logs show the
     real IP: Apache `mod_remoteip` with
     `RemoteIPHeader CF-Connecting-IP` and a `RemoteIPTrustedProxy` line for
     each Cloudflare range, and use `%a` instead of `%h` in the `LogFormat`.
     Then block the bot **in the Cloudflare dashboard** (see below).
   - **No:** the requests come from code running on Cloudflare, usually a
     Cloudflare Worker used as a scraper, or Cloudflare's WARP VPN. Workers
     add a `CF-Worker: <their domain>` header to their requests. Block by user
     agent or header rather than by IP, because the IP will change.
2. **Block in Cloudflare** (Security → WAF in the dashboard):
   - Custom rule: *User Agent contains `MSIE 7.0`* → **Block** (or a Managed
     Challenge).
   - Rate limiting rule on the search URLs, e.g. more than 30 requests in
     10 s from one visitor → Block for 10 min.
   - Turn on **Bot Fight Mode** (Security → Bots).
3. **Make the server accept only Cloudflare.** If the server still answers
   requests from any address, bots can go round Cloudflare. Allow ports
   80/443 only from Cloudflare's ranges in the firewall.

### Cause 2: slow queries keep connections open longer

If one page takes 5 s instead of 0.2 s, 25 times more connections are open at
the same moment for the same traffic.

- **Check:**
  ```sql
  SET GLOBAL slow_query_log = 1;
  SET GLOBAL long_query_time = 1;        -- log queries slower than 1 s
  SELECT @@slow_query_log_file;
  ```
  After a day, read the slow log (`mysqldumpslow -s t <file> | head -30`).
  During a busy period, run `SHOW FULL PROCESSLIST;` and look for many rows in
  `Query` or `Waiting for table metadata lock` / `Locked` states.
- **Fix:** add indexes for the slow queries (use `EXPLAIN <query>` to see
  them), cache results that don't change often, and avoid `SELECT *` on large
  tables in module helpers.

### Cause 3: the web server can run more workers than the database allows

If Apache or PHP-FPM can run 200 PHP processes and each can hold 2 connections,
up to 400 connections can be requested against a limit of 151.

- **Check:**
  ```bash
  grep -R "pm.max_children" /etc/php/*/fpm/pool.d/ 2>/dev/null   # PHP-FPM
  grep -Ri "MaxRequestWorkers\|MaxClients" /etc/apache2/ /etc/httpd/ 2>/dev/null   # Apache mod_php
  ```
  Also check the PHP-FPM log for `reached pm.max_children`.
- **Fix:** keep
  `max_children × connections per request (≈2–3) + other clients + headroom ≤ max_connections`.
  For example, with `max_connections = 300`, keep `pm.max_children` at around
  80–100. Extra requests then wait in the web server's queue instead of failing.

### Cause 4: idle or persistent connections

Not supported by the current numbers (only 2 connections, no long `Sleep`
rows), but worth confirming because `wait_timeout` is 8 hours.

- **Check:**
  ```sql
  SELECT user, SUBSTRING_INDEX(host, ':', 1) AS client, command, COUNT(*) AS n, MAX(time) AS max_secs
  FROM information_schema.PROCESSLIST GROUP BY 1, 2, 3 ORDER BY n DESC;
  ```
  Run it at a busy time. Many `Sleep` rows with large `max_secs` means
  connections are being kept open.
  Also check that `mysqli.allow_persistent` / `pdo_mysql` persistent options
  aren't enabled for the site (`php -i | grep -i persistent`), and that no
  custom PHP uses `mysqli_pconnect` or `PDO::ATTR_PERSISTENT`. Check the
  ITHANET code with
  `grep -rn "pconnect\|ATTR_PERSISTENT" <joomla root>`. Don't print the config
  files themselves, because they contain credentials.
- **Fix:** lower `wait_timeout` and `interactive_timeout` to 60–300 s (step 4)
  and remove any persistent connections.

### Cause 5: reconnect storm after a MariaDB restart

- **Check:** compare `started_at` from 2.1 with the peak time. Look in the
  MariaDB error log and `journalctl -u mariadb` for a shutdown or start around
  09:2x, and in the kernel log for the OOM killer.
- **Fix:** find out why it restarted. If it was the OOM killer, reduce
  `innodb_buffer_pool_size` or per-connection buffers, or add RAM. If it was an
  automatic update or a manual restart, schedule those for quiet hours. Capping
  web workers (cause 3) also limits how big the storm can get.

### Cause 6: another client on the same MariaDB server

Backups (`mysqldump` locks tables, which slows everything down; cause 2),
import or cron scripts, other sites, or a script that opens a connection inside
a loop.

- **Check:** the process list grouped by `user` and `client` during a busy
  time; cron schedules and logs for jobs running at about 09:20; the Shiny
  container logs.
- **Fix:** move heavy jobs to quiet hours, use `mysqldump --single-transaction`
  for InnoDB tables, and give each client its own DB user so the process list
  shows who is who. You can also cap each user:
  `ALTER USER '<user>'@'<host>' WITH MAX_USER_CONNECTIONS 50;`

### Cause 7 (minor): Shiny startup leaving a connection open

If [global.R](global.R) fails between `dbConnect` and `dbDisconnect`, the
connection stays open until the container restarts. With
`restart: unless-stopped`, a crash loop opens one new connection per attempt.

- **Check:** `docker compose ps` (does the restart count keep going up?) and
  `docker compose logs ithamaps-shiny`.
- **Fix:** close the connection even when an error happens, e.g.
  `on.exit(dbDisconnect(con), add = TRUE)` inside a function, or
  `tryCatch(..., finally = dbDisconnect(con))`.

---

## 4. Immediate hot fix (safe to apply now)

These steps buy headroom while the root cause is investigated. Apply them on
the dev server first.

1. **Raise the limit and drop idle connections sooner** (takes effect at once,
   but is lost on restart):
   ```sql
   SET GLOBAL max_connections = 300;
   SET GLOBAL wait_timeout = 120;
   SET GLOBAL interactive_timeout = 120;
   ```
   To keep it after a restart, add this to the MariaDB config
   (`/etc/mysql/mariadb.conf.d/50-server.cnf` or `/etc/my.cnf.d/server.cnf`,
   under `[mysqld]`):
   ```ini
   max_connections     = 300
   wait_timeout        = 120
   interactive_timeout = 120
   ```
   Check the server has enough memory first: `free -h`, and keep in mind that
   each connection can use several MB under load.
2. **Turn on the slow query log** (cause 2) so the next incident leaves
   evidence.
3. **Add a crawl-delay and rate limiting** for the search pages (cause 1).
4. **Cap PHP workers** to match the new `max_connections` (cause 3).

---

## 5. Investigation log

Fill this in as you go.

| Date | Check | Result | Next step |
|---|---|---|---|
| 2026-10-29 | Global status / process list | Limit reached (152/151) at 2026-10-22 09:21:30; now idle (2 connections); no leak | Check access log and `Uptime` around the peak |
| 2026-10-29 | `Uptime` | 600115 s (≈ 6 d 22 h 42 m) | Run the `started_at` query to get the exact start time and compare it with 09:21:30 on 22 Oct |
| 2026-10-29 | User reports | Error seen on 2026-10-27, but `Max_used_connections_time` shows 22 Oct (first peak only) | Check the access log for 27 Oct as well |
| 2026-10-29 | Top IPs in the access log | One IP: 1,290 requests with user agent `MSIE 7.0; Windows NT 6.0` (fake, so a bot), plus 7 Chrome requests | Look up who owns the IP |
| 2026-10-29 | `whois` on that IP | Cloudflare (`162.158.0.0/15`), so it's a Cloudflare server passing traffic on, not the bot's real address | Check whether the site is behind Cloudflare (`dig`); block in Cloudflare, not by IP on the server (section 3, cause 1) |
| | `Connection_errors_max_connections` | | |
| | Access log at 09:15–09:25 on 22 Oct | | |
| | MariaDB error log / OOM | | |
| | PHP-FPM `max_children` / Apache workers | | |
| | Slow query log (after 1 day) | | |
| | Cron jobs at about 09:20 | | |
