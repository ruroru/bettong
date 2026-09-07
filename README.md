# bettong

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.jj/bettong.svg)](https://clojars.org/org.clojars.jj/bettong)

An agent library for Clojure. You give an agent capabilities; it uses them to do
what you ask.

An agent starts with nothing. Every tool comes from a capability you pass in, and
a capability grants only reading unless you ask for more.

```clojure
[org.clojars.jj/bettong "0.1.0-SNAPSHOT"]
```

```clojure
(require '[jj.bettong.agent :as agent]
         '[jj.bettong.capabilities :as caps])

(def assistant
  (agent/deepseek {:api-key (System/getenv "DEEPSEEK_API_KEY")
                   :capabilities [(caps/filesystem {:root "/srv/data" :writable? true})]}))

(agent/run assistant "Summarise report.txt into summary.md")
;; => {:output "Wrote summary.md, 4 bullet points."
;;     :steps 3
;;     :tool-calls [{:tool "read_file" :capability :filesystem :args {...} :result "..."} ...]
;;     :messages [...]
;;     :usage {:requests 3 :prompt-tokens 3042 :completion-tokens 305 :total-tokens 3347
;;             :cache-hit-tokens 2688 :cache-miss-tokens 354 :reasoning-tokens 0}
;;     :done? true}
```

## Agents

`agent/deepseek` builds one; `agent/run` runs a prompt on it.

```clojure
(agent/run assistant "Summarise report.txt")
(agent/run assistant "Again in French" {:max-steps 3})   ; per-call override
```

| option | default | |
|---|---|---|
| `:capabilities` | `[]` | what it may do. Empty means no tools |
| `:api-key` | `$DEEPSEEK_API_KEY` | |
| `:model` | `deepseek-v4-flash` | |
| `:base-url` | `https://api.deepseek.com` | any OpenAI-compatible endpoint |
| `:max-steps` | `10` | model calls before giving up; `:done?` is `false` if hit |
| `:system` | `agent/base-system-prompt` | capability instructions are appended to it |
| `:approve-fn` | allow all | see [Approvals](#approvals) |
| `:on-event` | no-op | `{:event :assistant/:tool-call ...}` |
| `:temperature` | — | |
| `:chat-fn` | `llm/chat` | replace the model call, for tests |
| `:messages` | `[]` | transcript to continue from; what sessions use |

An unknown option throws rather than being ignored. `Agent` is a protocol, so you
can supply your own implementation.

## Conversations

`run` is one-shot. A session remembers, and each turn extends a prefix the
provider has already cached.

```clojure
(def chat (agent/session assistant))

(agent/run chat "Describe Lithuania using wikipedia")
(agent/run chat "What is its capital?")     ; knows what "its" means

(agent/transcript chat)      ; every message
(agent/session-usage chat)   ; cost of the whole conversation
(agent/clear! chat)          ; forget it, keep the agent
```

A session is an `Agent`, and the agent underneath stays stateless — two sessions
on one agent are two conversations.

## Capabilities

Each has its own namespace — `jj.bettong.capability.filesystem` and friends — and
`jj.bettong.capabilities` re-exports all of them for a single require. None is on
unless you pass it.

```clojure
(require '[jj.bettong.capabilities :as caps])            ; all of them
(require '[jj.bettong.capability.filesystem :as fs])     ; or just one
```

### `(caps/filesystem opts)`

`read_file`, `list_dir`, and with `:writable?` also `write_file`, `delete_file`.

| option | default | |
|---|---|---|
| `:root` | `nil` | confine every path here; `../` and symlinks cannot escape |
| `:writable?` | `false` | add the write tools |
| `:allow-secrets?` | `false` | permit credential files |

### `(caps/shell opts)`

`run_shell` — bash, combined stdout/stderr, exit code returned as data. Stdin is
closed. A non-zero exit is a result, not an error.

| option | default | |
|---|---|---|
| `:root` | `nil` | where a command may start — **not** what it can reach |
| `:timeout-ms` | `120000` | kill the command after this |
| `:max-output` | `20000` | characters kept, truncated head-and-tail |
| `:strip-secrets?` | `true` | hide credential env vars from the command |
| `:deny` | `[]` | extra `[regex reason]` refusals |
| `:deny-defaults?` | `true` | refuse catastrophic commands |
| `:allow` | `nil` | regexes; when set, only matching commands run |

### `(caps/edn opts)`

`read_edn`, and with `:writable?` also `set_edn`, `delete_edn`. Edits are spliced
into the file's text, so comments, blank lines and key order survive; every edit
is re-read and compared with the data it should hold, or it is not written.

```clojure
(require '[jj.bettong.edn :as edn])
(edn/get-in-file    "config.edn" [:server :port])       ;; => 8080
(edn/set-in-file!   "config.edn" [:server :port] 443)
(edn/update-in-file! "config.edn" [:db :pool-size] inc)
(edn/delete-in-file! "config.edn" [:debug])
```

Options: `:root`, `:writable?`. Key paths from the model are arrays read as EDN:
`[":server", ":port"]`.

### Search

Each source is its own capability with its own tool name, so an agent can hold
several and choose between them — and the descriptions tell it which is which.

```clojure
(agent/deepseek {:capabilities [(caps/wikipedia)      ; wikipedia_search
                                (caps/duckduckgo)]})  ; duckduckgo_search
```

| capability | tool | credentials |
|---|---|---|
| `caps/duckduckgo` | `duckduckgo_search` | none; scrapes the no-JS endpoint, which sometimes answers a scraper with a challenge |
| `caps/wikipedia` | `wikipedia_search` | none; `:language` picks the edition, and the article urls follow it |
| `caps/google` | `google_search` | `GOOGLE_API_KEY` + `:cx`/`GOOGLE_CSE_ID`; capped at 10 results |
| `caps/tavily` | `tavily_search` | `TAVILY_API_KEY` |
| `caps/brave` | `brave_search` | `BRAVE_API_KEY` |
| `caps/serper` | `serper_search` | `SERPER_API_KEY`; Google results, no Google Cloud project |

All of them take `:max-results` (5), `:snippet-length` (300), `:timeout-ms`,
`:base-url`, `:instructions` and `:description`.

`caps/search` is the builder they are all made of, and takes your own backend —
a function of `{:keys [query max-results]}` returning `[{:title :url :snippet}]`:

```clojure
(caps/search {:id :runbooks
              :tool-name "runbook_search"                     ; unique across capabilities
              :backend (fn [{:keys [query]}] (my-index/find query))
              :instructions "runbook_search is the only source of truth for deploys."})
```

Say what a source covers in `:instructions` when it is not the open web, or the
model answers from memory instead of searching it.

## Writing your own

A capability is anything satisfying `jj.bettong.capability/Capability`. A map is
one:

```clojure
{:id :weather
 :instructions "Temperatures are in celsius."          ; optional
 :tools [{:name "get_weather"
          :description "Current weather for a city."
          :parameters {:city {:type "string" :description "City name." :required true}}
          :handler (fn [{:keys [city]}] (str "17C in " city))}]}
```

`:handler` takes the decoded arguments and returns anything printable. A handler
that throws is reported to the model as `ERROR: …`, which it can read and work
around. `:parameters` takes the shorthand above or a full JSON Schema object.

Use `defrecord` or `reify` when a capability owns state or a connection.
Capabilities compose; two offering the same tool name is an error at startup.

Write array examples in descriptions as valid JSON — models copy the syntax they
are shown.

## Defaults

Everything a tool returns is uploaded to the model provider. So:

| | |
|---|---|
| credentials stripped from the shell's environment | `KEY`, `TOKEN`, `SECRET`, `PASSWORD`, `CREDENTIAL`, `AUTH`, `PRIVATE` |
| tool output redacted | credential values become `[redacted]`, including from your own capabilities |
| credential files refused | `.ssh/`, `.aws/credentials`, `.env`, `*.pem`, `*.key`, `.netrc`, `.npmrc`, `.git-credentials`, `id_rsa*`, `.docker/config.json`, `.kube/config`, `.gnupg/` |
| catastrophic commands refused | `rm -rf /`, `mkfs`, `dd of=/dev/…`, `shutdown`, fork bombs, `curl \| sh`, `chmod 777 /` |

Overrides: `:allow-secrets?`, `:strip-secrets?`, `:deny-defaults?`,
`(binding [cap/*redact-secrets?* false] …)`.

The command policy is a seatbelt, not a sandbox. It refuses the catastrophic
mistake an agent actually makes; it will not stop a determined one, and it
sometimes refuses something innocent that mentions a pattern (`echo "rm -rf /"`).
For a real boundary use `:allow`, or a rooted filesystem and no shell.

## Approvals

`:approve-fn` runs before every tool call. Truthy allows — so a set works —
`false`, `nil` or `{:allowed? false :reason "..."}` denies, and the reason goes to
the model.

```clojure
(agent/deepseek {:capabilities [(caps/shell)] :approve-fn agent/ask-in-terminal})

(agent/deepseek {:capabilities [(caps/filesystem) (caps/shell)]
                 :approve-fn (fn [{:keys [tool args capability]}]
                               (or (= :filesystem capability)
                                   {:allowed? false :reason "shell is off today"}))})
```

A hook that throws denies rather than killing the run.

## Cost

```clojure
(usage/summary (:usage result))
;; => "3 requests 3347 tokens (3042 in, 305 out) [2688 cached, 88% of input]"

(usage/cost (:usage result) {:input 0.22 :output 0.66 :cache-hit 0.007})
```

Prices change, so bettong holds none — pass your provider's, per million tokens.
Cached prompt tokens are billed at `:cache-hit` when given.

Two things protect the cache: tools are sent in declaration order, so adding a
capability appends rather than reshuffles; and anything varying per run in a
system prompt or `:instructions` invalidates everything after it.

## Layout

```
jj.bettong.agent          the Agent protocol, deepseek, run, sessions
jj.bettong.capability     the Capability protocol and the tool registry
jj.bettong.capabilities   every built-in capability, re-exported
jj.bettong.capability.*   filesystem, shell, edn, and one per search source
jj.bettong.edn            reading and editing values in EDN files
jj.bettong.search         the backend functions the search capabilities use
jj.bettong.llm            the provider call
jj.bettong.secrets        credential stripping and redaction
jj.bettong.usage          token accounting and cost
jj.bettong.cli            lein run entry point
jj.bettong.impl.*         internals: files, shell, edn-text, json, options
```

## Dependencies

`org.clojure/clojure`, [`org.clojars.jj/potoroo`](https://clojars.org/org.clojars.jj/potoroo)
(HTTP over `java.net.http`), `org.clojure/data.json`. Three jars, no Jackson, no
Apache HttpComponents.

## Tests

```bash
lein test                                        # 256 unit tests, no network
DEEPSEEK_API_KEY=sk-... lein test :integration   # 24 tests, real API and web
```

Unit tests use either a scripted `:chat-fn` or a real HTTP server on
[ring-http-exchange](https://clojars.org/org.clojars.jj/ring-http-exchange)
standing in for the model and the search APIs — so request building, headers,
error mapping and prompt-prefix stability are all checked without a network.
