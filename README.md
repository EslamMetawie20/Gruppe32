## Gruppe32 ( Haskell CLI Tool)

**Ziel:**
Ein voll funktionsfähiges Haskell-CLI-Tool zur **Manipulation von JSON-Dateien** (`--insert`, `--delete`, `--filter`, `--query`, ………) mit Ausgabe in Datei oder stdout.

Falls jemand zusätzliche CLI-Befehle oder Funktionen vorschlagen möchte (außer --insert, --delete, --filter, --query), bitte hier notieren:
..............................................................

**Dauer:** 4 Woche
**Deadline:** 01. Dezember 2025
**Team:** Eslam | Marco | Gary

---

### Backlog

| Nr | Titel                     | Beschreibung                                                                                        | Aufwand | Verantwortlich | Abhängigkeit  | Erweiterung / Ideen                                          | Erledigt |
| -- | ------------------------- | --------------------------------------------------------------------------------------------------- | ------- |----------------|---------------|--------------------------------------------------------------| -------- |
| 1  | Projektstruktur & Main.hs | Projekt mit `Main`, `CLI`, `DataHandler` aufsetzen; `main` liest Args und ruft `handleCommand` auf. | 2h      | **Eslam**      | —             | —                                                            | ja       |
| 2  | Datentyp `Record`         | Benutzerdefinierter Typ mit Record Syntax (`id`, `name`, `value`) + `deriving (Show, Eq)`           | 2h      | **Marco**      | Nr 1          | —                                                            | ja       |
| 3  | Parser für Datei          | JSON-Datei lesen → in `[Record]` umwandeln mit `Data.Aeson.decode` und `map`                        | 2h      | **Gary**       | Nr 2          | —                                                            | ja       |
| 4  | CLI-Command-Handler       | `handleCommand :: String -> [String] -> IO ()` für `--insert`, `--delete`, `--filter`, `--query`    | 3h      | **Eslam**      | Nr 1–3        | —                                                            | teils    |
| 5  | `--insert`                | Neuen Datensatz in JSON hinzufügen und Datei überschreiben (`encodeFile`)                           | 2h      | **Marco**      | Nr 4          | Ideen zu Validierung oder automatischer ID hier eintragen ⬇️ | ja       |
| 6  | `--delete`                | Eintrag nach ID löschen (`filter (\r -> id r /= givenID)`)                                          | 2h      | **Marco**      | Nr 4          | Ideen für Mehrfach-Delete oder Undo-Funktion ⬇️              | ja       |
| 7  | `--filter`                | Filtern nach Wert (`filter (\r -> value r > x)`)                                                    | 2h      | **Gary**       | Nr 4          | Ideen für regex-Filter oder Range-Filter ⬇️                  | ja       |
| 8  | `--query`                 | Einträge nach Feldinhalt anzeigen (z. B. Name enthält Text)                                         | 2h      | **Gary**       | Nr 4          | Suche nach mehreren Feldern; Case-insensitive (Done)         | ja       |
| 9  | Output-Option             | Parameter `--out` → Ausgabe in Datei oder stdout (JSON-Format)                                      | 1h      | **Eslam**      | Nr 4          | Ideen für farbige Konsole oder JSON-Export ⬇️                |          |
| 10 | Fehlerbehandlung          | Falsche Parameter, leere Dateien, I/O-Fehler mit `case` und Guards                                  | 2h      | **Gary**       | Nr 4–8        | —                                                            |          |
| 11 | ID-Duplikate verhindern | Beim Insert prüfen, ob die ID bereits existiert. Falls ja → kein Insert, Fehlermeldung ausgeben. | 1h      | wer ??         | Nr 5          | Option für automatische ID-Vergabe                           |          |
| 12 | Tests & Merge             | Unit-Tests, Review, Merge in `main`                                                                 | 3h      | **Alle**       | alle          | Ideen für Benchmark oder Performance-Test ⬇️                 |          |


### ✅ Definition of Done

* Tool kompiliert ohne Fehler
* Alle Befehle laufen korrekt
* Ausgabe funktioniert (File/Stdout)
* Code in Module geteilt + kommentiert
* Merge nach Review in `main`
