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
| Nr | Titel                     | Beschreibung                 | Aufwand | Verantwortlich | Abhängigkeit | Erweiterung / Ideen          | Erledigt |
| -- | ------------------------- | --------------------------------------------------------------------------------------------------- | ------- | -------------- | ------------ |------------------------------|----------|
| 1  | Projektstruktur & Main.hs | Projekt mit `Main`, `CLI`, `DataHandler` aufsetzen; `main` liest Args und ruft `handleCommand` auf. | 2h      | Eslam          | —            | —                            | ja       |
| 2  | Datentyp `Record`         | Benutzerdefinierter Typ mit Record Syntax (`id`, `name`, `value`) + `deriving (Show, Eq)`           | 2h      | Marco          | Nr 1         | —                            | ja       |
| 3  | Parser für Datei          | JSON-Datei lesen → in `[Record]` umwandeln mit `Data.Aeson.decode` und `map`                        | 2h      | Gary           | Nr 2         | —                            | ja       |
| 4  | CLI-Command-Handler       | `handleCommand :: String -> [String] -> IO ()` für `--insert`, `--delete`, `--filter`, `--query`    | 3h      | Eslam          | Nr 1–3       | —                            | ja       |
| 5  | `--insert`                | Neuen Datensatz in JSON hinzufügen und Datei überschreiben (`encodeFile`)                           | 2h      | Marco          | Nr 4         | Validierung, automatische ID | ja        |
| 6  | `--delete`                | Eintrag nach ID löschen (`filter (\r -> id r /= givenID)`)                                          | 2h      | Marco          | Nr 4         | Mehrfach-Delete, Undo        |          |
| 7  | `--filter`                | Filtern nach Wert (`filter (\r -> value r > x)`)                                                    | 2h      | Gary           | Nr 4         | Regex-Filter, Range-Filter   |          |
| 8  | `--query`                 | Einträge nach Feldinhalt anzeigen (z. B. Name enthält Text)                                         | 2h      | Gary           | Nr 4         | Suche in mehreren Feldern    |          |
| 9  | Output-Option             | Parameter `--out` → Ausgabe in Datei oder stdout (JSON-Format)                                      | 1h      | Eslam          | Nr 4         | Farbausgabe, JSON-Export     |          |
| 10 | Fehlerbehandlung          | Falsche Parameter, leere Dateien, I/O-Fehler mit `case` und Guards                                  | 2h      | Gary           | Nr 4–8       | —                            |          |
| 11 | Tests & Merge             | Unit-Tests, Review, Merge in `main`                                                                 | 3h      | Alle           | alle         | Benchmark, Performance-Test  |          |




### ✅ Definition of Done

* Tool kompiliert ohne Fehler
* Alle Befehle laufen korrekt
* Ausgabe funktioniert (File/Stdout)
* Code in Module geteilt + kommentiert
* Merge nach Review in `main`
