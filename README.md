# Gruppe32 - Haskell JSON Manager

> **Teamprojekt von Gruppe 32** (Eslam, Marco, Gary)  
> Ein **Haskell-CLI-Tool** zum Verwalten von JSON-Dateien.

---

## Schnellstart

### Projekt bauen und starten

```bash
# Projekt complen
cabal build

# Direkt ausführen (CLI-Modus)
cabal run grp32-exe -- --help

# Interaktive Shell starten
cabal run grp32-exe
```

---

## Zwei Nutzungsmodi

### 1. **CLI-Modus** (Einzelbefehle)
Für schnelle Operationen direkt aus dem Terminal:

```bash
cabal run grp32-exe -- --insert data.json 1 Max 200
cabal run grp32-exe -- --stats data.json
```

### 2. **Shell-Modus** (Interaktiv)
Für mehrere Operationen hintereinander - Änderungen bleiben im Speicher:

```bash
cabal run grp32-exe
> data.json
JSON-Shell> insert 1 Max 200
JSON-Shell> stats
JSON-Shell> save
JSON-Shell> quit
```

---

## JSON-Dateiformat

```json
[
  { "id": 1, "name": "Max", "value": 200.0 },
  { "id": 2, "name": "Anna", "value": 150.5 }
]
```

---

## Verfügbare Befehle

### **insert** - Eintrag einfügen

```bash
# CLI
cabal run grp32-exe -- --insert data.json 3 Marco 150

# Shell
insert 3 Marco 150
```

### **update** - Eintrag aktualisieren

```bash
# CLI
cabal run grp32-exe -- --update data.json 3 Marco 175

# Shell
update 3 Marco 175
```

### **delete** - Eintrag löschen

```bash
# CLI
cabal run grp32-exe -- --delete data.json 3

# Shell
delete 3
```

### **filter** - Nach Wert filtern

Zeigt alle Einträge mit `value > X`:

```bash
# CLI
cabal run grp32-exe -- --filter data.json 100

# Shell
filter 100
```

### **query** - Nach Name suchen

Findet Einträge, deren Name den Text enthält (case-insensitive):

```bash
# CLI
cabal run grp32-exe -- --query data.json ax

# Shell
query ax
```

### **stats** - Statistiken anzeigen

Berechnet Summe, Durchschnitt, Minimum und Maximum:

```bash
# CLI
cabal run grp32-exe -- --stats data.json

# Shell
stats
```

**Ausgabe:**
```
Statistik:
  Anzahl:       3
  Summe:        525.5
  Durchschnitt: 175.17
  Min:          150.0
  Max:          200.0
```

### **print / list** - Alle Einträge anzeigen

```bash
# CLI
cabal run grp32-exe -- --print data.json

# Shell
list
```

### **save** - Speichern

```bash
# CLI (in neue Datei)
cabal run grp32-exe -- --save data.json backup.json

# Shell
save              # In aktuelle Datei
save backup.json  # In neue Datei
```

### **help** - Hilfe anzeigen

```bash
cabal run grp32-exe -- --help
```

---

## Nutzung in GHCI

```bash
cabal repl
```

```haskell
-- Module laden (automatisch)
-- Befehle ausführen:
insert ["data.json", "1", "Max", "200"]
delete ["data.json", "1"]
filterRecords ["data.json", "50"]
query ["data.json", "ax"]
stats ["data.json"]
printRecords ["data.json"]
help
```

---

## Fehlerbehandlung

Das Tool prüft automatisch:

- Fehlende Parameter
- Ungültige Zahlen (ID muss Int, Wert muss Double sein)
- Doppelte IDs bei `insert`
- Nicht existierende IDs bei `update`/`delete`
- Fehlende oder ungültige JSON-Dateien
- Automatische Backups vor dem Speichern

---

### Projekt-Struktur

```
grp32/
├── grp32.cabal        # Projekt-Konfiguration
├── src/
│   ├── Main.hs        # Einstiegspunkt
│   ├── CLI.hs         # Parsing & Befehle
│   ├── Shell.hs       # Interaktive Shell
│   ├── DataHandler.hs # Datei-IO
│   └── Types.hs       # Datentypen
├── docs/
│   └── Entwickler_Handbuch.md
└── tests/
    └── test_suite.py
```

### Build-Befehle

```bash
cabal build      # Kompilieren
cabal run        # Ausführen
cabal repl       # GHCI mit Projekt
cabal clean      # Build-Artefakte löschen
```

---

## Hinweis

- **CLI-Modus**: Änderungen werden sofort gespeichert
- **Shell-Modus**: Änderungen bleiben im Speicher bis `save`
- Vor dem Speichern wird automatisch ein Backup erstellt (`.bak.json`)
