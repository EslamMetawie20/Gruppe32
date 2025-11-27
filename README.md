# 📘 How to Use the Gruppe32 Haskell CLI Tool

> **Was ist dieses Projekt?**
> Ein gemeinsames Teamprojekt für Gruppe 32 (Eslam, Marco, Gary), bei dem ein vollständiges **Haskell-CLI-Tool** entsteht, das JSON-Dateien lesen, verändern und ausgeben kann.

---

## 💻 Nutzung über GHCI

Wenn das Tool **direkt in GHCI** benutzt werden soll, dann so:

### **1) GHCI starten**

Terminal im Projektordner öffnen und eingeben:

```
ghci
```

### **2) Main.hs laden**

```
:l Main.hs
```

### **3) Kurzbefehle direkt ausführen**

```
insert ["data.json", "1", "Max", "200"]
delete ["data.json", "1"]
filterR ["data.json", "50"]
query  ["data.json", "ax"]
out    ["-", "data.json"]
```

Damit das funktioniert, muss in der Main.hs stehen:

---

#  Starten der App

## 💻 Nutzung in GHCI (zum Testen)

```
ghci
:l Main.hs
```

Danach können Befehle direkt ausgeführt werden.

---

#  JSON-Dateiformat

Die JSON-Datei enthält eine Liste von Records:

```json
[
  {
    "id": 1,
    "name": "Max",
    "value": 200.0
  }
]
```

---

# 🛠 Verfügbare CLI-Befehle

## 1️ **Eintrag einfügen – insert**

Fügt einen neuen Datensatz hinzu:

```
insert <Datei> <ID> <Name> <Wert>
```

**Beispiel:**

```
insert ["data.json", "3", "Marco", "150"]
```

---

## 2️ **Eintrag löschen – delete**

Löscht einen Datensatz anhand seiner ID:

```
delete <Datei> <ID>
```

**Beispiel:**

```
delete ["data.json", "3"]
```

---

## 3 **Filtern nach Wert – filter**

Zeigt alle Einträge mit einem Wert größer als X:

```
filter <Datei> <Wert>
```

**Beispiel:**

```
filter ["data.json", "100"]
```

---

## 4 **Suche nach Name – query**

Findet Einträge, deren Name einen bestimmten Text enthält:

```
query <Datei> <Text>
```

**Beispiel:**

```
query ["data.json", "ax"]
```

---

## 5 **Ausgabe – --out**

### Ausgabe auf Konsole:

```
out - <Datei>
```

### Ausgabe in Datei:

```
out [<Zieldatei>, <Quelldatei>]
```

**Beispiel 1:**
→ gibt den JSON-Inhalt auf der Konsole aus
```
out ["output.json", "data.json"]

```
**Beispiel 2:**
→ speichert den JSON-Inhalt in einer neuen Datei
```
out ["_", "data.json"]

```
---

# Nutzung in GHCI (Kurzbefehle)

Wenn Kurzbefehle aktiviert sind:

```
insert ["data.json","1","Max","200"]
delete ["data.json","1"]
query ["data.json","ax"]
filterR ["data.json","50"]
out ["-","data.json"]
```

---

# Fehlerbehandlung

Das Tool prüft automatisch:

* fehlende Parameter
* ungültige Zahlen
* leere Dateien
* ungültige IDs
* doppelte IDs bei --insert

---

# Hinweis

Alle Operationen überschreiben die JSON-Datei sofort. Falls nötig vorher ein Backup anlegen.

---

