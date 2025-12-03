import subprocess
import json
import os
import sys

# Configuration
EXE = ["cabal", "run", "grp32-exe", "--"] # Command to run the CLI
TEST_FILE = "test_data.json"
COPY_FILE = "test_data_copy.json"

def run_cli(args):
    """Runs the CLI with the given arguments and returns stdout."""
    cmd = EXE + args
    try:
        # Use PIPE explicitly for compatibility
        result = subprocess.run(
            cmd, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE, 
            text=True, 
            encoding='utf-8',
            errors='replace' # Handle encoding errors gracefully
        )
        
        out = result.stdout.strip() if result.stdout else ""
        err = result.stderr.strip() if result.stderr else ""
        return out, err, result.returncode
    except Exception as e:
        return "", str(e), -1

def setup():
    """Clean up previous test files."""
    if os.path.exists(TEST_FILE):
        os.remove(TEST_FILE)
    if os.path.exists(COPY_FILE):
        os.remove(COPY_FILE)
    # Create empty JSON array
    with open(TEST_FILE, 'w') as f:
        json.dump([], f)

def test_insert():
    print("Testing Insert...", end=" ")
    # Insert valid record
    out, err, code = run_cli(["--insert", TEST_FILE, "1", "ItemOne", "10.5"])
    if code != 0:
        print(f"FAILED (Exit Code {code})\nStderr: {err}")
        return False
    
    # Verify content
    with open(TEST_FILE, 'r') as f:
        data = json.load(f)
    
    if len(data) != 1 or data[0]['id'] != 1 or data[0]['name'] != "ItemOne":
        print(f"FAILED (Content mismatch): {data}")
        return False

    # Insert duplicate
    out, err, code = run_cli(["--insert", TEST_FILE, "1", "Duplicate", "20.0"])
    if "Fehler" not in out and "Fehler" not in err: # CLI prints errors to stdout usually
        print("FAILED (Duplicate not detected)")
        return False

    print("PASSED")
    return True

def test_stats():
    print("Testing Stats...", end=" ")
    # Add more data
    run_cli(["--insert", TEST_FILE, "2", "ItemTwo", "20.5"])
    run_cli(["--insert", TEST_FILE, "3", "ItemThree", "5.0"])

    out, err, code = run_cli(["--stats", TEST_FILE])
    
    # Expected: 
    # ID 1: 99.9 (Updated)
    # ID 2: 20.5
    # ID 3: 5.0
    # Sum = 125.4
    # Avg = 41.8
    # Min = 5.0
    # Max = 99.9
    
    if "Summe:        125.4" not in out:
        print(f"FAILED (Sum mismatch)\nOutput: {out}")
        return False
    if "Durchschnitt: 41.8" not in out:
        print(f"FAILED (Avg mismatch)\nOutput: {out}")
        return False
        
    print("PASSED")
    return True

def test_filter():
    print("Testing Filter...", end=" ")
    # Filter > 15.0 (Should be UpdatedItem (99.9) and ItemTwo (20.5))
    out, err, code = run_cli(["--filter", TEST_FILE, "15.0"])
    
    try:
        json_str = out.split('\n', 1)[1]
        data = json.loads(json_str)
        if len(data) != 2:
            print(f"FAILED (Filter result incorrect): {data}")
            return False
    except Exception as e:
        print(f"FAILED (Parsing error): {e}\nOutput: {out}")
        return False

    print("PASSED")
    return True

def test_query():
    print("Testing Query...", end=" ")
    # Query "Updated" (Should be UpdatedItem)
    out, err, code = run_cli(["--query", TEST_FILE, "Updated"])
    
    try:
        json_str = out.split('\n', 1)[1]
        data = json.loads(json_str)
        if len(data) != 1 or data[0]['name'] != "UpdatedItem":
            print(f"FAILED (Query result incorrect): {data}")
            return False
    except Exception as e:
        print(f"FAILED (Parsing error): {e}\nOutput: {out}")
        return False

    print("PASSED")
    return True

def test_delete():
    print("Testing Delete...", end=" ")
    # Delete ID 2
    run_cli(["--delete", TEST_FILE, "2"])
    
    with open(TEST_FILE, 'r') as f:
        data = json.load(f)
    
    ids = [r['id'] for r in data]
    if 2 in ids:
        print("FAILED (ID 2 still present)")
        return False
    if 1 not in ids or 3 not in ids:
        print("FAILED (Other IDs missing)")
        return False

    print("PASSED")
    return True

def test_save():
    print("Testing Save (Persistence)...", end=" ")
    # Save to new file
    run_cli(["--save", TEST_FILE, COPY_FILE])
    
    if not os.path.exists(COPY_FILE):
        print("FAILED (Copy file not created)")
        return False
        
    with open(COPY_FILE, 'r') as f:
        data = json.load(f)
        
    if len(data) != 2: # Should have ID 1 and 3
        print(f"FAILED (Copy content incorrect): {len(data)} records")
        return False

    print("PASSED")
    return True

def test_update():
    print("Testing Update...", end=" ")
    # Update ItemOne (ID 1) to "UpdatedItem" with value 99.9
    out, err, code = run_cli(["--update", TEST_FILE, "1", "UpdatedItem", "99.9"])
    if code != 0:
        print(f"FAILED (Exit Code {code})\nStderr: {err}")
        return False

    # Verify content
    with open(TEST_FILE, 'r') as f:
        data = json.load(f)
    
    # Find record with ID 1
    rec = next((r for r in data if r['id'] == 1), None)
    if not rec:
        print("FAILED (Record ID 1 not found)")
        return False
        
    if rec['name'] != "UpdatedItem" or rec['value'] != 99.9:
        print(f"FAILED (Content mismatch): {rec}")
        return False

    print("PASSED")
    return True

def main():
    print("=== Haskell JSON Manager Test Suite ===")
    setup()
    
    tests = [
        test_insert,
        test_update,
        test_stats,
        test_filter,
        test_query,
        test_delete,
        test_save
    ]
    
    passed = 0
    for t in tests:
        if t():
            passed += 1
            
    print("-" * 30)
    print(f"Tests: {passed}/{len(tests)} passed")
    
    if passed == len(tests):
        print("✅ ALL TESTS PASSED")
        sys.exit(0)
    else:
        print("❌ SOME TESTS FAILED")
        sys.exit(1)

if __name__ == "__main__":
    main()
